#!/usr/bin/env python3
"""
Recursively show how much code each git repository touched since a date,
rendered in the same `git diff --stat` style (per-file +/- bars), across the
current and nested directories.

Counts churn: additions and deletions summed over every commit on the current
branch since the date (a line added then later removed counts in both). This
answers "how much code was touched", not the net diff.

Read-only: it never fetches, checks out, or modifies any repository.

Usage:
    git rhist-stat                    # since yesterday
    git rhist-stat --since 2026-06-07 # since a given date
    git rhist-stat --since "2 weeks ago" path/to/dir

Requirements:
- Python 3.7+
- Nerd Fonts for icons (optional)
"""

import argparse
import logging
import os
import subprocess
import sys
import textwrap
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# Color codes for status icons
BLUE = "\033[34m"
GRAY = "\033[90m"
GREEN = "\033[32m"
ORANGE = "\033[91m"
RED = "\033[31m"
YELLOW = "\033[33m"
RESET = "\033[0m"
BOLD = "\033[1m"
DIM = "\033[2m"

# Max width of the +/- bar graph column, matching `git --stat` defaults.
GRAPH_WIDTH = 60

# Nerd Font status icons for the scanned-repo list.
ICON_OK = " "  # check-circle: no changes
ICON_CHANGED = " "  # refresh arrows: files changed
ICON_ERROR = " "  # times-circle: error

logger = logging.getLogger(__name__)


@dataclass
class FileStat:
    path: str
    adds: int
    dels: int
    binary: bool = False

    @property
    def total(self) -> int:
        return self.adds + self.dels


@dataclass
class RepoResult:
    path: str
    branch: str
    status: str  # 'changes', 'no_changes', 'error'
    remote: Optional[str] = None
    files: List[FileStat] = field(default_factory=list)
    error_message: Optional[str] = None

    @property
    def adds(self) -> int:
        return sum(f.adds for f in self.files)

    @property
    def dels(self) -> int:
        return sum(f.dels for f in self.files)


class GitRepoStat:
    def __init__(self, since: str):
        self.since = since
        self.results: List[RepoResult] = []

    def find_git_repos(self, root_path: Path) -> List[Path]:
        """Find all git repositories recursively with depth limit and submodule handling."""
        repos = []
        max_depth = 5

        def _find_repos_recursive(
            current_path: Path, current_depth: int, parent_submodules: set
        ) -> None:
            if current_depth > max_depth:
                return
            logger.debug(f"Checking directory at depth {current_depth}: {current_path}")

            try:
                # Check if current path is a git repository
                git_dir = current_path / ".git"
                if git_dir.exists() and git_dir.is_dir():
                    # Skip if this is a submodule of a parent repository
                    relative_path = str(current_path.relative_to(root_path))
                    if relative_path not in parent_submodules:
                        repos.append(current_path)
                        logger.debug(f"Found git repo: {current_path}")
                    else:
                        logger.debug(f"Skipping submodule: {current_path}")

                # Get submodules in current directory if it's a git repo
                current_submodules = parent_submodules.copy()
                gitmodules_file = current_path / ".gitmodules"
                if gitmodules_file.exists():
                    submodules = self._parse_submodules(
                        gitmodules_file, current_path, root_path
                    )
                    current_submodules.update(submodules)
                    if submodules:
                        logger.debug(
                            f"Found submodules in {current_path}: {submodules}"
                        )

                # Recurse into subdirectories
                for item in current_path.iterdir():
                    if (
                        item.is_dir()
                        and not item.name.startswith(".")
                        and item.name != "node_modules"
                    ):
                        _find_repos_recursive(
                            item, current_depth + 1, current_submodules
                        )

            except (PermissionError, OSError):
                # Skip directories we can't access
                pass

        _find_repos_recursive(root_path, 0, set())
        return sorted(repos)

    def _parse_submodules(
        self, gitmodules_file: Path, repo_root: Path, root_path: Path
    ) -> set:
        """Parse .gitmodules file and return set of submodule paths relative to root_path."""
        submodules = set()
        try:
            with open(gitmodules_file, "r") as f:
                for line in f:
                    line = line.strip()
                    if line.startswith("path = "):
                        submodule_path = line[7:].strip()
                        # Convert to absolute path then back to relative from root_path
                        abs_submodule_path = (repo_root / submodule_path).resolve()
                        try:
                            relative_to_root = str(
                                abs_submodule_path.relative_to(root_path.resolve())
                            )
                            submodules.add(relative_to_root)
                        except ValueError:
                            # Submodule is outside root_path, skip it
                            logger.debug(
                                f"Submodule outside root_path: {abs_submodule_path}"
                            )
        except (IOError, OSError) as e:
            logger.debug(f"Error reading .gitmodules file {gitmodules_file}: {e}")
        return submodules

    def run_git_command(
        self, repo_path: Path, command: List[str]
    ) -> Tuple[bool, str, str]:
        """Run a git command in the specified repository."""
        # Disable any interactive credential prompting so a single repo that
        # needs auth cannot grab the shared TTY and block the parallel workers.
        env = os.environ.copy()
        env["GIT_TERMINAL_PROMPT"] = "0"
        env.pop("GIT_ASKPASS", None)
        env.pop("SSH_ASKPASS", None)
        try:
            result = subprocess.run(
                ["git"] + command,
                cwd=repo_path,
                capture_output=True,
                text=True,
                timeout=30,
                stdin=subprocess.DEVNULL,
                env=env,
            )
            return result.returncode == 0, result.stdout.strip(), result.stderr.strip()
        except subprocess.TimeoutExpired:
            return False, "", "Command timed out"
        except Exception as e:
            return False, "", str(e)

    def get_current_branch(self, repo_path: Path) -> Optional[str]:
        """Get the current branch name (empty string if detached HEAD)."""
        success, stdout, _ = self.run_git_command(
            repo_path, ["branch", "--show-current"]
        )
        return stdout if success else None

    def get_remote(self, repo_path: Path) -> Optional[str]:
        """Return a short owner/repo slug for the origin remote, if any."""
        success, stdout, _ = self.run_git_command(
            repo_path, ["remote", "get-url", "origin"]
        )
        if not success or not stdout:
            return None
        return self._shorten_remote(stdout)

    @staticmethod
    def _shorten_remote(url: str) -> str:
        """Reduce a remote URL to an `owner/repo` slug."""
        slug = url.strip()
        if slug.endswith(".git"):
            slug = slug[:-4]
        # git@host:owner/repo  ->  owner/repo
        if "@" in slug and ":" in slug.split("@", 1)[1]:
            slug = slug.split(":", 1)[1]
        # scheme://host/owner/repo  ->  owner/repo
        elif "://" in slug:
            slug = slug.split("://", 1)[1].split("/", 1)[-1]
        return slug

    def get_stats_since(self, repo_path: Path) -> Tuple[bool, List[FileStat], str]:
        """Aggregate per-file additions/deletions since the configured date."""
        success, stdout, stderr = self.run_git_command(
            repo_path,
            [
                "log",
                f"--since={self.since}",
                "--numstat",
                "-M",
                "--format=",
            ],
        )

        if not success:
            return False, [], stderr

        stats: Dict[str, FileStat] = {}
        for line in stdout.split("\n"):
            line = line.strip()
            if not line:
                continue
            parts = line.split("\t")
            if len(parts) < 3:
                continue
            adds_raw, dels_raw, path = parts[0], parts[1], "\t".join(parts[2:])
            binary = adds_raw == "-" or dels_raw == "-"
            adds = 0 if binary else int(adds_raw)
            dels = 0 if binary else int(dels_raw)

            entry = stats.get(path)
            if entry is None:
                stats[path] = FileStat(path=path, adds=adds, dels=dels, binary=binary)
            else:
                entry.adds += adds
                entry.dels += dels
                entry.binary = entry.binary and binary

        # Largest churn first, like a glanceable activity report.
        files = sorted(stats.values(), key=lambda f: (f.total, f.path), reverse=True)
        return True, files, ""

    def inspect_repo(self, repo_path: Path) -> RepoResult:
        """Collect per-file churn for a single repository."""
        rel = str(repo_path.relative_to(Path.cwd()))
        # The cwd repo shows up as "."; use its real directory name instead.
        repo_name = repo_path.name if rel == "." else rel
        remote = self.get_remote(repo_path)

        current_branch = self.get_current_branch(repo_path)
        if current_branch is None:
            return RepoResult(
                path=repo_name,
                branch="unknown",
                status="error",
                remote=remote,
                error_message="Could not determine current branch",
            )
        # Detached HEAD reports an empty branch name.
        branch = current_branch or "(detached)"

        success, files, stderr = self.get_stats_since(repo_path)
        if not success:
            return RepoResult(
                path=repo_name,
                branch=branch,
                status="error",
                remote=remote,
                error_message=f"Failed to read log: {stderr}",
            )

        return RepoResult(
            path=repo_name,
            branch=branch,
            status="changes" if files else "no_changes",
            remote=remote,
            files=files,
        )

    def _print_result(self, result: RepoResult):
        """Print immediate feedback for a repository result."""
        if result.status == "changes":
            n = len(result.files)
            print(
                f"{YELLOW}{ICON_CHANGED}{RESET} {result.path} {GRAY}({n} files changed){RESET}"
            )
        elif result.status == "no_changes":
            print(f"{GREEN}{ICON_OK}{RESET} {result.path} {GRAY}(no changes){RESET}")
        elif result.status == "error":
            print(f"{RED}{ICON_ERROR}{RESET} {result.path} {GRAY}(error){RESET}")

    def inspect_all_repos(
        self, root_path: Path = Path.cwd(), max_workers: Optional[int] = None
    ):
        """Inspect all repositories in the given path concurrently."""

        repos = self.find_git_repos(root_path)

        if not repos:
            print("No git repositories found.")
            return

        # Use as many workers as repos by default
        workers = max_workers if max_workers else len(repos)
        print(
            f"Showing code touched since {BOLD}{self.since}{RESET} "
            f"in {len(repos)} git repositories...\n"
        )

        with ThreadPoolExecutor(max_workers=workers) as executor:
            future_to_repo = {
                executor.submit(self.inspect_repo, repo): repo for repo in repos
            }

            for future in as_completed(future_to_repo):
                result = future.result()
                self.results.append(result)
                self._print_result(result)

    def _render_bars(self, file: FileStat, scale: float) -> str:
        """Render the colored +/- bar graph for a single file, git-stat style."""
        if file.binary:
            return f"{GRAY}Bin{RESET}"
        if file.total == 0:
            return ""

        if scale >= 1.0:
            bar_len = file.total
        else:
            bar_len = max(1, round(file.total * scale))

        plus = round(file.adds * bar_len / file.total) if file.total else 0
        plus = min(plus, bar_len)
        minus = bar_len - plus
        return f"{GREEN}{'+' * plus}{RESET}{RED}{'-' * minus}{RESET}"

    def _repo_title(self, repo: RepoResult) -> str:
        """A clear per-repo header: name, remote slug, branch."""
        remote = f" {GRAY}({repo.remote}){RESET}" if repo.remote else ""
        return f"\n{BOLD}{YELLOW} {repo.path}{RESET}{remote} {DIM}{repo.branch}{RESET}"

    def _print_repo_stat(self, repo: RepoResult):
        """Print one repository's per-file stat block."""
        num_w = max(len(str(f.total)) for f in repo.files)
        max_total = max(f.total for f in repo.files)
        scale = 1.0 if max_total <= GRAPH_WIDTH else GRAPH_WIDTH / max_total

        print(self._repo_title(repo))
        for f in repo.files:
            bars = self._render_bars(f, scale)
            count = "Bin" if f.binary else str(f.total)
            print(f"   {f.path} {GRAY}|{RESET} {count:>{num_w}} {bars}")

        adds, dels = repo.adds, repo.dels
        print(
            f"   {GRAY}{len(repo.files)} files, "
            f"{GREEN}{adds} insertions(+){RESET}{GRAY}, "
            f"{RED}{dels} deletions(-){RESET}"
        )

    def print_summary(self):
        """Print a detailed summary of all operations."""

        changed_repos = sorted(
            (r for r in self.results if r.status == "changes"),
            key=lambda r: r.adds + r.dels,
            reverse=True,
        )
        no_change_repos = [r for r in self.results if r.status == "no_changes"]
        error_repos = [r for r in self.results if r.status == "error"]

        if changed_repos:
            print(
                f"\n{YELLOW}  REPOSITORIES WITH CHANGES{RESET} ({len(changed_repos)})"
            )
            for repo in changed_repos:
                self._print_repo_stat(repo)

        if error_repos:
            print(f"\n{RED}  REPOSITORIES WITH ERRORS ({len(error_repos)}){RESET}")
            for repo in error_repos:
                err_msg = (
                    textwrap.indent(repo.error_message, "    ")
                    if repo.error_message
                    else "Unknown error"
                )
                print(f"\n{RED} {repo.path} ({repo.branch}) {RESET}")
                print(f"{err_msg}")

        total_adds = sum(r.adds for r in changed_repos)
        total_dels = sum(r.dels for r in changed_repos)

        print(f"\n{BLUE}󰈙  CODE TOUCHED SUMMARY{RESET}\n")
        print(f"Since: \t\t\t{self.since}")
        print(f"Total repositories: \t{len(self.results)}")
        print(f"With changes: \t\t{len(changed_repos)}")
        print(f"No changes: \t\t{len(no_change_repos)}")
        print(f"Errors: \t\t{len(error_repos)}")
        print(f"Insertions: \t\t{total_adds}")
        print(f"Deletions: \t\t{total_dels}")
        print(f"Total touched: \t\t{total_adds + total_dels}\n")


def main():
    parser = argparse.ArgumentParser(
        description="Recursively show how much code each git repository touched "
        "since a date, in `git --stat` style, across nested directories"
    )
    parser.add_argument(
        "--since",
        default="yesterday",
        help="Count changes more recent than this date (default: yesterday). "
        "Accepts any git-parseable date, e.g. 2026-06-07 or '2 weeks ago'.",
    )
    parser.add_argument(
        "path",
        nargs="?",
        default=".",
        help="Root path to search for repositories (default: current directory)",
    )

    args = parser.parse_args()
    logging.basicConfig(
        level=logging.DEBUG if os.environ.get("DEBUG") else logging.INFO,
        format="%(levelname)s: %(message)s",
    )

    root_path = Path(args.path).resolve()
    if not root_path.exists():
        print(f"Error: Path '{root_path}' does not exist")
        sys.exit(1)

    stat = GitRepoStat(since=args.since)

    try:
        stat.inspect_all_repos(root_path, max_workers=50)
        stat.print_summary()
    except KeyboardInterrupt:
        print(f"\n\n{YELLOW}  Operation cancelled by user{RESET}")
        if stat.results:
            stat.print_summary()
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
