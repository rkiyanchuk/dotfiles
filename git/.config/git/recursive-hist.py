#!/usr/bin/env python3
"""
Recursively show recent commits across all git repositories in current and
nested directories, in the same reporting style as recursive-sync.py (rpull).

Read-only: it never fetches, checks out, or modifies any repository.

Usage:
    git rhist                    # commits since yesterday
    git rhist --since 2026-06-07 # commits since a given date
    git rhist --since "2 weeks ago" path/to/dir

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
from typing import List, Optional, Tuple


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

logger = logging.getLogger(__name__)


@dataclass
class CommitInfo:
    hash: str
    title: str
    author: str
    date: str


@dataclass
class RepoResult:
    path: str
    branch: str
    status: str  # 'changes', 'no_changes', 'error'
    commits: List[CommitInfo] = field(default_factory=list)
    error_message: Optional[str] = None


class GitRepoHistory:
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

    def get_commits_since(self, repo_path: Path) -> Tuple[bool, List[CommitInfo], str]:
        """Get commits on the current branch since the configured date."""
        success, stdout, stderr = self.run_git_command(
            repo_path,
            [
                "log",
                f"--since={self.since}",
                "--pretty=format:%H|%s|%an|%ad",
                "--date=short",
            ],
        )

        if not success:
            return False, [], stderr

        commits = []
        for line in stdout.split("\n"):
            if "|" in line:
                parts = line.split("|", 3)
                if len(parts) >= 4:
                    commits.append(
                        CommitInfo(hash=parts[0][:8], title=parts[1], author=parts[2], date=parts[3])
                    )
        return True, commits, ""

    def inspect_repo(self, repo_path: Path) -> RepoResult:
        """Collect recent commits for a single repository."""
        repo_name = str(repo_path.relative_to(Path.cwd()))

        current_branch = self.get_current_branch(repo_path)
        if current_branch is None:
            return RepoResult(
                path=repo_name,
                branch="unknown",
                status="error",
                error_message="Could not determine current branch",
            )
        # Detached HEAD reports an empty branch name.
        branch = current_branch or "(detached)"

        success, commits, stderr = self.get_commits_since(repo_path)
        if not success:
            return RepoResult(
                path=repo_name,
                branch=branch,
                status="error",
                error_message=f"Failed to read log: {stderr}",
            )

        return RepoResult(
            path=repo_name,
            branch=branch,
            status="changes" if commits else "no_changes",
            commits=commits,
        )

    def _print_result(self, result: RepoResult):
        """Print immediate feedback for a repository result."""
        if result.status == "changes":
            print(
                f"{YELLOW} {RESET} {result.path} {GRAY}({len(result.commits)} commits){RESET}"
            )
        elif result.status == "no_changes":
            print(f"{GREEN} {RESET} {result.path} {GRAY}(no changes){RESET}")
        elif result.status == "error":
            print(f"{RED} {RESET} {result.path} {GRAY}(error){RESET}")

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
            f"Showing changes since {BOLD}{self.since}{RESET} "
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

    def print_summary(self):
        """Print a detailed summary of all operations."""

        changed_repos = sorted(
            (r for r in self.results if r.status == "changes"), key=lambda r: r.path
        )
        no_change_repos = [r for r in self.results if r.status == "no_changes"]
        error_repos = [r for r in self.results if r.status == "error"]

        if changed_repos:
            print(f"\n{YELLOW}  REPOSITORIES WITH CHANGES{RESET} ({len(changed_repos)})")
            for repo in changed_repos:
                print(f"\n{YELLOW} {repo.path} ({repo.branch}){RESET}")
                for commit in repo.commits:
                    print(
                        f"   {BLUE}{commit.hash}{RESET} {GRAY}{commit.date}{RESET} {commit.title} {GREEN}{DIM}{commit.author}{RESET}"
                    )

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

        total_commits = sum(len(r.commits) for r in changed_repos)

        print(f"\n{BLUE}󰈙  GIT HISTORY SUMMARY{RESET}\n")
        print(f"Since: \t\t\t{self.since}")
        print(f"Total repositories: \t{len(self.results)}")
        print(f"With changes: \t\t{len(changed_repos)}")
        print(f"No changes: \t\t{len(no_change_repos)}")
        print(f"Errors: \t\t{len(error_repos)}")
        print(f"Total commits: \t\t{total_commits}\n")


def main():
    parser = argparse.ArgumentParser(
        description="Recursively show recent commits across all git repositories "
        "in current and nested directories"
    )
    parser.add_argument(
        "--since",
        default="yesterday",
        help="Show commits more recent than this date (default: yesterday). "
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

    history = GitRepoHistory(since=args.since)

    try:
        history.inspect_all_repos(root_path, max_workers=50)
        history.print_summary()
    except KeyboardInterrupt:
        print(f"\n\n{YELLOW}  Operation cancelled by user{RESET}")
        if history.results:
            history.print_summary()
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
