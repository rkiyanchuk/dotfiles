#!/usr/bin/env python3
"""
Recursively pull all git repositories in current and nested directories.
Provides detailed reporting of updates, commits, and any issues encountered.

Requirements:
- Python 3.7+
- Nerd Fonts for icons (optional)
"""

import argparse
import subprocess
import sys
import textwrap

from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Tuple


# Color codes for status icons
BLUE = '\033[34m'
GRAY = '\033[90m'
GREEN = '\033[32m'
ORANGE = '\033[91m'
RED = '\033[31m'
YELLOW = '\033[33m'
RESET = '\033[0m'


@dataclass
class CommitInfo:
    hash: str
    title: str
    date: str


@dataclass
class RepoResult:
    path: str
    branch: str
    status: str  # 'updated', 'up_to_date', 'error', 'diverged'
    commits_pulled: List[CommitInfo] = field(default_factory=list)
    error_message: Optional[str] = None
    commits_ahead: int = 0
    commits_behind: int = 0


class GitRepoPuller:
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.results: List[RepoResult] = []
        
        
    def find_git_repos(self, root_path: Path) -> List[Path]:
        """Find all git repositories recursively."""
        repos = []
        for item in root_path.rglob('.git'):
            if item.is_dir():
                repo_path = item.parent
                repos.append(repo_path)
        return sorted(repos)
    
    def run_git_command(self, repo_path: Path, command: List[str]) -> Tuple[bool, str, str]:
        """Run a git command in the specified repository."""
        try:
            result = subprocess.run(
                ['git'] + command,
                cwd=repo_path,
                capture_output=True,
                text=True,
                timeout=30
            )
            return result.returncode == 0, result.stdout.strip(), result.stderr.strip()
        except subprocess.TimeoutExpired:
            return False, "", "Command timed out"
        except Exception as e:
            return False, "", str(e)
    
    def get_current_branch(self, repo_path: Path) -> Optional[str]:
        """Get the current branch name."""
        success, stdout, _ = self.run_git_command(repo_path, ['branch', '--show-current'])
        return stdout if success else None
    
    def get_main_branch(self, repo_path: Path) -> Optional[str]:
        """Get the main/master branch name."""
        # Check if main exists
        success, _, _ = self.run_git_command(repo_path, ['show-ref', '--verify', '--quiet', 'refs/heads/main'])
        if success:
            return 'main'
        
        # Check if master exists
        success, _, _ = self.run_git_command(repo_path, ['show-ref', '--verify', '--quiet', 'refs/heads/master'])
        if success:
            return 'master'
        
        return None
    
    def get_remote_tracking_branch(self, repo_path: Path, branch: str) -> Optional[str]:
        """Get the remote tracking branch for a local branch."""
        success, stdout, _ = self.run_git_command(
            repo_path, ['rev-parse', '--abbrev-ref', f'{branch}@{{upstream}}']
        )
        return stdout if success else None
    
    def get_commit_range_info(self, repo_path: Path, local_ref: str, remote_ref: str) -> Tuple[int, int]:
        """Get number of commits ahead and behind between local and remote."""
        # Commits behind (remote has but local doesn't)
        success, stdout, _ = self.run_git_command(
            repo_path, ['rev-list', '--count', f'{local_ref}..{remote_ref}']
        )
        behind = int(stdout) if success and stdout.isdigit() else 0
        
        # Commits ahead (local has but remote doesn't)
        success, stdout, _ = self.run_git_command(
            repo_path, ['rev-list', '--count', f'{remote_ref}..{local_ref}']
        )
        ahead = int(stdout) if success and stdout.isdigit() else 0
        
        return ahead, behind
    
    def get_commits_in_range(self, repo_path: Path, commit_range: str) -> List[CommitInfo]:
        """Get commit information for a range."""
        success, stdout, _ = self.run_git_command(
            repo_path, ['log', '--pretty=format:%H|%s|%an|%ad', '--date=short', commit_range]
        )
        
        if not success or not stdout:
            return []
        
        commits = []
        for line in stdout.split('\n'):
            if '|' in line:
                parts = line.split('|', 3)
                if len(parts) >= 4:
                    commits.append(CommitInfo(
                        hash=parts[0][:8],
                        title=parts[1],
                        date=parts[3]
                    ))
        return commits
    
    def update_repo(self, repo_path: Path) -> RepoResult:
        """Update a single repository."""
        repo_name = str(repo_path.relative_to(Path.cwd()))
        
        if self.verbose:
            print(f"\n {repo_name}")
        
        # Get current branch
        current_branch = self.get_current_branch(repo_path)
        if not current_branch:
            return RepoResult(
                path=repo_name,
                branch="unknown",
                status="error",
                error_message="Could not determine current branch"
            )
        
        # Find main/master branch
        main_branch = self.get_main_branch(repo_path)
        if not main_branch:
            return RepoResult(
                path=repo_name,
                branch=current_branch,
                status="error",
                error_message="No main or master branch found"
            )
        
        # Switch to main/master if not already there
        if current_branch != main_branch:
            success, _, stderr = self.run_git_command(repo_path, ['checkout', main_branch])
            if not success:
                return RepoResult(
                    path=repo_name,
                    branch=current_branch,
                    status="error",
                    error_message=f"Could not switch to {main_branch}: {stderr}"
                )
            current_branch = main_branch
        
        # Get remote tracking branch
        remote_branch = self.get_remote_tracking_branch(repo_path, main_branch)
        if not remote_branch:
            return RepoResult(
                path=repo_name,
                branch=current_branch,
                status="error",
                error_message=f"No remote tracking branch for {main_branch}"
            )
        
        # Fetch latest changes
        success, _, stderr = self.run_git_command(repo_path, ['fetch'])
        if not success:
            return RepoResult(
                path=repo_name,
                branch=current_branch,
                status="error",
                error_message=f"Failed to fetch: {stderr}"
            )
        
        # Check commits ahead/behind
        ahead, behind = self.get_commit_range_info(repo_path, main_branch, remote_branch)
        
        if behind == 0:
            status = "up_to_date" if ahead == 0 else "up_to_date"
            return RepoResult(
                path=repo_name,
                branch=current_branch,
                status=status,
                commits_ahead=ahead,
                commits_behind=behind
            )
        
        if ahead > 0:
            return RepoResult(
                path=repo_name,
                branch=current_branch,
                status="diverged",
                commits_ahead=ahead,
                commits_behind=behind,
                error_message=f"Branch has diverged: {ahead} ahead, {behind} behind"
            )
        
        # Get commits that will be pulled
        commits_to_pull = self.get_commits_in_range(repo_path, f'{main_branch}..{remote_branch}')
        
        # Perform the pull (fast-forward merge)
        success, _, stderr = self.run_git_command(repo_path, ['merge', '--ff-only', remote_branch])
        if not success:
            return RepoResult(
                path=repo_name,
                branch=current_branch,
                status="error",
                commits_ahead=ahead,
                commits_behind=behind,
                error_message=f"Failed to merge: {stderr}"
            )
        
        return RepoResult(
            path=repo_name,
            branch=current_branch,
            status="updated",
            commits_pulled=commits_to_pull,
            commits_ahead=ahead,
            commits_behind=behind
        )
    
    def pull_all_repos(self, root_path: Path = Path.cwd()):
        """Pull all repositories in the given path."""
        
        repos = self.find_git_repos(root_path)
        
        if not repos:
            print("No git repositories found.")
            return
        
        print(f"Found {len(repos)} git repositories:")
        
        for repo in repos:
            result = self.update_repo(repo)
            self.results.append(result)
            
            # Print immediate feedback
            if result.status == "updated":
                print(f"{YELLOW}󰓦 {RESET} {result.path} ({len(result.commits_pulled)} commits)")
            elif result.status == "up_to_date":
                print(f"{GREEN} {RESET} {result.path} (up to date)")
            elif result.status == "diverged":
                print(f"{ORANGE} {RESET} {result.path} (diverged)")
            elif result.status == "error":
                print(f"{RED} {RESET} {result.path} (error)")

    def print_summary(self):
        """Print a detailed summary of all operations."""
        
        updated_repos = [r for r in self.results if r.status == "updated"]
        up_to_date_repos = [r for r in self.results if r.status == "up_to_date"]
        diverged_repos = [r for r in self.results if r.status == "diverged"]
        error_repos = [r for r in self.results if r.status == "error"]
        
        
        if updated_repos:
            print(f"\n󰓦 UPDATED REPOSITORIES ({len(updated_repos)})")
            print("-" * 40)
            for repo in updated_repos:
                print(f"\n{YELLOW} {repo.path} ({repo.branch}){RESET}")
                for commit in repo.commits_pulled:
                    print(f"   {BLUE}{commit.hash}{RESET} {GRAY}{commit.date}{RESET} {commit.title}")

        if diverged_repos:
            print(f"\n{ORANGE}  DIVERGED REPOSITORIES ({len(diverged_repos)}){RESET}")
            print("-" * 40)
            for repo in diverged_repos:
                print(f"\n {repo.path} ({repo.branch})")
                print(f"  {repo.commits_ahead} commits ahead, {repo.commits_behind} commits behind")
                if repo.error_message:
                    print(f"  {repo.error_message}")
        
        if error_repos:
            print(f"\n❌ REPOSITORIES WITH ERRORS ({len(error_repos)})")
            print("-" * 40)
            for repo in error_repos:
                err_msg = textwrap.indent(repo.error_message, '    ') if repo.error_message else "Unknown error"
                print(f"\n{RED} {repo.path} ({repo.branch}) {RESET}")
                print(f"{err_msg}")

        if up_to_date_repos and self.verbose:
            print(f"\n  UP TO DATE REPOSITORIES ({len(up_to_date_repos)})")
            print("-" * 40)
            for repo in up_to_date_repos:
                ahead_info = f" ({repo.commits_ahead} ahead)" if repo.commits_ahead > 0 else ""
                print(f" {repo.path} ({repo.branch}){ahead_info}")

        print("\nGIT PULL SUMMARY")
        print("-" * 40)
        print(f"Total repositories: {len(self.results)}")
        print(f"Up to date: {len(up_to_date_repos)}")
        print(f"Updated: {len(updated_repos)}")
        print(f"Diverged: {len(diverged_repos)}")
        print(f"Errors: {len(error_repos)}\n")


def main():
    parser = argparse.ArgumentParser(
        description="Recursively pull all git repositories in current and nested directories"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output"
    )
    parser.add_argument(
        "path",
        nargs="?",
        default=".",
        help="Root path to search for repositories (default: current directory)"
    )
    
    args = parser.parse_args()
    
    root_path = Path(args.path).resolve()
    if not root_path.exists():
        print(f"Error: Path '{root_path}' does not exist")
        sys.exit(1)
    
    puller = GitRepoPuller(verbose=args.verbose)
    
    try:
        puller.pull_all_repos(root_path)
        puller.print_summary()
    except KeyboardInterrupt:
        print(f"\n\n{YELLOW}  Operation cancelled by user{RESET}")
        if puller.results:
            puller.print_summary()
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()