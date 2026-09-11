"""Tests for local/.local/bin/claude-branch-check, the Bash PreToolUse hook.

The hook reads the Claude Code hook JSON on stdin and exits 2 to block the
call. Each test runs the real script, so it checks the same contract Claude
Code sees.
"""

import json
import subprocess
from pathlib import Path

import pytest

HOOK = Path(__file__).resolve().parents[1] / "local/.local/bin/claude-branch-check"


def run_hook(command, cwd):
    payload = {"tool_name": "Bash", "tool_input": {"command": command}, "cwd": str(cwd)}
    return subprocess.run(
        ["python3", str(HOOK)], input=json.dumps(payload), capture_output=True, text=True
    )


def git(repo, *args):
    subprocess.run(["git", "-C", str(repo), *args], check=True, capture_output=True)


@pytest.fixture
def repo(tmp_path):
    git(tmp_path, "init", "-q", "-b", "main")
    return tmp_path


@pytest.mark.parametrize(
    "command",
    [
        "git add -A",
        "git add --all",
        "git add .",
        "git add -u",
        "git add --update",
        "git -C /repo add -A",
        "cd /repo && git add -A && git commit -m x",
        "git commit -a -m x",
        "git commit -am x",
        "git commit --all -m x",
    ],
)
def test_blocks_staging_everything(command, tmp_path):
    result = run_hook(command, tmp_path)
    assert result.returncode == 2
    assert "explicit paths" in result.stderr


@pytest.mark.parametrize(
    "command",
    [
        "git add src/a.py tests/b.py",
        "git add .gitignore",
        "git add -p src/a.py",
        "git commit --amend --no-edit",
        'git commit -m "never use git add -A"',
        "git status",
    ],
)
def test_allows_explicit_staging(command, tmp_path):
    # tmp_path is not a git repository, so commits skip the branch check.
    assert run_hook(command, tmp_path).returncode == 0


def test_blocks_commit_on_shared_branch(repo):
    result = run_hook("git commit -m x", repo)
    assert result.returncode == 2
    assert "shared branch 'main'" in result.stderr


def test_allows_commit_on_feature_branch(repo):
    git(repo, "checkout", "-q", "-b", "feat/x")
    result = run_hook("git commit -m x", repo)
    assert result.returncode == 0
    assert "feat/x" in result.stdout
