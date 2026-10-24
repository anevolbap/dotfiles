"""Tests for local/.local/bin/claude-publish-guard, the Bash PreToolUse hook.

The hook lets agents publish freely to repositories owned by anevolbap, blocks
any git push to someone else's repository, and asks before gh writes (PRs,
issues, comments, API writes) that land in someone else's repository. Each
test runs the real script on the Claude Code hook JSON, against throwaway
repositories whose remotes are never contacted.
"""

import json
import subprocess
from pathlib import Path

import pytest

HOOK = Path(__file__).resolve().parents[1] / "local/.local/bin/claude-publish-guard"

OWN = "https://github.com/anevolbap/mobel.git"
FORK = "git@github.com:anevolbap/CausalPy.git"
UPSTREAM = "https://github.com/pymc-labs/CausalPy.git"


def git(repo, *args):
    subprocess.run(["git", "-C", str(repo), *args], check=True, capture_output=True)


def make_repo(path, **remotes):
    path.mkdir()
    git(path, "init", "-q", "-b", "feat")
    for name, url in remotes.items():
        git(path, "remote", "add", name, url)
    return path


@pytest.fixture
def own(tmp_path):
    return make_repo(tmp_path / "own", origin=OWN)


@pytest.fixture
def oss(tmp_path):
    return make_repo(tmp_path / "oss", origin=FORK, upstream=UPSTREAM)


def run_hook(command, cwd):
    payload = {"tool_name": "Bash", "tool_input": {"command": command}, "cwd": str(cwd)}
    return subprocess.run(
        ["python3", str(HOOK)], input=json.dumps(payload), capture_output=True, text=True
    )


def verdict(result):
    """'block', 'ask' or 'pass' (no opinion: the normal permission flow decides)."""
    if result.returncode == 2:
        return "block"
    assert result.returncode == 0, result.stderr
    if result.stdout.strip():
        out = json.loads(result.stdout)
        return out["hookSpecificOutput"]["permissionDecision"]
    return "pass"


# git push


def test_push_to_own_repo_passes(own):
    assert verdict(run_hook("git push origin feat", own)) == "pass"


def test_push_to_own_fork_passes(oss):
    assert verdict(run_hook("git push -u origin feat", oss)) == "pass"


def test_push_to_upstream_is_blocked(oss):
    result = run_hook("git push upstream feat", oss)
    assert verdict(result) == "block"
    assert "pymc-labs" in result.stderr


def test_bare_push_follows_the_branch_remote(oss):
    git(oss, "config", "branch.feat.remote", "upstream")
    assert verdict(run_hook("git push", oss)) == "block"
    git(oss, "config", "branch.feat.remote", "origin")
    assert verdict(run_hook("git push", oss)) == "pass"


def test_push_remote_override_wins(oss):
    git(oss, "config", "branch.feat.remote", "origin")
    git(oss, "config", "remote.pushDefault", "upstream")
    assert verdict(run_hook("git push", oss)) == "block"


def test_push_by_url_with_credential_helper(own, oss):
    helper = "-c credential.helper='!gh auth git-credential'"
    blocked = f"git -C {oss} {helper} push {UPSTREAM} feat"
    allowed = f"git -C {oss} {helper} push https://github.com/anevolbap/CausalPy.git feat"
    assert verdict(run_hook(blocked, own)) == "block"
    assert verdict(run_hook(allowed, own)) == "pass"


def test_cd_prefix_is_followed(own, oss):
    assert verdict(run_hook(f"cd {oss} && git push upstream feat", own)) == "block"


def test_push_in_a_chain_is_found(oss):
    assert verdict(run_hook("git add x.py; git commit -m x && git push upstream feat", oss)) == "block"


def test_push_to_local_path_passes(tmp_path, own):
    bare = tmp_path / "bare.git"
    subprocess.run(["git", "init", "-q", "--bare", str(bare)], check=True)
    assert verdict(run_hook(f"git push {bare} feat", own)) == "pass"


def test_push_to_other_host_is_blocked(own):
    assert verdict(run_hook("git push https://gitlab.com/anevolbap/x.git feat", own)) == "block"


# gh writes


def test_pr_create_in_fork_clone_asks(oss):
    # Without -R, gh pr create in a fork targets the parent repository.
    assert verdict(run_hook("gh pr create --draft --fill", oss)) == "ask"


def test_pr_create_with_own_repo_flag_passes(oss):
    assert verdict(run_hook("gh pr create -R anevolbap/CausalPy --fill", oss)) == "pass"


def test_pr_create_in_own_repo_passes(own):
    assert verdict(run_hook("gh pr create --fill", own)) == "pass"


def test_comment_by_url_on_someone_else_asks(own):
    cmd = "gh pr comment https://github.com/pymc-labs/CausalPy/pull/12 --body hi"
    assert verdict(run_hook(cmd, own)) == "ask"


def test_issue_comment_with_repo_flag_asks(own):
    cmd = "gh issue comment 5 --repo pymc-devs/pymc --body hi"
    assert verdict(run_hook(cmd, own)) == "ask"


def test_gh_reads_pass(oss):
    for cmd in ("gh pr view 12 --repo pymc-labs/CausalPy", "gh issue list", "gh pr checks 3"):
        assert verdict(run_hook(cmd, oss)) == "pass", cmd


# gh api


def test_api_get_on_someone_else_passes(own):
    assert verdict(run_hook("gh api repos/pymc-labs/CausalPy/pulls", own)) == "pass"


def test_api_patch_on_someone_else_asks(own):
    cmd = "gh api -X PATCH repos/pymc-labs/CausalPy/issues/comments/1 -f body=x"
    assert verdict(run_hook(cmd, own)) == "ask"


def test_api_fields_imply_post(own):
    assert verdict(run_hook("gh api repos/pymc-labs/CausalPy/issues -f title=t", own)) == "ask"
    assert verdict(run_hook("gh api repos/anevolbap/mobel/issues -f title=t", own)) == "pass"


def test_api_get_with_fields_passes(own):
    cmd = "gh api -X GET search/issues -f q=bocpd"
    assert verdict(run_hook(cmd, own)) == "pass"


def test_graphql_mutation_asks_and_query_passes(own):
    assert verdict(run_hook("gh api graphql -f query='mutation { x }'", own)) == "ask"
    assert verdict(run_hook("gh api graphql -f query='query { x }'", own)) == "pass"


# everything else


def test_unrelated_commands_pass(own):
    for cmd in ("ls -la", "git status", "git log --oneline -5", "echo 'git push upstream'"):
        assert verdict(run_hook(cmd, own)) == "pass", cmd


def test_unparseable_publish_command_asks(own):
    assert verdict(run_hook("git push upstream 'unterminated", own)) == "ask"


def test_non_bash_payload_passes(own):
    payload = {"tool_name": "Read", "tool_input": {"file_path": "x"}, "cwd": str(own)}
    result = subprocess.run(["python3", str(HOOK)], input=json.dumps(payload),
                            capture_output=True, text=True)
    assert verdict(result) == "pass"
