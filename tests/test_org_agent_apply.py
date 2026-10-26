"""Tests for local/.local/bin/org-agent-apply.

Each test puts fake claude, emacsclient and notify-send on PATH. The fakes log
their arguments, so the tests check when Claude runs, with which permissions,
and what the notification says. No test calls the real Claude.
"""

import os
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[1] / "local/.local/bin/org-agent-apply"

FAKES = {
    # Prints the summary line the skill asks for, so the script can notify.
    "claude": 'echo "$@" >> "$FAKE_LOG/claude"\necho "applied 1: Renew passport -> DONE"',
    # FAKE_EMACS holds what the daemon would print, or "fail" for no daemon.
    "emacsclient": '[ "$FAKE_EMACS" = fail ] && exit 1\necho "$FAKE_EMACS"',
    "notify-send": 'echo "$@" >> "$FAKE_LOG/notify-send"',
}

TICKED = "* TODO Review triage\n** Renew passport -> DONE\n- [X] apply\n"
UNTICKED = "* TODO Review triage\n** Renew passport -> DONE\n- [ ] apply\n"
APPLIED = "* DONE Review triage\n** Renew passport -> DONE\n- [X] applied [2026-10-21]\n"


@pytest.fixture
def env(tmp_path):
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    for name, body in FAKES.items():
        p = bin_dir / name
        p.write_text("#!/bin/sh\n" + body + "\n")
        p.chmod(0o755)
    org = tmp_path / "org"
    org.mkdir()
    log = tmp_path / "log"
    log.mkdir()
    return {
        "PATH": f"{bin_dir}:{os.environ['PATH']}",
        "HOME": str(tmp_path),
        "ORG_DIR": str(org),
        "CLAUDE": str(bin_dir / "claude"),
        "FAKE_LOG": str(log),
        "FAKE_EMACS": "nil",
    }


def run(env, report):
    Path(env["ORG_DIR"], "review-triage.org").write_text(report)
    out = subprocess.run([str(SCRIPT)], env=env, capture_output=True,
                         text=True, timeout=30)
    log = Path(env["FAKE_LOG"])
    read = lambda n: (log / n).read_text() if (log / n).exists() else ""
    return out, read("claude"), read("notify-send")


def test_nothing_ticked_does_not_call_claude(env):
    out, claude, notify = run(env, UNTICKED)
    assert out.returncode == 0
    assert claude == "" and notify == ""


def test_already_applied_does_not_call_claude(env):
    # The agent's own write to the report fires the watcher again.
    out, claude, _ = run(env, APPLIED)
    assert out.returncode == 0
    assert claude == ""


def test_ticked_runs_claude_with_edit_limited_to_org_files(env):
    out, claude, notify = run(env, TICKED)
    assert out.returncode == 0, out.stderr
    org = env["ORG_DIR"]
    for name in ("tasks.org", "projects.org", "review-triage.org"):
        assert f"Edit(/{org}/{name})" in claude
    assert "Bash(" not in claude.replace("Bash(date:*)", "")
    assert "applied 1: Renew passport -> DONE" in notify


def test_unsaved_emacs_buffer_skips_and_notifies(env):
    env["FAKE_EMACS"] = f'"{env["ORG_DIR"]}/projects.org"'
    out, claude, notify = run(env, TICKED)
    assert out.returncode == 0
    assert claude == ""
    assert "unsaved" in notify and "projects.org" in notify


def test_no_emacs_daemon_still_applies(env):
    env["FAKE_EMACS"] = "fail"
    out, claude, _ = run(env, TICKED)
    assert out.returncode == 0, out.stderr
    assert claude != ""
