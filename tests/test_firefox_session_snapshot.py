"""Tests for local/.local/bin/firefox-session-snapshot.

Each test builds a fake Firefox profile under a temporary HOME and runs the
real script, so it reads and writes the same paths it would in use.
"""

import json
import os
import subprocess
import time
from pathlib import Path

import lz4.block
import pytest

SCRIPT = Path(__file__).resolve().parents[1] / "local/.local/bin/firefox-session-snapshot"


def mozlz4(session):
    return b"mozLz40\0" + lz4.block.compress(json.dumps(session).encode())


def tab(title, url):
    return {"entries": [{"title": title, "url": url}], "index": 1}


SESSION = {
    "windows": [{"tabs": [tab("Docs [draft]", "https://example.com/a"), {**tab("B", "https://example.com/b"), "pinned": True}]}],
    "_closedWindows": [{"tabs": [tab("Closed", "https://example.com/c")], "closedAt": 1_700_000_000_000}],
}


@pytest.fixture
def home(tmp_path):
    profile = tmp_path / ".mozilla/firefox/abc.default-esr/sessionstore-backups"
    profile.mkdir(parents=True)
    (profile / "recovery.jsonlz4").write_bytes(mozlz4(SESSION))
    return tmp_path


def run(home):
    subprocess.run(["python3", str(SCRIPT)], env={**os.environ, "HOME": str(home)}, check=True)
    return home / ".local/state/firefox-sessions/abc.default-esr"


def test_copies_session_and_writes_org(home):
    out = run(home)
    snaps = sorted(out.glob("*.jsonlz4"))
    assert len(snaps) == 1
    assert snaps[0].read_bytes() == mozlz4(SESSION)
    org = (home / "Documents/org/firefox-tabs.org").read_text()
    assert "* Window 1 (2 tabs)" in org
    assert "** [[https://example.com/b][B]] :pinned:" in org
    assert "** [[https://example.com/a][Docs (draft)]]\n" in org
    assert "* Closed window 1 (1 tabs)" in org
    assert org.startswith(":PROPERTIES:\n:UPDATED:  [")
    assert f":SNAPSHOT: {snaps[0]}\n" in org
    assert "#+title: Firefox tabs\n#+startup: overview\n" in org
    assert ":CLOSED_AT: [2023-11-1" in org


def test_unchanged_session_is_not_copied_again(home):
    run(home)
    out = run(home)
    assert len(list(out.glob("*.jsonlz4"))) == 1


def test_old_snapshots_are_pruned_but_newest_is_kept(home):
    out = home / ".local/state/firefox-sessions/abc.default-esr"
    out.mkdir(parents=True)
    old = time.time() - 8 * 86400
    for name in ["20200101-0000.jsonlz4", "20200102-0000.jsonlz4"]:
        (out / name).write_bytes(mozlz4(SESSION))
        os.utime(out / name, (old, old))
    run(home)
    # The live session equals the newest old copy, so nothing new is written
    # and that copy must survive, even though it is older than the limit.
    assert [p.name for p in out.glob("*.jsonlz4")] == ["20200102-0000.jsonlz4"]
