"""Tests for local/.local/bin/work-backup.

A fake gcloud on PATH logs its arguments one per line, so the tests check the
destination, the source and the exclude pattern without touching a bucket.
"""

import os
import re
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[1] / "local/.local/bin/work-backup"


@pytest.fixture
def env(tmp_path):
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    fake = bin_dir / "gcloud"
    fake.write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$FAKE_LOG"\n')
    fake.chmod(0o755)
    home = tmp_path / "home"
    (home / ".config/work-backup").mkdir(parents=True)
    return {
        "PATH": f"{bin_dir}:{os.environ['PATH']}",
        "HOME": str(home),
        "FAKE_LOG": str(tmp_path / "gcloud.log"),
    }


def run(env, *args):
    return subprocess.run([SCRIPT, *args], env=env, capture_output=True, text=True)


def test_syncs_work_dir_to_configured_bucket(env):
    conf = Path(env["HOME"]) / ".config/work-backup/env"
    conf.write_text("WORK_BACKUP_DEST=gs://bucket/laptop\n")
    result = run(env, "--dry-run")
    assert result.returncode == 0, result.stderr
    args = Path(env["FAKE_LOG"]).read_text().splitlines()
    assert args[:3] == ["storage", "rsync", "--recursive"]
    assert "--dry-run" in args
    assert args[-2:] == [f"{env['HOME']}/Documents/work", "gs://bucket/laptop"]
    assert "--delete-unmatched-destination-objects" not in args


def test_fails_without_destination(env):
    result = run(env)
    assert result.returncode != 0
    assert "WORK_BACKUP_DEST" in result.stderr
    assert not Path(env["FAKE_LOG"]).exists()


@pytest.mark.parametrize(
    "path, skipped",
    [
        ("repos/client/.pixi/envs/default/bin/python", True),
        (".pixi/x", True),
        ("repos/client/.scratch/rendered/deck.html", True),
        ("repos/a/src/pkg/__pycache__/m.pyc", True),
        ("repos/a/node_modules/x/index.js", True),
        ("org/work.org", False),
        ("repos/client/pixi.lock", False),
        ("repos/client/.envrc", False),
        ("repos/client/.git/HEAD", False),
    ],
)
def test_exclude_pattern(env, path, skipped):
    conf = Path(env["HOME"]) / ".config/work-backup/env"
    conf.write_text("WORK_BACKUP_DEST=gs://b\n")
    run(env)
    exclude = next(
        a.split("=", 1)[1]
        for a in Path(env["FAKE_LOG"]).read_text().splitlines()
        if a.startswith("--exclude=")
    )
    assert bool(re.match(exclude, path)) is skipped
