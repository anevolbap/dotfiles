"""Tests for system/systemd-sleep/restart-wifi, the systemd sleep hook.

The hook runs as root and reloads ath11k_pci, so each test runs a copy with
/sbin/modprobe, logger and sleep swapped for stubs that log their calls.
"""

import subprocess
from pathlib import Path

import pytest

HOOK = Path(__file__).resolve().parents[1] / "system/systemd-sleep/restart-wifi"


@pytest.fixture
def hook(tmp_path):
    calls = tmp_path / "calls"
    stub = tmp_path / "stub"
    stub.write_text(f'#!/bin/sh\necho "$(basename "$0") $*" >> {calls}\n')
    stub.chmod(0o755)
    for name in ("modprobe", "logger"):
        (tmp_path / name).symlink_to(stub)
    text = HOOK.read_text()
    text = text.replace("/sbin/modprobe", str(tmp_path / "modprobe"))
    text = text.replace("logger ", f"{tmp_path / 'logger'} ")
    text = text.replace("sleep 1", "true")
    script = tmp_path / "restart-wifi"
    script.write_text(text)
    script.chmod(0o755)

    def run(*args):
        subprocess.run([str(script), *args], check=True)
        return calls.read_text().splitlines() if calls.exists() else []

    return run


@pytest.mark.parametrize("kind", ["suspend", "hibernate", "suspend-then-hibernate"])
def test_reloads_driver_after_resume(hook, kind):
    calls = hook("post", kind)
    assert "modprobe -r ath11k_pci" in calls
    assert "modprobe ath11k_pci" in calls


@pytest.mark.parametrize("kind", ["suspend", "hibernate", "suspend-then-hibernate"])
def test_does_nothing_before_sleep(hook, kind):
    assert hook("pre", kind) == []
