"""Tests for local/.local/bin/osd, the volume and brightness popup.

Each test puts fake pactl, brightnessctl and notify-send on PATH. The fakes log
their arguments, so the tests check what osd changes and what it shows.
"""

import os
import subprocess
from pathlib import Path

import pytest

OSD = Path(__file__).resolve().parents[1] / "local/.local/bin/osd"

FAKES = {
    "pactl": """
case "$1" in
    get-sink-volume) echo "Volume: front-left: 33370 /  51% / -17.59 dB,   front-right: 33370 /  51% / -17.59 dB" ;;
    get-sink-mute) echo "Mute: ${FAKE_MUTE:-no}" ;;
esac
""",
    "brightnessctl": """
[ "$1" = -m ] && echo "amdgpu_bl0,backlight,229,90%,255"
""",
    "notify-send": "",
}


@pytest.fixture
def bin_dir(tmp_path):
    for name, body in FAKES.items():
        fake = tmp_path / name
        fake.write_text(f'#!/bin/sh\necho "{name} $*" >> "$FAKE_LOG"\n{body}')
        fake.chmod(0o755)
    return tmp_path


def run_osd(bin_dir, *args, mute="no"):
    log = bin_dir / "log"
    log.touch()
    env = {
        **os.environ,
        "PATH": f"{bin_dir}:{os.environ['PATH']}",
        "FAKE_LOG": str(log),
        "FAKE_MUTE": mute,
    }
    result = subprocess.run(["bash", str(OSD), *args], env=env, capture_output=True, text=True)
    return result, log.read_text().splitlines()


def notification(calls):
    return next(c for c in calls if c.startswith("notify-send"))


def test_volume_up_shows_level(bin_dir):
    result, calls = run_osd(bin_dir, "volume", "up")
    assert result.returncode == 0
    assert "pactl set-sink-volume @DEFAULT_SINK@ +5%" in calls
    sent = notification(calls)
    assert "int:value:51" in sent
    assert "string:x-dunst-stack-tag:osd-volume" in sent
    assert "--transient" in sent
    assert sent.endswith("Volume: 51%")


def test_volume_mute_shows_muted(bin_dir):
    _, calls = run_osd(bin_dir, "volume", "mute", mute="yes")
    assert "pactl set-sink-mute @DEFAULT_SINK@ toggle" in calls
    sent = notification(calls)
    assert "audio-volume-muted-symbolic" in sent
    assert sent.endswith("Volume: muted")


def test_brightness_down_shows_level(bin_dir):
    result, calls = run_osd(bin_dir, "brightness", "down")
    assert result.returncode == 0
    assert "brightnessctl -q set 5%-" in calls
    sent = notification(calls)
    assert "int:value:90" in sent
    assert "string:x-dunst-stack-tag:osd-brightness" in sent
    assert sent.endswith("Brightness: 90%")


def test_bad_args_exit_2_without_changes(bin_dir):
    result, calls = run_osd(bin_dir, "volume")
    assert result.returncode == 2
    assert "usage" in result.stderr
    assert calls == []
