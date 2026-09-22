# Blinking a Corne keyboard when Claude Code needs attention

This documents how to make a Corne keyboard flash its LEDs whenever Claude Code
finishes a response and waits for input. It covers QMK firmware with Raw HID, a
Python host script, Claude Code hooks, and Linux-specific issues encountered
along the way.

---

## Overview

The setup has three parts:

1. **Firmware** -- code running on the keyboard's microcontroller. It listens
   for a message from the computer over USB and triggers the LED animation when
   that message arrives.

2. **Host script** -- a small Python script running on the computer. It finds
   the keyboard's USB interface and sends the message the firmware expects.

3. **Hook** -- a Claude Code configuration that runs the host script
   automatically every time Claude finishes a response. No manual triggering
   needed.

---

## Firmware side

The Corne runs QMK. To receive data from the host, `RAW_ENABLE = yes` goes in
`rules.mk`. Then in `keymap.c`:

```c
#ifdef RAW_ENABLE
#include "raw_hid.h"

#define ALERT_BLINKS   20
#define ALERT_INTERVAL 200  // ms

static bool     alert_active = false;
static uint8_t  alert_count  = 0;
static bool     alert_phase  = false;  // false=blue, true=orange
static uint32_t alert_timer  = 0;

// saved state to restore after the alert
static bool    saved_enabled = false;
static uint8_t saved_mode, saved_hue, saved_sat, saved_val;

void raw_hid_receive(uint8_t *data, uint8_t length) {
    if (data[0] == 0x01 && !alert_active) {
        saved_enabled = rgblight_is_enabled();
        saved_mode    = rgblight_get_mode();
        saved_hue     = rgblight_get_hue();
        saved_sat     = rgblight_get_sat();
        saved_val     = rgblight_get_val();

        alert_active = true;
        alert_count  = ALERT_BLINKS;
        alert_phase  = false;
        alert_timer  = timer_read32();

        rgblight_enable_noeeprom();
        rgblight_mode_noeeprom(RGBLIGHT_MODE_STATIC_LIGHT);
        rgblight_sethsv_noeeprom(170, 255, 200);  // blue
    }

    uint8_t response[32] = {0};
    response[0] = 0x01;
    raw_hid_send(response, sizeof(response));
}

void housekeeping_task_user(void) {
    if (!alert_active) return;
    if (timer_elapsed32(alert_timer) < ALERT_INTERVAL) return;

    alert_timer = timer_read32();
    alert_phase = !alert_phase;
    rgblight_sethsv_noeeprom(alert_phase ? 21 : 170, 255, 200);  // orange / blue

    if (--alert_count == 0) {
        alert_active = false;
        if (saved_enabled) {
            rgblight_enable_noeeprom();
            rgblight_mode_noeeprom(saved_mode);
            rgblight_sethsv_noeeprom(saved_hue, saved_sat, saved_val);
        } else {
            rgblight_disable_noeeprom();
        }
    }
}
#endif
```

The effect alternates between blue (hue 170) and orange (hue 21) for 4 seconds,
then restores whatever the LEDs were doing before. QMK maps hue 0-255 to 0-360°:
red=0, yellow=43, green=85, cyan=128, blue=170, so the values follow directly
from the color wheel.

A few things worth noting:

- The Corne uses **RGBLIGHT** (underglow LEDs), not RGB Matrix (per-key). In
  current QMK the correct keycodes for controlling underglow are `UG_TOGG`,
  `UG_HUEU`, etc. The old `RGB_TOG` family is gone and `RM_*` targets RGB
  Matrix, which is the wrong subsystem.
- `RGBLIGHT_ENABLE = yes` needs to be explicit in the keymap `rules.mk`. The
  board's default `info.json` enables `rgb_matrix`, which adds ~1 KB of unused
  code. Disabling it drops the firmware from 97% to 93% of flash. The board also
  enables OLED by default, but re-enabling it in the keymap costs nothing without
  an `oled_task_user()` implementation. QMK only links the driver if the hook is
  defined.

Flash both halves: `qmk flash -kb crkbd -km <keymap> -bl dfu`

---

## Host script

The script finds the Raw HID interface and sends a 32-byte packet with `0x01`
in the first byte.

```python
#!/usr/bin/env python3
import sys, os, glob

QMK_USAGE_PAGE = 0xFF60
QMK_USAGE      = 0x0061
COMMAND_ALERT  = 0x01

_RAW_HID_DESC_PREFIX = bytes([0x06, 0x60, 0xFF, 0x09, 0x61])

def _find_keyboard_via_sysfs():
    for rd_path in glob.glob("/sys/class/hidraw/hidraw*/device/report_descriptor"):
        try:
            with open(rd_path, "rb") as f:
                desc = f.read(len(_RAW_HID_DESC_PREFIX))
            if desc == _RAW_HID_DESC_PREFIX:
                return f"/dev/{rd_path.split('/')[4]}".encode()
        except OSError:
            continue
    return None

def find_keyboard():
    try:
        import hid
    except ImportError:
        return None
    for dev in hid.enumerate():
        if dev.get("usage_page") == QMK_USAGE_PAGE and dev.get("usage") == QMK_USAGE:
            return dev["path"]
    if os.path.isdir("/sys/class/hidraw"):
        return _find_keyboard_via_sysfs()
    return None

def send_alert():
    try:
        import hid
    except ImportError:
        sys.exit(0)

    path = find_keyboard()
    if path is None:
        sys.exit(0)

    if os.path.isdir("/sys/class/hidraw"):
        with open(path, "wb") as f:
            f.write(bytes([COMMAND_ALERT]) + bytes(31))
        return

    h = hid.device()
    h.open_path(path)
    h.set_nonblocking(True)
    packet = [0x00] + [0x00] * 32
    packet[1] = COMMAND_ALERT
    h.write(packet)
    h.close()

if __name__ == "__main__":
    send_alert()
```

### Linux issues

Two Linux-specific problems came up.

**1. `hid.enumerate()` does not report usage pages.**

The `libhidapi-hidraw` backend (default on Linux) does not parse HID report
descriptors, so every device shows `usage_page = 0`. The fix is to read the
report descriptor directly from sysfs and match the known prefix bytes for QMK
Raw HID (`06 60 FF 09 61`).

**2. `hid.device().open_path()` fails on the Raw HID interface.**

After opening the file descriptor, `libhidapi-hidraw` does extra `ioctl` calls
that fail on this interface. The fix is to skip the `hid` module for writing
and use `open(path, "wb")` directly. The udev rule below grants access to
`/dev/hidrawN` for the `plugdev` group.

```
KERNEL=="hidraw*", MODE="0660", GROUP="plugdev", TAG+="uaccess"
```

---

## Claude Code hook

Claude Code supports hooks: shell commands that run on events. The `Stop`
event fires after every response. Add this to `~/.claude/settings.json`:

```json
{
  "hooks": {
    "Stop": [
      {
        "matcher": "",
        "hooks": [{ "type": "command", "command": "$HOME/.local/bin/corne-rgb-alert" }]
      }
    ],
    "Notification": [
      {
        "matcher": "",
        "hooks": [{ "type": "command", "command": "$HOME/.local/bin/corne-rgb-alert" }]
      }
    ]
  }
}
```

### Settings are loaded at session start

Claude Code reads `settings.json` once when the session starts. If the file
changes mid-session, the hooks do not reload until the next restart. This is
easy to miss when setting up for the first time.

---

## Result

After flashing both keyboard halves and restarting Claude Code, the keyboard
alternates blue and orange for 4 seconds on every response.

Source: `crkbd_layout/` (firmware) and `~/.local/bin/corne-rgb-alert` (host script).
