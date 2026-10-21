"""Tests for ao/review-done-when-checked in emacs/org-config.el.

Ticking the last checkbox of a REVIEW entry turns it into DONE. Each test runs
`emacs --batch -Q`, evaluates only the forms of org-config.el that belong to
this feature (the full config needs elpaca and the network), ticks boxes with
`org-toggle-checkbox`, and prints the resulting heading and CLOSED line.
"""

import json
import subprocess
from pathlib import Path

ORG_CONFIG = Path(__file__).resolve().parents[1] / "emacs/org-config.el"

# Evaluate the defun and the add-hook for this feature, and nothing else.
LOAD = f"""
(require 'org)
(with-temp-buffer
  (insert-file-contents "{ORG_CONFIG}")
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (or (and (eq (car-safe form) 'defun)
                         (eq (cadr form) 'ao/review-done-when-checked))
                    (and (eq (car-safe form) 'add-hook)
                         (equal (cadr form) ''org-checkbox-statistics-hook)))
            (eval form t))))
    (end-of-file nil)))
"""


def run(org_text, ticks):
    """Tick the checkboxes on the given 1-based lines, return the buffer text."""
    tick_forms = "\n".join(
        f"(goto-char (point-min)) (forward-line {n - 1}) (org-toggle-checkbox)"
        for n in ticks
    )
    script = f"""
{LOAD}
(with-temp-buffer
  (insert {json.dumps(org_text)})
  (org-mode)
  {tick_forms}
  (princ (buffer-string)))
"""
    # json.dumps of ASCII text is a valid Emacs string literal.
    out = subprocess.run(
        ["emacs", "--batch", "-Q", "--eval", f"(progn {script})"],
        capture_output=True, text=True, timeout=60,
    )
    assert out.returncode == 0, out.stderr
    return out.stdout


HEADER = "#+TODO: TODO ONGOING | REVIEW DONE\n"


def test_last_tick_turns_review_into_done():
    text = HEADER + "* REVIEW Check the fix\nPending:\n- [ ] one\n- [ ] two\n"
    out = run(text, ticks=[4, 5])
    assert out.splitlines()[1] == "* DONE Check the fix"
    assert "CLOSED: [" in out


def test_partial_ticks_keep_review():
    text = HEADER + "* REVIEW Check the fix\nPending:\n- [ ] one\n- [ ] two\n"
    out = run(text, ticks=[4])
    assert out.splitlines()[1] == "* REVIEW Check the fix"
    assert "CLOSED:" not in out


def test_open_entry_is_left_alone():
    # An ONGOING entry with every box ticked may still have work in NEXT:.
    text = HEADER + "* ONGOING Port the script\n- [ ] one\nNEXT: run it\n"
    out = run(text, ticks=[3])
    assert out.splitlines()[1] == "* ONGOING Port the script"


def test_child_heading_boxes_do_not_count():
    text = (
        HEADER
        + "* REVIEW Parent\n- [ ] own box\n"
        + "** TODO Child\n- [ ] child box\n"
    )
    out = run(text, ticks=[3])
    assert out.splitlines()[1] == "* DONE Parent"
    assert out.splitlines()[-1] == "- [ ] child box"


def test_existing_closed_stamp_is_kept():
    text = (
        HEADER
        + "* REVIEW Check the fix\nCLOSED: [2026-10-01 Thu 10:00]\n- [ ] one\n"
    )
    out = run(text, ticks=[4])
    assert out.splitlines()[1] == "* DONE Check the fix"
    assert out.count("CLOSED:") == 1
    assert "CLOSED: [2026-10-01 Thu 10:00]" in out
