"""Tests for the Work agenda command and ao/appt-notify in emacs/org-config.el.

Runs Emacs in batch mode and evaluates only the forms under test from
org-config.el (the rest of the file needs the full package set): the NEXT:
helpers, the "w" entry of `org-agenda-custom-commands', and ao/appt-notify.
"""

import shutil
import subprocess
from datetime import date, timedelta
from pathlib import Path

import pytest

CONFIG = Path(__file__).resolve().parents[1] / "emacs/org-config.el"

pytestmark = pytest.mark.skipif(shutil.which("emacs") is None, reason="emacs not installed")

READ_FORMS = """
(defun test/read-form (start)
  (with-temp-buffer
    (insert-file-contents "{config}")
    (search-forward start)
    (goto-char (match-beginning 0))
    (read (current-buffer))))
"""


def run_emacs(body):
    elisp = "(progn " + READ_FORMS.format(config=CONFIG) + body + ")"
    result = subprocess.run(
        ["emacs", "-Q", "--batch", "--eval", elisp], capture_output=True, text=True
    )
    assert result.returncode == 0, result.stderr
    return result.stdout


def stamp(day, hhmm):
    return f"{day:%Y-%m-%d %a} {hhmm}"


AGENDA = """
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(dolist (name '("(defun ao/entry-next-action" "(defun ao/agenda-skip-unless-next"
                "(defun ao/agenda-insert-next-actions"))
  (eval (test/read-form name) t))
(defvar my-org-work-files '("{work}"))
(let ((command (eval (list '\\` (test/read-form "(\\"w\\" \\"Work\\"")) t)))
  (setq org-agenda-files '("{personal}" "{work}")
        org-agenda-custom-commands (list command))
  (org-agenda nil "w")
  (princ (with-current-buffer org-agenda-buffer-name
           (buffer-substring-no-properties (point-min) (point-max)))))
"""


def test_work_view_shows_only_work_files(tmp_path):
    today = date.today()
    work = tmp_path / "work.org"
    work.write_text(
        "#+TODO: TODO ONGOING | DONE\n"
        "* Client\n"
        "** ONGOING Billable task\n"
        ":LOGBOOK:\n"
        f"CLOCK: [{stamp(today, '09:00')}]--[{stamp(today, '10:30')}] =>  1:30\n"
        ":END:\n"
        "NEXT: write the report.\n"
        "** TODO Due soon\n"
        f"DEADLINE: <{today + timedelta(days=3):%Y-%m-%d %a}>\n"
        "** TODO No next line\n"
    )
    personal = tmp_path / "personal.org"
    personal.write_text(
        "* TODO Personal task\n"
        ":LOGBOOK:\n"
        f"CLOCK: [{stamp(today, '11:00')}]--[{stamp(today, '12:00')}] =>  1:00\n"
        ":END:\n"
        "NEXT: not billable.\n"
    )
    out = run_emacs(AGENDA.format(work=work, personal=personal))

    today_block, rest = out.split("Next 14 days", 1)
    upcoming, pickup = rest.split("Pick up", 1)
    assert "Billable task" in today_block
    assert "Clocked:" in today_block and "(1:30)" in today_block
    assert "*Total time*" in today_block and "*1:30*" in today_block
    assert "Due soon" in upcoming
    assert "Billable task" in pickup
    assert "NEXT: write the report." in pickup
    assert "No next line" not in pickup
    assert "Personal task" not in out
    assert "not billable" not in out


APPT = """
(defvar test/sent nil)
(defun ao/notify (title body &optional _urgency) (push (list title body) test/sent))
(eval (test/read-form "(defun ao/appt-notify") t)
(ao/appt-notify "10" "14:00" "Group meeting")
(ao/appt-notify '("5" "10") "14:00" '("Weekly sync" "1:1"))
(prin1 (reverse test/sent))
"""


def test_appt_notify_sends_one_notification_per_reminder():
    out = run_emacs(APPT)
    assert out == (
        '(("In 10 min" "Group meeting") ("In 5 min" "Weekly sync") ("In 10 min" "1:1"))'
    )
