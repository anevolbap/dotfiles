"""Tests for ao/refile-target-p in emacs/org-config.el.

Runs Emacs in batch mode, evaluates only that defun from org-config.el (the
rest of the file needs the full package set), and builds refile targets for a
small org file the way `org-refile' does.
"""

import shutil
import subprocess
from pathlib import Path

import pytest

CONFIG = Path(__file__).resolve().parents[1] / "emacs/org-config.el"

ORG = """\
* Project
** TODO Entry one
Summary.
*** Context
Background.
** DONE Entry two
*** Context
More background.
*** Findings
"""

ELISP = """
(progn
  (require 'org)
  (require 'org-refile)
  (with-temp-buffer
    (insert-file-contents "{config}")
    (search-forward "(defun ao/refile-target-p")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t))
  (setq org-refile-targets '((nil :maxlevel . 3))
        org-refile-use-outline-path t
        org-refile-target-verify-function #'ao/refile-target-p)
  (find-file "{org}")
  (dolist (target (org-refile-get-targets))
    (princ (format "%s\\n" (car target)))))
"""


@pytest.mark.skipif(shutil.which("emacs") is None, reason="emacs not installed")
def test_context_headings_are_not_refile_targets(tmp_path):
    org = tmp_path / "projects.org"
    org.write_text(ORG)
    elisp = ELISP.format(config=CONFIG, org=org)
    result = subprocess.run(
        ["emacs", "-Q", "--batch", "--eval", elisp], capture_output=True, text=True, check=True
    )
    targets = result.stdout.splitlines()
    assert "Project/Entry one" in targets
    assert "Project/Entry two/Findings" in targets
    assert not [t for t in targets if t.endswith("/Context")]
