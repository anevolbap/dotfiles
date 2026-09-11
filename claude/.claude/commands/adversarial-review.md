# Adversarial Review

Review the current branch as a hostile reviewer before flipping a PR to ready-for-review.

Steps:
1. Run `git status` and `git diff <base>...HEAD` to see the full set of changes. Base defaults to `main` unless I say otherwise.
2. Read the diff as if you wanted to reject it. Look for: scope creep, missing tests, dead code, ignored edge cases, docstring/behavior mismatch, debug artifacts, unrelated formatting churn.
3. Check the PR description if a PR exists: `gh pr view --json title,body`. Confirm:
   - Concise, no em-dashes, no unsolicited @mentions, no pedantic asides.
   - Commits split logically.
   - References cited if any (papers, issues, prior PRs).
4. Check CI: `gh pr checks` if a PR is open, otherwise run the local test and lint commands the repo uses.
5. Verify any upstream claim (docstring, behavior, bug) against the current source of the dependency before asserting it in the PR.
6. Report findings in prose: what a reviewer would push back on, what is missing, what to fix. Order by severity.
7. Do not flip the PR to ready-for-review until I sign off on the remediation.
