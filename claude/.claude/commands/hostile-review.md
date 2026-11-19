# Hostile Review

Adversarial, empirically verified review of a GitHub PR.

1. `gh pr view $ARGUMENTS --json headRefName,baseRefName,headRefOid,state,mergedAt,body,files` and confirm the branch and that the PR is still open; create a worktree from that exact `headRefOid`.
2. Fetch ALL feedback: issue comments AND `gh api repos/{owner}/{repo}/pulls/$ARGUMENTS/reviews` + review comments.
3. For every claim in the PR body/release notes, verify it by running code. Print `pkg.__file__` to prove the right source is imported.
4. For each bug found, write a test that fails on the PR head.
5. Output findings ranked Blocker/Major/Minor with repro commands. Do NOT post or push until I approve.
6. Write any review text by the rules in `~/.claude/oss-contributions.md`: only defects, gaps and suggestions, no praise for correct parts, no restating resolved points.
7. Right before posting, re-run `gh pr view $ARGUMENTS --json headRefOid,state` and cite only that SHA. If it changed since step 1, tell me before posting.
8. After approval: edit-in-place for corrections, stage explicit paths only, then remove the worktree, confirm with `git worktree list`, and update the tracker with the `track` skill.
