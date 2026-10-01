# Hostile Review

Adversarial, empirically verified review of a GitHub PR.

1. `gh pr view $ARGUMENTS --json headRefName,baseRefName,body,files` and confirm the branch; create a worktree from that exact head.
2. Fetch ALL feedback: issue comments AND `gh api repos/{owner}/{repo}/pulls/$ARGUMENTS/reviews` + review comments.
3. For every claim in the PR body/release notes, verify it by running code. Print `pkg.__file__` to prove the right source is imported.
4. For each bug found, write a test that fails on the PR head.
5. Output findings ranked Blocker/Major/Minor with repro commands. Do NOT post or push until I approve.
6. After approval: edit-in-place for corrections, stage explicit paths only, then remove the worktree and update projects.org.
