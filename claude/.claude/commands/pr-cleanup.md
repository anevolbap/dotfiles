---
description: Clean up after a merged PR (worktree, local branch, tracker), checking live state first
argument-hint: <PR number>
---
# PR Cleanup

1. `gh pr view $ARGUMENTS --json state,mergedAt,headRefName`. If `state` is not MERGED, stop and report. Do not trust memory or the tracker.
2. `git worktree list`. For each worktree on `headRefName`: check `git -C <path> status --short`. If it has uncommitted changes, stop and ask. Otherwise `git worktree remove <path>`, then `git worktree prune`.
3. `git branch -d <headRefName>`. If git refuses (squash merge), confirm the PR is merged from step 1, then use `-D`.
4. Print the remote delete command for me to run, do not run it: `git push origin --delete <headRefName>`.
5. Update the tracker entry with the `track` skill: flip it to MERGED with `:STATUS: merged [<mergedAt date>]` and a Done: line with the PR link.
6. Run `git worktree list` again and report in one line that nothing for this branch is left.
