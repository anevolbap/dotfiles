---
description: End-of-session check for a PR or issue session (tracking, cleanup, leftovers, safe to delete the session)
argument-hint: [PR or issue number]
---
# Wrap up

Answer four questions for this session: is the work tracked, is the cleanup done, is anything left over, can the session be deleted. Check live state for every answer. Do not answer from memory.

1. Find the target. Use `$ARGUMENTS` if given. Otherwise take the branch of the session's worktree (`git branch --show-current`) and look it up with `gh pr list --head <branch> --state all`, or search the tracker for the branch name. If there is still no target, ask.
2. Get live state: `gh pr view <N> --json state,mergedAt,isDraft,url,headRefName` (or `gh issue view <N> --json state,url` for an issue with no PR).
3. Look for leftovers. These checks only read, run them all:
   - `git -C <worktree> status --short` (uncommitted work)
   - `git -C <worktree> log --oneline @{u}..` (unpushed commits; no upstream means nothing is pushed)
   - `git stash list`
   - ignored scratch files in the worktree, other than `.venv` (`git status --short --ignored`). `git worktree remove` deletes them without a warning.
   - background jobs or monitors started in this session that are still running
   - public text drafted in this session and not posted yet
   - scratch files this session wrote under `~/tmp`
4. Clean up based on state:
   - MERGED: follow `/pr-cleanup <N>`. Stop if it stops.
   - OPEN or draft: no cleanup. The worktree stays, and `wt-gc` removes it after the merge.
   - CLOSED without merge: ask before you remove anything. The branch may hold work worth keeping.
   - Issue with no PR: remove the worktree only if it has no commits beyond its base. Otherwise keep it.
5. Track with the `track` skill. A finished entry gets its done keyword and a Done: line. An open entry gets a dated `:STATUS:` and a `NEXT:` line that says the next concrete action, so `/resume-entry` can continue without this session.
6. If the session fixed a problem that meets the CLAUDE.md "Problem notes" rule and no note exists yet, write it with `roam-problem` and give the path.
7. Run `git worktree list` again and report in this format, one line each:

```text
Target:   PR #N <state> <url>
Tracker:  <file> / <heading>, <keyword>, NEXT: <text or "closed">
Cleanup:  <what was removed, or "none needed (PR open)">
Leftovers: <list, or "none">
Delete session: yes|no, <reason>
```

Answer "yes" for delete session only when all of these hold: nothing is uncommitted, unpushed or stashed for this work, the tracker entry is closed or has a `NEXT:` line, no draft waits for approval, and no background job from this session is still running. Deleting the session loses only the chat context. The tracker entry and the branch are what `/resume-entry` uses.

Never push, delete remote branches or post anything from this command. Print those commands for me to run.
