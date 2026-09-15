---
description: Address review feedback on one of my open PRs, from fetch to posted reply
argument-hint: <PR number, or review/comment URL>
---
# Address PR Review

Handle review feedback on one of my PRs. Read `~/.claude/oss-contributions.md` first.

Steps:
1. Fetch narrowly. Never dump full JSON (user objects and links waste context).
   - Review body: `gh api repos/<o>/<r>/pulls/<N>/reviews/<ID> --jq '{user: .user.login, state, body}'`. Without a review ID, list them with `--jq '.[] | {id, user: .user.login, state}'`.
   - Inline comments: `gh api repos/<o>/<r>/pulls/<N>/comments --paginate --jq '.[] | {id, path, line, user: .user.login, body}'`.
   - Timeline: `gh pr view <N> --repo <o>/<r> --json comments,reviews,commits --jq '{c: [.comments[].author.login], r: [.reviews[] | {a: .author.login, s: .state}], head: .commits[-1].oid[0:8]}'`.
2. Tracker: print only this PR's entry: `awk '/pull\/<N>\]/{f=1} f&&/^\*\* /&&!/pull\/<N>\]/{exit} f' ~/Documents/org/projects.org`. Read the NEXT line and any decision that says what to stay out of.
3. Worktree: if `git worktree list` shows the PR branch, enter it with `EnterWorktree` and `path`. Otherwise create one. Confirm `git branch --show-current` and that `python -c 'import pkg; print(pkg.__file__)'` points at the worktree.
4. Decide whether to plan:
   - The reviewer gave tested code or a clear design choice, and I already asked for it: implement directly.
   - Open design choice, or the change grows past the PR's scope: write a short plan with the options and stop.
5. Before editing, record behavior: a small script under `$CLAUDE_JOB_DIR/tmp` or `~/tmp` that prints the result for every affected input (for example an operator or argument matrix). Run it before and after, then `diff`. Every changed row must be either intended or reported.
6. Tests: add tests for each changed row. Swap the old file back in (`cp` to tmp, `git checkout HEAD -- <file>`, run, `cp` back) and show the new tests fail. Then run the full test file with no `-k` filter, plus `pre-commit run --files`.
7. Commit with plain, separate git commands. In a worktree session, run every git and gh command on its own, never chained with `&&`, `;` or `cd` (the worktree guard rejects them). One commit per reviewer point. Conventional commits, no AI attribution.
8. Drafts: the new PR title/body (only if scope changed) and the reply, in quoted blocks. Next to the drafts, list each factual claim with its proof command or `file:line`. Stop for approval.
9. After approval:
   1. Recheck the timeline (step 1) so nothing is duplicated.
   2. `git push origin <branch>`.
   3. Title/body with `gh api -X PATCH repos/<o>/<r>/pulls/<N> -f title=... -F body=@file` (`gh pr edit` fails on the Projects classic deprecation).
   4. Reply with `gh api repos/<o>/<r>/issues/<N>/comments -F body=@file`, or answer an inline thread with `gh api repos/<o>/<r>/pulls/<N>/comments/<ID>/replies -F body=@file`.
   5. Update the tracker entry: heading title, STATUS, SESSION, and one NEXT line (usually: check CI with `gh pr checks <N>`, then wait for re-review).
