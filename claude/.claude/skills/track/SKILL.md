---
name: track
description: Track work in Pablo's org files so it shows in his Emacs agenda ("p" Pick up view). Use when he says "track this", "add a TODO", "add to my agenda", "remind me", when a piece of work starts, changes state, stops halfway or ends, and before ending a session that made progress on tracked work.
---

# Track work in org

The goal: when Pablo opens the agenda and presses `p` (Pick up), he sees every open item with the next action to take. The `p` view lists open TODO entries (any not-done keyword) in `~/Documents/org/*.org` that have a `NEXT:` line in their body, grouped by parent heading, and shows the last `NEXT:` line under each entry.

So every tracked entry needs an open TODO keyword and a `NEXT:` line. An entry without `NEXT:` is invisible in `p`.

## Pick the file

- Work tied to a repo, PR or issue (open source or personal code): `~/Documents/org/projects.org`, under the project's top-level heading (`* CausalPy`, `* dotfiles`, ...). Create the project heading if it is missing.
- Anything else (a loose task, something to read, try or check later): `~/Documents/org/tasks.org`, as a child of `* Tasks to be reviewed`, appended at the end.
- Do not write to `claude-sessions.org`. It is generated and rewritten wholesale.

Before adding, search for an existing entry (`rg -n '<issue number|title words>' ~/Documents/org/projects.org ~/Documents/org/tasks.org`). Update it instead of adding a duplicate.

## Keywords

- `projects.org` (`#+TODO: TODO ONGOING PAUSED | DONE MERGED CANCELLED`): TODO not started, ONGOING in progress, PAUSED parked. Finish as MERGED (PR merged or issue closed) or DONE (personal work), CANCELLED if dropped.
- `tasks.org` (`#+SEQ_TODO: TODO ONGOING | REVIEW DONE`): TODO or ONGOING, finish as DONE. REVIEW is a done state that means "closed, but the close should be checked": use it when the evidence is not in front of you. The triage run below turns it into DONE or reopens it.

## Entry format

projects.org, for a PR or issue:

```org
** ONGOING [[https://github.com/O/R/pull/N][PR #N]] - Title :tag1:tag2:
:PROPERTIES:
:PR: https://github.com/O/R/pull/N
:BRANCH: fix/N-short-name
:SESSION: <session id>
:STATUS: draft PR open [YYYY-MM-DD], waiting on review
:END:
One or two sentences: what this is and where it stands.

Done:
- what was checked, decided or done, with evidence (=command= or file:line)

Pending:
- [ ] step left to do
- [X] step finished since the entry was written
NEXT: one concrete next action.
```

Use `[[url][Issue #N]]` and an `:ISSUE:` property for issues. Personal work with no URL uses a plain `** STATE Title :tags:`. Bullets that reference issues use `[[url][#N]]`.

tasks.org:

```org
** TODO Short title                                                   :tag:
:PROPERTIES:
:CAPTURED: [YYYY-MM-DD Day HH:MM]
:URL: https://... (only if there is one)
:END:

One or two sentences: what this is and why it matters.

Done:
- fact with evidence

Pending:
- [ ] step left to do
NEXT: one concrete next action.
```

Body structure, for both files: summary first, then the `Done:` bullets, then the `Pending:` checklist, then `NEXT:` as the last line. Leave out a group that would be empty. Each bullet is one unwrapped line with one fact or step. No paragraph runs longer than two sentences, and details go in bullets. When a pending step is finished, tick it (`[X]`) and do not delete it. When you update an entry whose body is still a wall of text, rewrite that entry's body into this structure, keeping every fact.

Get the timestamp with `date '+%Y-%m-%d %a %H:%M'`. Tags cannot contain hyphens.

## Rules

1. Verify before writing. For a PR or issue, run `gh pr view <N> --repo O/R --json state,mergedAt` or `gh issue view <N> --repo O/R --json state,closedAt` so the entry matches GitHub.
2. Set `:SESSION:` to `$CLAUDE_CODE_SESSION_ID` on meaningful progress.
3. Keep exactly one live next action: the last `NEXT:` line in the body. Replace the old `NEXT:` line instead of stacking new ones. Write it as an action Pablo can start cold ("check #963 for a reply, then ..."), not a summary of what happened.
4. Dates: if Pablo gives a date ("tomorrow", "Friday", "next week"), add a `SCHEDULED: <YYYY-MM-DD Day>` line right below the heading so it also shows in the daily agenda. A hard deadline uses `DEADLINE:`. No date given, no timestamp.
5. When work ends: flip the keyword (MERGED with `:STATUS: merged [YYYY-MM-DD]`, `closed [YYYY-MM-DD]` for an issue closed externally, DONE for personal work), and replace the `NEXT:` line with a `Done:` bullet that holds the closing evidence, so the entry leaves `p`.
6. Body follows the structure above. Lines are unwrapped. Plain English, no em-dashes.
7. Edit the files with the Edit tool. Do not reformat or touch other entries.

## Check

After writing, confirm the entry would show in `p`: it has an open keyword and a `NEXT:` line, e.g. `rg -n -A12 '<title words>' <file> | rg 'NEXT:'`. Tell Pablo the file and heading in one line.

## Triage REVIEW entries

Runs when Pablo asks ("triage review entries") and every night from `org-review-triage.timer`. This mode only proposes. It writes `~/Documents/org/review-triage.org` and nothing else. Do not edit `tasks.org` or `projects.org` here.

1. Collect every `REVIEW` entry in `~/Documents/org/tasks.org`.
2. For each one, test the claim in its body with a read-only check: `gh pr view` / `gh issue view` for a PR or issue, Grep in the repo for a code or config change, `claude mcp list` for MCP servers, `systemctl --user status` for units. Then pick one verdict:
   - DONE: the check confirms the claim, the body has an evidence line, and there is no `NEXT:` line.
   - REOPEN: the check fails or work is left. Propose `TODO` and write the `NEXT:` line.
   - UNSURE: no available check can test the claim. Say what Pablo should look at.
3. Hygiene sweep over open entries in `tasks.org` and `projects.org`:
   - open keyword with no `NEXT:` line (invisible in `p`). Skip entries under `* Reading list` and entries tagged `read` or `video`: they live in the `l` view and need no `NEXT:`.
   - something that looks like a secret (token, API key, password). Give heading and line number only, never copy the value.
   - two entries that look like the same work
4. Overwrite the report. Heading text in the report must not start with a TODO keyword, except the top entry:

```org
#+TITLE: REVIEW triage
* TODO Review triage [YYYY-MM-DD Day]
NEXT: read the proposals in review-triage.org and say which to apply ("apply review triage").
** <entry title> (tasks.org:<line>) -> DONE | REOPEN | UNSURE
One sentence: the claim and the verdict.
- Check: =<command>= showed <result>
- Change: <exact change: keyword, CLOSED stamp, Done: bullet, or the new NEXT: line>
- [ ] for UNSURE: what Pablo should look at
** Hygiene
*** Missing NEXT:
- tasks.org:<line> <heading>
*** Possible duplicates
- tasks.org:<line> and tasks.org:<line>: <why>
*** Possible secrets
- tasks.org:<line> <heading>
```

   One fact per bullet, no paragraphs. Leave out a hygiene group that has no findings. Collapse long lists of the same kind (for example many reading links with no `NEXT:`) into one bullet with a count and line numbers.

   If there are no REVIEW entries and no hygiene findings, write the top entry as `* DONE Review triage [date]` with no `NEXT:` line, so nothing shows in `p`.

Applying: when Pablo approves some or all proposals, make those changes with the Edit tool (DONE gets a `CLOSED: [YYYY-MM-DD Day HH:MM]` line under the heading and a `Done:` bullet with the evidence in place of `NEXT:`; REOPEN gets the open keyword and the `NEXT:` line). Then set the report's top entry to DONE and remove its `NEXT:` line.
