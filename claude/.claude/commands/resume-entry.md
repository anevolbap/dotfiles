---
description: Resume tracked work from its projects.org entry, check it against the repo, then continue from NEXT
argument-hint: <issue number, PR number or title words>
---

1. Find the entry: `rg -n "$ARGUMENTS" ~/Documents/org/projects.org`. If more than one open entry matches, list them and stop.
2. Read the entry: summary, Pending (ticked and unticked), NEXT:, :BRANCH:, and the Context Done: log. Boxes Pablo ticked and Done: lines he added are work he did by hand. Do not redo it.
3. Check the repo, read-only: `git status`, `git log -5` on the branch, and any check the entry names. Note what changed since the last Done: line.
4. Report in at most five lines: where it stands, what changed since last time (including Pablo's manual steps), what you will do next. If the next step is Pablo's decision (approve, choose, publish), stop and ask.
5. Otherwise continue with NEXT:. Delegate implementation and experiment runs to the `build` subagent with the exact files, the command that proves the result, and a budget for any sampling. Check its result before you report.
6. Never push, post, comment, mention anyone or open a PR. Prepare it locally and add `- [ ] approve: <action>` to Pending.
7. Before ending, update the entry with the track skill: tick finished steps, add dated Done: lines with evidence, rewrite NEXT:.
