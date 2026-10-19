---
description: Check whether a GitHub issue is worth picking up (claimed status, feasibility, verified claims), then stop before implementation
argument-hint: <owner/repo#NUM, or NUM>
---
# Assess GitHub Issue

Assess whether a GitHub issue is worth picking up. Stop before implementation.

Argument: `owner/repo#NUM` or just `NUM` if the issue lives in the current repo's origin.

Steps:
1. Fetch the issue: `gh issue view <NUM> --repo <owner>/<repo>`.
2. Suggest that I rename the session to label its focus: `/rename issue-<NUM>`.
3. Check for existing PRs that reference it: `gh pr list --repo <owner>/<repo> --search "<NUM>" --state all`. Also scan the issue's linked PRs, timeline, and assignees for partial work or claimed status.
4. If claimed or in progress, stop and report. Do not proceed to step 5.
5. Explore the codebase to confirm feasibility. Use `rg` over the relevant paths. Read only the regions you need.
6. Verify any factual claim in the issue (bug, docstring error, behavior) against the current source. Do not trust the report at face value. If the claim depends on a release or a diff, check against the true upstream base (`upstream/main`), not a stale fork `origin/main`. Run `git fetch upstream` first.
7. Produce a structured assessment in prose:
   - One line: what the issue asks for.
   - Scope: which files/functions are involved.
   - Approach: minimal fix, or the alternatives if more than one. Weigh them honestly, do not undersell options.
   - Risks: what could break.
   - Effort: trivial / small / medium / large per the planning bucket in `~/CLAUDE.md`.
8. Stop. Do not start implementation until I confirm. If you draft a comment for the issue, follow the communication rules in `~/.claude/oss-contributions.md`: terse, question-oriented, no preamble, no mention of unrelated PRs, draft-first for my approval.
9. After I confirm, track the issue with the `track` skill: a `TODO` entry in `~/Documents/org/projects.org` under the project heading, with a `NEXT:` line.
