# Duplicate and freshness checks

Investigated: 2026-11-07
Tested main: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

`gh issue list --state all --limit 2000` and `gh pr list --state all --limit
2000` exported 711 Issues and 699 PRs, including their titles and bodies, before
publication. The issue export also included comments and labels. Exports were
kept outside the submitting worktree. A local case-insensitive search found no
body naming `as.data.table.dtplyr_step`, one Issue matching registered/bracket
in either order (#1397), and one matching metadata/converter (#947).

Live `gh api --method GET search/issues` queries included both Issues and PRs
in all states:

| Query after `repo:sayuks/marginplyr` | Matches | Relevant distinction |
| --- | ---: | --- |
| `"registered" "bracket"` | 3 | #1397 protected head; PR #1333 protected lexical head/bracket; PR #1379 concerned Grouping pattern dispatch. |
| `"root" "bracket"` | 7 | #1359, #1382, #1397 and their related PRs described derived bracket or root head/subclass admission. |
| `"metadata" "S3"` | 21 | Existing pattern, head and derived bracket reports; no converter-registration report. |
| `"as.data.table" "registered"` | 1 | #184 was a zero-column cardinality defect. |
| `"converter" "metadata"` | 4 | #947/PR #959 concerned a standard converter rejecting `.SD`; the other matches were artifact/cell-constructor work. |

The coordinator read complete bodies and comments for #1359, #1397, #1396,
#1360, #947 and #184, and PRs #1333 and #1371. The backend lane recorded its
additional related-report reads. The Grouping lane read #1288 and #897, and
compared the private-parenthesis candidate against pre-#1396 main before
declining another report. No parent Issue was changed.

The publication-time fetch retained main at the tested SHA. `git merge-tree
--write-tree FETCH_HEAD HEAD` exited zero with tree
`fbb0f613176867ab32bdca9518dc8a3850ff3914`; HEAD still equaled main before
research files were added. A separate Git archive of this SHA supplied the
coordinator replay. The two independent tickets were created as
[#1418](https://github.com/sayuks/marginplyr/issues/1418) and
[#1419](https://github.com/sayuks/marginplyr/issues/1419), then fetched to verify
body, title and `bug` / `ready-for-agent` labels. Neither had a blocking edge.
