# Margin summary lane

Investigated: 2026-11-05
Repository snapshot: `f158df41168bbf0b64bce01518fcff1bd88cb11f`

The probes ran on R 4.6.1 on aarch64 macOS with pkgload 1.5.3, dplyr
1.2.1, tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1 and
data.table 1.18.6.1. The backend exploration also used dtplyr 1.3.3,
Arrow 25.0.1, dbplyr 2.6.0, DBI 1.3.0, RSQLite 3.53.3 and DuckDB 1.5.5.

## First packed across resolved a numbered promise as a column name

A variadic wrapper forwarding x through a first named
`packed = dplyr::across(..1, sum)` produced a packed x of six through
dplyr. The Margin summary reported an unknown column named `..1`.
A preceding independent summary or enclosing `identity()` allowed the
same Margin call to return six. An unnamed direct across was a separate
expansion control and also failed in ordinary dplyr.

`preflight_local_selection()` resolved the first summary's selector when a
symbol was absent from both the data proxy and the captured environment's
bindings. A populated numbered dots promise was absent from that binding
test. The error preceded the `local_forwarded_selection()` path that
captured and resolved the promise successfully in both controls. ADRs 0007
and 0019 and *Relationship to dplyr summaries* in the Margin summary reference
established the ordinary local evaluation contract.

The replay compared five numbered spellings, two helper qualifiers, three
grains and three input classes. It reproduced 90 Margin-only refusals and
180 successful Margin controls. The all-state Issue and PR searches
included `"numbered dots" "across"`, `"first" "selector" "across"` and
`"first named" "numbered"`. Closed #1138 and merged #1149 covered
runtime forwarding inside inline calls, #1151 and #1172 covered unnamed
top-level expansion, #1211 and #1223 covered noncanonical spellings,
#1092 and #1131 covered extra reads of externally bound selectors, and
#701 and #708 covered packed selections beside unrelated shares.
The reproduced refusal occurred without a share and with a canonical
reference, before runtime forwarding.

Replay from the source root with `Rscript first-numbered-selector.R`.

## Namespace operators intercepted package-owned calls

With x = 1:3 and y = 10:12, the caller requested the sum of bare `pick(x)`.
A caller binding of the namespace operator `::` replaced only the generated
`base::quote` resolution. Ordinary dplyr returned six and never entered the
binding's matching branch. The Margin summary returned 33 and entered it
once. The same result followed from replacing only the generated
`tidyselect::all_of` lookup or intercepting the private local selector via
`:::`. A bare runtime pick in an inline function could be redirected via
the generated `dplyr::pick` lookup to return 99, and intercepting the
private nested-mask thunk via `:::` likewise returned 99. None of the
caller expressions contained a namespace-qualified call.

ADR 0019 established that Contextual helpers resolve by spelling, and
ordinary caller expressions retain their lookup. The observed redirections
only replaced lookups introduced by the Margin verb. Restoring the ordinary
operator without changing the caller expression restored the values.
The matrix reproduced 15 wrong Margin values and callback invocations,
with 15 ordinary and 15 unshadowed Margin comparisons.

All-state issue and PR searches paired `"namespace operator" "summary"`
and `"namespace" "shadow" "pick"`. #172 and PR #191 established static
recognition. #1263 and #1267 with PR #1289 protected against direct bindings
named `quote` and `function`. Their qualified calls were the new
operator-lookup sites exercised here. #1288 and PR #1297 addressed explicit
parentheses written in the selection, and PR #1299 addressed share
dependency analysis.

Replay from the source root with `Rscript namespace-operator.R`.

## An Assigned summary name was checked before the output existed

A function returned a one-row frame with column out = sum(columns).
`frame(dplyr::pick(x))` over x = 1:3 expanded into out = 6 in ordinary
dplyr and in the Margin control. Asking for an identifier named
`frame(dplyr::pick(x))` raised a summary-output collision. A fixed key
or Margin dimension with that same name raised a grouping-overwrite
condition. The output neither reused nor exposed that name.

`name_rewritten_summary_dots()` assigned a temporary name while
`execute_margin_summary()` treated it as a public output before
`local_assigned_summary_value()` could see the frame. ADR 0028 and
*Relationship to dplyr summaries* required the unnamed frame to expand
and the Assigned summary name to remain temporary. The output-column
and grouping-identifier contracts still required collisions with real
names to be refused.

The matrix established 18 false collisions over fixed keys, single
Grouping sets, rollups and public identifiers with all three Margin
order options, using data.frame, tibble and data.table inputs. Real scalar
and real expanded output collisions still refused; named packing still
worked. All-state Issue and PR searches included
`"assigned summary name" "collision"` and
`"assigned" "summary output" "conflict"`. Closed #435 and merged #442
established expansion and checking for real inner collisions. The
existing report did not concern a collision with the temporary name.

Replay from the source root with `Rscript assigned-name-collision.R`.

## Replay and limits

Run each script from the source root, passing the script's path to Rscript.
All three report modes exited zero and the three `--assert` modes exited
one on the intended assertions through terminal handle 66120. No package
implementation or regression tests were changed.

An exploratory sweep compared 46 ordinary summary expression shapes on each
of dtplyr, Arrow, SQLite and DuckDB. Among comparisons with a successful side,
the differing values and errors were confined to Arrow name handling
already covered by renamed-across repairs and the Margin preprocessing of
`.names` expressions. Two additional warning-count differences arose from
Arrow and dbplyr's rate-limited aggregation warnings. That sweep compared
one grouping grain and did not assert a portable SQL translation policy.
Eager omitted-`.cols` lifecycle warnings differed from ordinary dplyr
as recorded by the existing empty-argument tests. Paired refusals were not
treated as proof of equivalent diagnostics. Hosted backends, arbitrary caller
code, other dependency versions, and other platforms were outside this lane.
