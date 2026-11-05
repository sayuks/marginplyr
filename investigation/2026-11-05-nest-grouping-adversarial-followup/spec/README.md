# Grouping specification product and lexical boundary hunt

Investigated: 2026-11-05
Tested package SHA: `0403c9150394cbae21b59f34c75791d0873b8451` (the coordinator's fetched main)

This track inspected constructors, structural preflight, retained arguments,
name-only classification, family algebra, duplicate policies, inspection
formats and identity fields. No package implementation or permanent tests
were changed. The November 5 nesting/specification hunt and November 4
boundary, greedy and follow-up investigations supplied prior counterexamples.
ADRs 0005, 0007, 0008, 0013 and 0026 supplied the relevant contracts.

## Confirmed: a multi-set suffix hid a guaranteed duplicate prefix

The product of `grouping_sets(a, a)` with a two-set union, whose first set
used a typed predicate and whose second set was empty, necessarily had equal
occurrences 1/3 and 2/4. Each suffix branch contributed exactly one grouping
set regardless of which columns the predicate selected. No type information
was needed to establish those pairs or their occurrence positions.

`non-singleton-suffix-boundaries.R` compared this case with a single-set typed
suffix and a two-set literal suffix. All three public consumers, inspection
and the two Nest verbs, called the predicate twice on the local frame before
reporting duplicates. On an unsafe derived dtplyr input all three substituted
the typed-metadata refusal for the guaranteed duplicate error. The source
callback was never called. DuckDB inspection issued one actual `dbSendQuery`
statement containing `LIMIT 0`; the Sent query view separately recorded one
`selection_proxy`. Both controls refused the duplicate before evaluating the
predicate or issuing metadata statements.

The final matrix had 21 cases: 14 matching controls and seven violations of
the early-refusal contract. Observation mode exited 0 after asserting those
dated results. `--assert` exited 1 requiring desired early refusal. The
earlier `non-singleton-suffix.R` was the initial local minimization; the final
boundary script retained the authoritative matrix, DBI trace and controls.

The ranked hypotheses before the separating run were: (1) incomplete product
evidence was limited to singleton suffixes; (2) selection-order policy required
the work; (3) another preflight layer already refused the duplicate. The
singleton control, fixed suffix cardinality and observed metadata read
supported the first. `observe_product_factor()` gated incomplete evidence on
`single_suffix()`. ADR 0005 required refusal of an error already established
from local metadata; this witness did not require reading or skipping a later
selector to establish it.

### Duplicate disposition

All-state Issue and PR searches included `"product" "suffix"`,
`"guaranteed" "product"` and `"duplicate" "multi"`; the saved JSON results
each had fewer than the requested 100 results. Full bodies and comments for
#1246, #1266 and #1305, and the related PR #1286, were read. #1246 concerned
completed leading union branches. #1266 explicitly specified a trailing
single-set factor. #1305 explicitly specified remaining factors contributing
one set each. PR #1286 repaired those singleton cases and resource behavior;
its bounded multi-set-tail control established reaching a caller's typed
error, rather than this known equal prefix and fixed two-set suffix. These
reports did not establish the multi-set suffix boundary observed here.

The witness was one independent repair slice with no blockers. It should
retain leading selection error order, defer cases without a proven duplicate,
and preserve occurrence numbering, drop/keep semantics and evaluation budgets.

## Refuted and negative findings

`deep-family-algebra.R` extended the November 4 independent family oracle to
1,500 deterministic generated trees, at most six constructor levels deep,
with no more than 64 pre-deduplication occurrences each. There were 300 roots
of each of the five constructor kinds. Names, `all_of()`, positions, `c()`,
empty sets, identity products, overlaps, repeated units and nested union/product
composition were varied. All 9,000 comparisons matched: three duplicate
policies, with and without fixed keys, on every tree. Successful plans were
compared as complete ordinary inspection tibbles, including dimension order,
omitted/fixed columns, named Grouping bits, occurrence IDs and Grouping IDs;
duplicate error controls required the Package condition and stable message.
This independent oracle did not call the package's expansion/mask helpers.

The lexical inspection found conservative handling of deferred and active
bindings, helper-argument dots, caller closures, binary subtraction,
parenthesized/qualified function positions and retained constructor answers
in the inspected paths. No additional lexical defect was established. This
code reading was not counted as executed coverage. The oracle established
finite algebra correctness, not early metadata timing, so it did not negate
the positive finding above.

## Known resource frontier reproduced without a new bug claim

The review record on PR #1250 had explicitly left a larger product case
outside the concrete #1245 acceptance cases: 20 independent optional columns
crossed with a family whose members each omit a different one of those columns.
The final family had only the complete set and 20 sets each missing one column.
No column was mandatory across the suffix, so mandatory-column compaction did
not identify those prefixes as equivalent.

`optional-prefix-small-output.R` reproduced that recorded frontier on the
investigated SHA with `R_MAX_VSIZE=96M`. Its literal final-family control matched
21 independently specified sets and occurrence identifiers. The product then
raised `vector memory limit of 96.0 Mb reached, see mem.maxVSize()`. At width
8 under the same cap the product matched the entire nine-row literal inspection
tibble. The capped width-20 observation process (session `59307`) completed at
exit 0 after capturing that condition; the width-8 control completed at exit 0.

The full bodies/comments of #1245 and #1274 were read. Their acceptance cases
covered repeated units, repeated families and universally required columns.
Neither those tickets nor the public interface supplied a general bounded-heap
complexity promise for all products. The existing PR review recorded this
specific frontier rather than deciding a new public guarantee. It was retained
as a known performance limitation and possible enhancement; this track did
not classify it as a newly discovered correctness defect or recommend a
duplicate bug ticket.

```sh
R_MAX_VSIZE=96M Rscript /path/to/optional-prefix-small-output.R 8
R_MAX_VSIZE=96M Rscript /path/to/optional-prefix-small-output.R 20
```

## Replay and limits

Run from the investigated package root:

```sh
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript /path/to/non-singleton-suffix-boundaries.R
Rscript /path/to/non-singleton-suffix-boundaries.R --assert
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript /path/to/deep-family-algebra.R
```

The final boundary observation execution used terminal session `80374` and
completed at exit 0. The desired-behavior assertion completed at exit 1.
The deep oracle used terminal session `66878` and completed at exit 0.
All handles were collected before another reproduction began. The probes
used R 4.6.1 on aarch64-apple-darwin23, dplyr 1.2.1, dtplyr 1.3.3,
DuckDB 1.5.5, tidyselect 1.2.1 and rlang 1.3.0. The logs recorded versions.
The DBI tracer was removed and the DuckDB connection closed on exit.

This track did not run a complete suite, package build, installation or site
render. Hosted databases, other R/library/OS versions, unbounded algebra and
arbitrary caller dispatch remained outside its finite evidence. No fix or
post-fix result was claimed.
