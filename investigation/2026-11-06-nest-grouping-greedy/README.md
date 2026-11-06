# Greedy nesting and Grouping specification investigation

Investigated: 2026-11-06
Tested package SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`
Environment: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2

Four investigators examined `nest_with_margins()`, `nest_by_with_margins()`,
the five Grouping specification constructors and `inspect_grouping()` in a
new worktree from main. Fetching origin/main at the start and again before
Issue publication resolved the tested SHA above. The November 5
[adversarial follow-up] and [boundary hunt], and the November 4
[nesting follow-up], supplied known failures, completed repairs and controls.

## Findings and tickets

The user delegated ticket granularity and publication. Two independently
verifiable repairs were published through `to-tickets`, each with `bug` and
`ready-for-agent`, acceptance criteria and no blocking edges. Only dated
investigation evidence was changed; no implementation or permanent regression
test was added.

| Issue | Observed violation | Evidence |
| --- | --- | --- |
| [#1359] | A caller-local bracket S3 method changed derived dtplyr zero-row metadata, silently removing a real Grouping dimension or fixed key. | [Backend finding and controls](backend/README.md) |
| [#1360] | An accepted logical scalar storing TRUE disabled observed Margin label collision checking through S3 negation dispatch. | [Local finding and controls](local/README.md) |

### Bracket method lookup changed the grouping grain

An immutable dtplyr input contained two character keys, a and b, and an
integer payload incremented by an arithmetic mutation. The caller's bracket
method delegated ordinarily for populated input and changed the key's type
only on zero rows. Native collection kept the character keys. Typed
inspection selected no key, and both Nest verbs returned one keyless cell
instead of two keyed cells. Typed fixed-key selection failed at the same
boundary. Removing the method restored the result; neither the source nor
the global S3 registry changed.

The 54-case matrix separated six failures from 48 matching controls.
The repaired bracket-generic lookup in #1326/#1333 did not check this S3
method. The full-join repair in #1341/#1352 checked a different generated
entry point. ADR 0020's dtplyr metadata amendment required a faithful schema
or safe refusal. The witness established false partitioning; it did not
establish an unrequested read of populated source rows.

### Logical admission and execution disagreed

A logical scalar stored TRUE and passed admission, but its negation method
returned TRUE as well. On a one-row source whose dimension already equaled
the default Margin label, both Nest verbs, expansion and summary returned
the same two indistinguishable rows as plain FALSE. Plain TRUE reported the
collision. Named, matrix-shaped and AsIs TRUE controls retained the check,
and the custom scalar still retained keys when used as `.keep`.

The reverse constant-negation FALSE scalar was refused during admission;
no extra lazy read was inferred. This was a narrower logical-dispatch
boundary than #1309's repaired character Option argument conversion. Its
unusual class-specific trigger was reflected in the P3 priority.

## Completed finite coverage

| Boundary | Dated result |
| --- | --- |
| [Local multikey oracle](local/README.md) | 3,024 calls matched source membership, cell projections, column order and an independent Margin-order key across twelve key families, both Nest verbs, keep choices and sort choices. |
| [Grouping specification oracle](spec/README.md) | 1,588 comparisons matched: 1,500 generated-family comparisons, 78 width/identifier comparisons and ten empty-selection comparisons. |
| [Backend dispatch matrix](backend/bracket-method-matrix.csv) | Six failures and 48 matching controls; native collection, source immutability and unchanged global registration were asserted. |
| [Branch sharing](backend/source-reuse-control.txt) | Four existing controls passed: both Nest verbs with uncached/caller-cached volatile input evaluated it once at execution, with matching detail and total membership. |
| [Construction reentry](coordinator/README.md) | Sixty entry-argument cases and five post-SQL cases preserved the parent's record and saved audit setting. |

The local oracle grouped original typed keys with vctrs and checked cell row
identifiers against source projections. The independent specification oracle
calculated singleton, union, rollup, cube and product families without package
expansion, deduplication or mask helpers. It included non-syntactic/Unicode
names and the 31/32-variable-dimension Grouping identifier boundary. Successful
algebra did not establish metadata timing, which the backend finding measured
separately.

## Independent confirmation and duplicate disposition

The coordinator extracted an independent Git archive of the tested SHA.
The backend desired-behavior replay retained terminal session `92606` and
exited 1 after its native and unmasked controls passed. The logical replay
retained session `27930` and exited 0 after observing all four mismatches
and controls; its separate desired-behavior invocation exited 1. The adjacent
[backend assertion](coordinator/bracket-assert.txt),
[logical observations](coordinator/logical-observed.txt) and
[logical assertion](coordinator/logical-assert.txt) captured those outcomes.
All execution handles reached terminal completion.

The track notes recorded the all-state Issue/PR search queries and adjacent
report dispositions. The backend track also retained retrieved JSON records.
The bracket investigation distinguished the earlier generic and merge
repairs. The logical investigation distinguished #1309, #109, #371 and
#516. Fetching main immediately before publication returned the unchanged
tested SHA. No parent issue was modified or closed.

## Replay and limits

Run from the tested source root; track READMEs give output-location options
and individual matrix commands. The two `--assert` commands intentionally
required desired behavior and exited 1 on the investigated source.

```sh
evidence=investigation/2026-11-06-nest-grouping-greedy
Rscript "$evidence/backend/bracket-method.R" --assert
Rscript "$evidence/local/logical-check-dispatch.R" --assert
Rscript "$evidence/local/multikey-oracle.R"
Rscript "$evidence/local/multikey-oracle.R" --mixed
Rscript "$evidence/spec/family-boundaries.R"
Rscript "$evidence/coordinator/reentry.R"
```

The finite investigation did not establish behavior for arbitrary classes,
all possible expression/query graphs, hosted backends, other operating
systems or package versions, unbounded resource use, or interruption. No fix
or post-fix success was claimed. Detailed environments accompanied the track
results. The new evidence used the existing investigation exclusion; package
code, tests, helpers, generated documentation, installed files, dependencies,
build exclusions and check machinery retained their baseline bytes. The
scope decision, pinned submitted revision, Review-ready outcome and main
compatibility assessment belonged to the PR verification record.

[adversarial follow-up]: ../2026-11-05-nest-grouping-adversarial-followup/README.md
[boundary hunt]: ../2026-11-05-nesting-grouping-spec-hunt/README.md
[nesting follow-up]: ../2026-11-04-nesting-grouping-followup/README.md
[#1359]: https://github.com/sayuks/marginplyr/issues/1359
[#1360]: https://github.com/sayuks/marginplyr/issues/1360
