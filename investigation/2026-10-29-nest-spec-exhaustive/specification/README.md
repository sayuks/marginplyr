# Grouping specification algebra and reentrant selection observations

Investigated: 2026-10-29
Baseline: `ee4321789ca60b6259c53df2852f1574aaede584` (`main`)

This track examined the five Grouping specification constructors and
`inspect_grouping()`, with both nesting verbs as selection consumers. It
established no new reproducible defect within the finite probes below. No
package implementation, permanent test, existing investigation note, or
configuration was changed.

The readings included `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, the investigation
note convention, ADRs 0002, 0005, 0007, 0008, 0013, and 0026, the current
Grouping compiler and selection-helper tests, and the 2026-10-28 nest/spec,
2026-10-29 nesting/Grouping, and 2026-10-29 selector-dispatch investigation
records. The latter record's #995 implicit-dispatch boundary informed the
remaining-dispatch probe; those already reported symptoms were not treated as
new findings.

## Independent selection and specification algebra

`selection-algebra.R` used seed `291029701` and generated 5,000 trees from all
five kinds, with three recursive descent levels and optional terminal composite
dimensions. Fifteen selection shapes included names, selected
column order, a numeric position vector, range, negation, intersection,
set difference, `all_of()`, `where(is.character)`, a name pattern, and empty
selections. A separate recursive oracle expanded each tree from direct typed
tidyselect answers; it did not call the package's expansion or plan helpers.
The oracle represented a composite Grouping set as one unit and an ordinary
multi-column rollup/cube selection as separate units.

The observed counts were:

- 4,607 legal trees matched the oracle's included and omitted columns,
  Grouping bits, Grouping set identifiers, and retained occurrence order.
  Dropping duplicates also matched the independently deduplicated family.
- 389 trees resolved an empty rollup/cube unit or composite and received a
  Package condition as expected.
- Four trees whose oracle expansion exceeded 2,048 grouping-set occurrences
  were excluded by the probe's size bound.

The harness initially exposed two errors in its own representation of a NULL
leaf and an all-empty family; those oracle representations were corrected
before this final recorded run. Neither observation established a package
failure. The final log reported the complete 5,000-attempt run.

## Reentrant selection context and options

`reentrant-selections.R` compared each outer call with an explicit
`all_of("region")` control. The matrix crossed local/dtplyr input, fixed-key
inspection, inspection through each of the five constructors, both nesting
verbs, helper/`where()` entry, and inner inspection/ordinary nesting/rowwise
nesting. Each inner action either succeeded or raised an error about a missing
column that the callback caught.

All 192 public comparisons matched the control's plan or nested cells. The
384 callback invocations made 768 successful assertions that
`tidyselect::peek_data()` and `peek_vars()` returned to their original values
after the inner call. The callbacks ran once for a helper or once per column
for `where()`, as expected. `lifecycle_verbosity`, `warn`, and
`rlang_backtrace_on_error` had their original values after each outer call.
This measured restoration following nested success and caught failure; it did
not establish behavior for every possible condition handler or interruption.

## Remaining implicit-dispatch candidates

`remaining-dispatch.R` installed a temporary implicit-class S3 method for each
operation/class pair, invoked the ordinary operation, then removed that
method. The nine values were an unclassed matrix, array, integer, numeric,
logical, character, raw, complex, and list. Twenty-seven operations remaining
in the helper analyzer's safe operation vocabulary produced 243 observations
with no injected-method dispatch. Three direct invocations raised ordinary
operation errors; these were dispatch measurements, not claims that all
invocations were successful selections.

The same mechanism was calibrated against `format()`, `unique()`, and `sort()`:
all 27 positive controls invoked the supplied implicit-class method exactly
once. The absence of dispatch in the 243 tested combinations therefore did
not arise from a mechanism that could not observe dispatch. The sweep did not
cover every safe operation, every possible argument list, explicit classes,
modified base functions, or arbitrary custom methods.

## Replay and limits

Run from the investigated repository root:

```sh
Rscript investigation/2026-10-29-nest-spec-exhaustive/specification/selection-algebra.R
Rscript investigation/2026-10-29-nest-spec-exhaustive/specification/reentrant-selections.R
Rscript investigation/2026-10-29-nest-spec-exhaustive/specification/remaining-dispatch.R
```

Each script loaded that checkout with `pkgload::load_all()` and exited zero in
the retained final run. Its assertions would fail on the respective measured
mismatch, but no new red public-call reproduction was established. The logs
were finite negative evidence, not evidence that a previously reported bug
was repaired or that the interfaces were free of other bugs. Resource stress,
unbounded specification depth, other R/dependency versions, other operating
systems, and live SQL services were outside these probes. Backend preparation
and local source-membership tracks recorded their separate observations
elsewhere in the same investigation bundle.
