# dtplyr metadata conversion dispatch boundaries

Investigated: 2026-11-07
Tested source: `e1dabaabb392a3791177e5c48e983f780c7c93ec` (main at hunt start).

## Finding 1: an ordinary root admitted a replaced registered bracket

The [reproduction](registered-root-bracket.R) changed only the base S3
registration for `[.data.table`. Its replacement delegated to the ordinary
data.table method, then changed character `g` to integer only when the result
had zero rows. The ordinary root class vector and registered `head.data.table`
method were unchanged. Native `dplyr::collect()` retained `g = c("a", "b")`
and did not invoke the replacement. Typed inspection of
`grouping_set(where(is.character))` nevertheless returned `included = "()"`.
Both Nest verbs consequently returned one keyless cell instead of two cells
keyed by `g`. Typed fixed-key selection through `.by` had the same defect.

The unmasked typed control selected `g`; a name-only inspection selected `g`
without dispatching a replacement that would have stopped if invoked. The
observation run exited 0. Its `--assert` run demanded correct metadata or a
metadata-safety Package condition, and exited 1 at the desired-behavior
assertion. [Observation](root-bracket.log), [assertion](root-bracket-assert.log).

### Ranked hypotheses and observations

1. Root metadata admission omitted a registered bracket used by the conversion
   adapter. Prediction: changing that registration alone would affect typed
   inspection but not native collection or name-only inspection. All three
   observations matched that prediction.
2. Native dtplyr execution already returned an integer key. Prediction: native
   collection would lose character `g`. It retained both character values,
   refuting this explanation.
3. The typed selector independently selected the wrong key. Prediction:
   restoring the registration would leave the failure intact. The unmasked
   control selected `g`, refuting this explanation.

After the red-capable reproduction, callback-stack inspection located the
dispatch at `isolated_dtplyr_proxy()` -> the registered
`as.data.table.dtplyr_step()` -> `dt_eval(x)[]` -> `[.data.table`. It was the
conversion method's otherwise invisible trailing bracket, rather than the
root's initial head call. Root admission checked the ordinary class vector and
registered head method, while the bracket-registration check applied to derived
bracket-generating steps. Grouped and cached aliases also reached the root
path. Filter and arithmetic graph controls refused the replaced registration
before dispatch.

The [60-case matrix](root-bracket-matrix.csv) crossed replaced/ordinary
registrations, root/grouped/cached/filter/arithmetic graphs, typed dimension or
fixed-key selection, and inspection/both Nest verbs. It observed 27 correct
results, 15 false metadata/partition results, 12 metadata-safety refusals and
six structural refusals. The six structural refusals were grouped inputs with
an explicitly supplied `.by`, as required by that separate contract; three
occurred without a replacement and three with one. They were not counted as
metadata refusals. [Matrix output](root-bracket-matrix.log).

Each case asserted the native character values and serialized source bytes.
The minimal replacement received only a zero-row object. No populated-source
exposure or source mutation was observed for that replacement.

## Finding 2: typed metadata invoked an unchecked registered converter

The separate [reproduction](registered-converter.R) left bracket and head
registrations ordinary and replaced only `as.data.table.dtplyr_step` in the
data.table generic's registration table. The replacement delegated to the
original converter and changed `g` only in its empty output. Native collection
retained the original character values and invoked the converter zero times.
Typed inspection instead called the replacement once and lost the character
dimension. Both Nest verbs returned the same false partitions; typed fixed-key
selection also failed.

The observation exited 0; `--assert` exited 1 at its desired-behavior assertion.
[Observation](converter.log), [assertion](converter-assert.log).

### Ranked hypotheses and observations

1. Metadata acquisition trusted a mutable converter registration without
   checking it. Prediction: changing only that registration would change typed
   metadata while native collection retained its own result. Both occurred.
2. The bracket-registration defect above was required. Prediction: ordinary
   bracket/head registrations would prevent the failure. Both were ordinary
   throughout this witness, refuting that explanation.
3. The source or selector independently changed type. Prediction: the unmasked
   converter control would also lose `g`. It selected `g`, and all native
   collections retained the character values, refuting this explanation.

Source inspection located the independent boundary in
`isolated_dtplyr_proxy()`: it retrieved the converter through
`utils::getS3method()` and called it directly. No converter identity or
registration-safety predicate preceded that retrieval. This path applied to
admitted derived graphs as well as roots.

The [48-case matrix](converter-matrix.csv) crossed replaced/ordinary
registrations, root/cached/filter/arithmetic graphs, typed dimension or fixed-key
selection, and inspection/both Nest verbs. All 24 ordinary-registration cases
were correct; all 24 replacement cases returned false metadata or partitions.
Every case retained the native character values and serialized source bytes.
[Matrix output](converter-matrix.log).

## Shared registration frontier and contract

The [six-case registry probe](registry-frontier.R) varied absent, active and
unknown lazy registrations for each entry point. Name-only inspection looked
up none of them. Typed root inspection forced every active/lazy binding once,
and each callback read the original source's `v` values and recorded sum `3`
before returning the ordinary method. Typed inspection still returned `(g)`.
Thus a faithful final schema alone did not establish absence of unrequested
source work. No source bytes changed in these controls.
[Observations](registry-frontier.csv), [output](registry-frontier.log).

An absent converter produced the metadata-safety Package condition. An absent
bracket registration retained the correct plan through the remaining method
lookup path. The observations therefore did not justify requiring rejection
merely because a registration was absent.

ADR 0020's amendment *a zero-row result does not permit full-source dtplyr work*
required faithful metadata without processing source rows, or the established
safe refusal. ADR 0013 required inspection to represent the input's Grouping
plan. The Nest reference promised one data-frame cell per source group in each
set. False metadata and merged cells violated those respective contracts.
Unknown registration forcing also allowed the captured source-reading callback
to run during typed inspection. No full-source read was inferred merely from
the empty replacement output.

The two findings had different entry points, admission conditions and affected
graph sets. Their repairs could be specified and verified independently.

Related records read through the earlier investigation notes were #1359
(derived caller/registered brackets), #1382 (root subclass truncation), and
#1397 (ordinary-root registered head). The bracket witness changed the base
bracket registration on an ordinary root while leaving its head registration
ordinary. The converter witness changed neither entry point covered by those
reports. Tracker-level duplicate searches and full-record disposition belonged
to the coordinator's publication record.

## Completed ordinary and remote controls

The earlier November 6 [graph differential](graph-differential.R) was replayed
on this SHA. Its 1,242 comparisons covered 23 ordinary graph shapes,
zero/one/four source rows, three type predicates, two selector positions and
three consumers. It recorded 1,134 matches and 108 safe metadata refusals, with
no admitted mismatch, native backend error or local-only error.
[Per-case observations](graph-results.csv), [output](graph-differential.log).
The comparison normalized cell classes and cell row order, and therefore did
not assert either as a uniform backend contract.

The November 4 [source-sharing replay](dtplyr-source-reuse.R) completed with
`--assert` and exit 0 for uncached/cached inputs and both Nest verbs. Each
explicit execution evaluated the volatile source predicate once, and detail
and total cells retained the same source row. Construction and display of a
deferred result evaluated it zero times. [Output](source-reuse.log).

The November 6 [remote frontier replay](remote-frontier-controls.R) completed
96 calls: 36 SQLite, 36 DuckDB and 24 Arrow. SQL Nest refusals did not force
supplied Grouping specifications or send statements. SQLite sent none across
the controls; DuckDB sent six `LIMIT 0` statements for typed inspection and
none for name-only inspection. All 18 Arrow readers retained their 36 original
rows. [Output](remote-frontier.log). These controls did not establish Nest
support for SQL or Arrow.

## Replay, environment and limits

Set `MARGINPLYR_HUNT_SOURCE` to a source directory of the pinned SHA. Run each
command separately from the evidence directory, or pass its full script path:

```sh
Rscript registered-root-bracket.R
Rscript registered-root-bracket.R --assert
Rscript registered-root-bracket.R --matrix
Rscript registered-converter.R
Rscript registered-converter.R --assert
Rscript registered-converter.R --matrix
Rscript registry-frontier.R
Rscript dtplyr-source-reuse.R --assert
Rscript remote-frontier-controls.R
Rscript graph-differential.R
```

The two `--assert` invocations intentionally failed on the investigated SHA;
the observational matrices completed with exit 0. No implementation fix or
post-fix pass was claimed. Each short-lived process restored replaced methods
on exit; no registry mutation persisted. All executions completed terminally,
including the retained graph-differential session `72460`; no child process
remained. Testthat parallel execution was explicitly disabled. No package code,
permanent test, dependency, installed file, build or full-suite invocation was
changed or run by this track.

Environment: R 4.6.1, aarch64-apple-darwin23, Darwin 25.6.0 arm64; dplyr 1.2.1,
dtplyr 1.3.3, data.table 1.18.6.1, tibble 3.3.1, rlang 1.3.0, vctrs 0.7.3,
tidyselect 1.2.1, DBI 1.3.0, dbplyr 2.6.0, RSQLite 3.53.3, DuckDB 1.5.5,
Arrow 25.0.1, sf 1.1.2.

The finite probes did not establish every registered method, arbitrary class
extension, graph shape, hosted backend, package/OS version, interruption
behavior or resource limit. The ordinary controls did not settle the separate
custom-registration failures. sf-specific behavior was not newly exercised by
this track.
