# Grouping specification composition and selection dispatch

Investigated: 2026-11-05
Baseline: `0403c915` (the complete SHA and package versions are in
[environment.txt](environment.txt)).

## S1. Known stringr pattern classes bypassed typed selection dispatch

The public constructors delegate their selections to tidyselect, and
`inspect_grouping()` describes selection against captured column metadata.
The dispatch regression from #995 and the test named *dispatched helper
arguments use one zero-row snapshot* established the existing boundary:
a caller's S3 method can inspect the typed selection context.

[The replay](pattern-dispatch.R) used an ordinary `stringr::regex("^a")`
object and a caller-defined `length.stringr_regex()` method. The method
returned one when column `a` in the selection context was character, and two
otherwise. Direct typed tidyselect selected `a`. The explicit opaque-wrapper
control, `matches(identity(pattern))`, also selected `a` in inspection and
expansion. Its method observations were `TRUE, TRUE`: both executions saw
the character prototype.

The otherwise identical `matches(pattern)` selection failed in inspection,
expansion, and fixed-key inspection, with:

```text
In argument: `matches(pattern)`.
Caused by error in `matches()`:
stringr patterns must be length 1.
```

Each failed consumer recorded one `FALSE` observation: the method ran against
the integer name proxy. The pattern itself had one stored element. The
name-only classifier allowed the exact known stringr class vector, while
tidyselect called the dispatched `length()` method before delegating matching
to stringr. This was a premature selection error, rather than a missing
backend dependency or unsupported source operation. It required a custom S3
method for the existing pattern class; no claim was established about ordinary
unmodified stringr patterns. No wrong-column result was established for this
fixture.

#1255 concerned the reverse classification error for ordinary stringr
patterns, which requested unnecessary metadata. #995 concerned implicit-class
base generics and injected classed selectors. This witness exercised the
explicit known-pattern-class exception after those repairs. The root
investigation note records the all-state duplicate search and final ticket.

Run in fresh R processes from the investigated package root:

```sh
Rscript investigation/2026-11-05-expand-grouping-greedy/spec/pattern-dispatch.R
Rscript investigation/2026-11-05-expand-grouping-greedy/spec/pattern-dispatch.R --assert
```

The first command asserted the direct and typed-wrapper controls and exited
zero. The desired-behavior assertion intentionally exited one against the
baseline. Both transcripts are retained beside the script. The process owned
the custom S3 method and observer bindings; process exit removed them.

## Bounded algebra coverage

[The results](oracle-results.txt) recorded two independent seeded
explorations. They were newly executed on this baseline; prior notes supplied
fixtures and exclusions, rather than results credited to this run.

The existing adversarial mixed-family oracle was extended from 400 to 2,000
generated shapes using seed 41248. Of those, 1,634 were valid fixture-sized
Grouping specifications: 1,092 had unique memberships and 542 required the
duplicate diagnostic. All matched the independent family, order and set-ID
oracle across `keep`, `drop`, and `error`. The other 358 were top-level
selectors and eight exceeded the fixture's 300-occurrence cap.

[The composite-dimension oracle](composite-oracle.R) used seed 51796 and five
available dimensions, repeated selections, multi-column composite dimensions,
unions, products, rollups, cubes, and nested products. Of 2,000 generated
shapes, 1,772 compared exact public included-column order and set membership
under both `keep` and `drop` with independently enumerated families. All
matched. The other 190 were top-level selectors and 38 exceeded the
500-occurrence fixture cap. The generated cube cap was ten units.

These finite observations did not establish unbounded plan resource limits or
arbitrary extension-method behavior. The exploration also tested method
dispatch on unclassed primitive and matrix arguments of selected base
operations in the classifier; the specific coercion, indexing, repetition,
length and names probes did not call the supplied implicit-class methods.
An attempted `as.character.stringr_regex()` wrong-column witness did not run
that method in stringi matching, so it was excluded.

Literal numeric or logical variadic forwarding remained a potential extension
of the existing #1280 boundary; no independent new defect was established from
that lead, so it did not become a separate ticket.

## Independent SQLite challenge

The companion [SQLite replay](../sql/sqlite-derived-destination-independent.R)
challenged the coordinator's destination-rebinding finding with three source
rows, identifier `FALSE`, case-equivalent destination `FACTS`, and four ordinary
derivations: mutation, projection, arrangement and `head()`. Each new temporary
destination returned zero rows instead of six, or four for `head()`. The same
four overwrites replaced a three-row temporary input with an empty table.

Direct materialization copied six rows for a new destination and refused an
existing temporary-input overwrite while preserving all three source rows.
Qualified persistent source, different destination, and ordinary identifier
controls each copied six rows. The persistent source retained all three rows
in every comparison. These outcomes supported one derived-materialization
ticket, rather than separate tickets for the four transformations.
