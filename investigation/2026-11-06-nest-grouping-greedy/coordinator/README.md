# Construction reentry and record ownership

Investigated: 2026-11-06
Tested package SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`
Environment: R 4.6.1, aarch64-apple-darwin23

The hypothesis was that a nested inspection or Nest call, forced from a
Grouping-specification argument or typed predicate, could replace its active
parent's Sent query record. ADR 0027's *restore the active call's record under
reentry* and *ownership follows live recording frames* amendments required the
parent to regain its own record and saved audit setting on resumption.

`reentry.R` crossed three outer consumers, five nested outcomes, and both
outer/inner audit settings: 60 entry-argument cases matched. Nested outcomes
included local inspection, live DuckDB typed inspection, both local Nest
verbs, and a caught Package condition. The resumed callback and completed
outer call retained the expected record owner. Another five live DuckDB
inspection cases invoked the nested calls from ten typed-predicate callbacks
after the parent had recorded its `selection_proxy` statement. Each preserved
that exact parent record.

This finite matrix refuted the selected reentry hypothesis. It did not cover
interrupts, process termination, arbitrary recursive depth, or every external
condition class. It established no additional bug.

Replay from the tested checkout root:

```sh
Rscript investigation/2026-11-06-nest-grouping-greedy/coordinator/reentry.R
```
