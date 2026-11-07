# Share closure-list boundaries

Investigated: 2026-11-07

Tested source: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`, from a disposable
Git archive of the new main-based worktree. No package implementation or package
test was changed. All research R processes explicitly set
`TESTTHAT_PARALLEL=false` and were collected to terminal completion.

## Contract and reproduction

ADR 0010, *Compute Parent shares as a contextual summary*, required a share
source to be self-contained and forbade a later ordinary summary from reading
an earlier share. ADR 0017 applied the same rules to Total shares. The helper
reference's *Eligible source summaries* and *Direct shares* sections stated
these rules. The following finite cases crossed those boundaries on the named
source SHA.

The fixture had grouping values a/b, numeric x=1/3, and a source-data column
prior=10/20. A first summary calculated value=sum(x); an earlier share was
named prior; a later ordinary summary observed invoked the following wrapper.
The local prior argument initially hid the data-mask binding. Removing it
before invocation exposed that binding. Every family returned observed
10/20/30 beside the displayed share 0.25/0.75/1 rather than refusing the
dependency. The grand-total share remained one. Reversing the dependency
order made the wrapper read the earlier ordinary prior=sum(x), and that
non-self-contained observed summary was accepted as a share source.

Each remaining component of the minimal wrapper supplied a distinct role:
the formal hid the name during closure construction, the list carried the
closure, the boundary under test changed its visible carrier or lookup path,
removal exposed the forbidden binding, and invocation demanded it. Deleting
the removal made each witness legal and returned scalar one on every row.

### F1: transparent list-value transfers lost invocation dependencies

```r
(function(prior) {
  carrier <- list(function() sum(prior))
  copied <- identity(carrier)
  rm(prior)
  copied[[1L]]()
})(1)
```

`invisible(carrier)` and `force(carrier)` reproduced the same failure. Copying
the list directly instead of calling a transfer correctly refused both
forbidden dependency directions. The distinguishing hypotheses were that
(1) list identity itself was lost, (2) literal closure removal was not followed,
or (3) the share name was not registered. The direct-list-copy refusal and
the retained-argument legal control separated (1) from (2) and (3).

The implementation's `scope_value_closure()` recognized these known base
transfer calls for a closure value, whereas `scope_closure_list()` only
recognized bare bindings, literal lists, concatenation, append, and literal
subsets. This was a list-value provenance boundary rather than a change to
share arithmetic. The relevant existing tests covered those transfer calls
for a direct closure and c/append/subset for a list, but not their combination.

### F2: literal list-name writes left a stale closure member map

```r
(function(prior) {
  carrier <- list(read = function() sum(prior))
  names(carrier) <- 'other'
  rm(prior)
  carrier$other()
})(1)
```

Literal writes through `attr(carrier, 'names')` and `attributes(carrier)` also
reproduced the missing refusal. Calling `carrier[[1L]]()` after the same
rename correctly refused. This separated an obsolete name-to-element map
from loss of the list value or closure itself.

There was an independently observed false refusal on a legal call. Starting
with read=the dependent closure and other=function() 7, then swapping their
names to other/read and invoking carrier$read() should return seven. It
instead raised the forbidden-dependency error for the old read member.
The position-equivalent carrier[[2L]]() returned seven. Thus invalidating all
list provenance after a name write would fix neither direction correctly.

The generic assignment handling preserved a list's old element names while
`scope_value_closure()` resolved a subsequent literal member against those
names. Existing list-write tests covered element replacement and deletion;
partial-name tests covered lookup against unchanged names.

### F3: nested literal lists lost the inner callable descriptor

```r
(function(prior) {
  carrier <- list(outer = list(read = function() sum(prior)))
  rm(prior)
  carrier$outer$read()
})(1)
```

Two numeric `[[1L]]` steps, two literal named steps, the recursive literal
`[[c('outer', 'read')]]` form, and extracting the inner list before invocation
all reproduced the missing refusal. A one-level literal list correctly
refused. The depth difference separated missing nested-container structure
from closure removal or member-name recognition.

`scope_closure_list()` represented each list element as a direct closure or
NULL. A nested plain list therefore had no retained child-list structure for
the later member traversal. Fixing F1's known-call transfers would leave this
boundary, and fixing finite nested traversal would leave F1. These were
recommended as three independent repair slices, with no blocking edges.

## Completed comparisons and retained evidence

`share-list-boundaries.R SOURCE OUTPUT_DIR observation|control|contract
[all|transfer|rename|nested]` was the portable replay command.

- Observation mode completed 168 comparisons: three eager input classes
  (data.frame, tibble, data.table), both helpers, both prohibited dependency
  directions, seven witness/control forms and retained/removed bindings.
  It recorded 60 missing refusals, 24 correct refusals and 84 legal successes.
- Control mode completed 108 comparisons and exited zero: all retained-binding
  calls, all direct-list-copy refusals, and all position-based renamed-list
  refusals. All input serializations were unchanged.
- The combined contract mode intentionally exited one. Separate contract modes
  also intentionally exited one: transfer=72 comparisons, rename=24,
  nested=24. Each asserted the desired refusal rather than merely observing
  successful execution. Their logs and CSVs retained the missing refusals.
- `share-renamed-safe.R` observation mode completed 24 comparisons: 12 false
  named-member refusals and 12 equivalent numeric-member legal results of
  seven. Inputs were unchanged. Its control/contract modes respectively select
  the legal numeric access alone / assert both accesses are legal.

The original coarse probes were retained as exploratory evidence separately
from the final portable harness. Probe failures through rep/rev/head/tail,
as.list/unname/setNames/structure, higher-order mapping, an immediately called
list-returning identity closure and pairlists were adjacent observations, not
claims of complete support for their dispatch or evaluation semantics. F1's
final red-capable matrix selected only identity/invisible/force on plain lists.
They were not counted as further independent findings.

A seeded numeric oracle separately compared every share with its displayed
denominator. `numeric-oracle.R SOURCE OUTPUT_DIR` used seeds 1:24; 0 through
11 input rows; positive, negative, zero and missing measures; missing fixed
and variable keys; sum/mean/count summaries; both helpers; local and collected
dtplyr; and all three sort requests. All 144 collections and 9,540 ratio
assertions succeeded. The oracle asserted ratios and cardinalities, not a
complete Margin-order contract. Empty fixed-key inputs contributed no rows.

Scripts, CSVs, logs, and session information were saved beside this file.
The environment was R 4.6.1, macOS Tahoe 26.6.2 aarch64, marginplyr 0.1.0,
dplyr 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1, dtplyr 1.3.3 and
data.table 1.18.6.1. Seeds were fixed only for the numeric oracle; the closure
witnesses were deterministic and used no randomness.

## Duplicate boundary and limitations

The local duplicate search read the recent carrier/exhaustive/deep hunt notes
and searched investigation and share tests for nested literal lists,
names(carrier), identity(carrier), and the adjacent transfer spellings.
No earlier note/test described these exact three finite boundaries. The
closest reports were #1361 (literal one-level closure list), #1389/#1402
(direct closure transfers), #1403/#1414 (list element writes), #1413
(c/append/literal subset), and #1415 (partial lookup with unchanged member
names). F1 retained a list rather than a direct closure; F2 changed only
names metadata; F3 used a nested literal container rather than one level.

The delegated lane performed no external-system operations. Before publishing,
the coordinator needed all-state issue/PR duplicate queries such as
`"share" "identity" "list"`, `"share" "names" "closure"`,
`"share" "nested" "list"`, plus full bodies/comments of the closest matches.
Those external duplicate checks and current-main confirmation belonged to
the coordinator's publication record, not this lane's claimed evidence.

No guarantee was established for arbitrary higher-order carriers, custom list
classes/methods, general reflection, unbounded nesting, SQL execution of these
R closures, other dependency versions, platforms or concurrency. No repair
was attempted, and these research outcomes were not Review-ready results.
