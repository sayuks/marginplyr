# Delegate result class and attributes to dplyr

marginplyr promises exactly what marginplyr constructs. The class and the
attributes of a Margin operation's result are whatever the dplyr and vctrs
operations that the verb is composed from produce, and marginplyr writes no
attribute-restoration logic of its own. Factor restoration in `R/factor.R` is
the single exception, because marginplyr is what decomposed the factors it
rebuilds. The reference will state that results follow the same class and
attribute rules as the corresponding dplyr verb, and will not promise that the
input class, its object-level attributes, or its column attributes are
preserved.

## Decision

### The promise boundary

The rule that decides every case below is whether marginplyr constructed the
property or merely passed it through.

marginplyr constructs, and therefore promises:

- the ungrouped result of `summarize_with_margins()`,
  `expand_with_margins()`, and `nest_with_margins()`, and the row-wise result
  of `nest_by_with_margins()`, both applied unconditionally in
  `finalize_margin_operation()` and at the end of `nest_by_with_margins()`;
- Margin columns first, in Grouping-plan order, followed by the remaining
  columns;
- factor and ordered-factor levels, ordering, and Margin-label placement, per
  ADR 0012.

marginplyr passes through, and therefore describes but does not promise:

- the class of the result relative to the class of the input;
- object-level attributes of the input;
- attributes and classes of columns marginplyr does not rewrite.

Nothing else in the package needs a reconstruction rule, because every Margin
verb ends in a dplyr verb applied to a data frame that dplyr itself produced.

### Measured behavior

Measured against marginplyr at this commit with R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, and dtplyr 1.3.3. The issue's table recorded
"preserved" for two rows that are preserved only under conditions the table
did not vary, so those rows are restated here with the condition attached.

| Subject | `summarize` | `expand` | `nest` outer | `nest` inner |
|---|---|---|---|---|
| Result class from a plain `data.frame` | `data.frame` | `data.frame` | `data.frame` | `tbl_df` |
| Result class from a tibble | `tbl_df` | `tbl_df` | `tbl_df` | `tbl_df` |
| `data.frame` subclass (`mytbl`) | dropped | kept | — | — |
| Object-level attribute, plain input | dropped | kept | dropped | dropped |
| Object-level attribute, subclassed input | dropped | **dropped** | — | — |
| Bare column attribute, one grouping set | — | kept | — | — |
| Bare column attribute, many grouping sets | — | **dropped** | — | — |
| Classed column (`Date`, `POSIXct`, custom) | kept | kept | kept | kept |
| Factor levels and `.margin_label` | kept | kept | kept | — |

The two bolded rows are the corrections. Both are decisive for this decision,
because both show the same verb answering the same question differently
depending on an input or plan property the caller did not think they were
choosing:

- Object-level attributes survive `expand_with_margins()` for a plain
  `data.frame` and are lost for a subclass of one. The step responsible is
  `dplyr::mutate()`, which restores object-level attributes for a bare
  `data.frame` and drops them for a subclass with no `dplyr_reconstruct()`
  method. Plain `dplyr::bind_rows()` keeps them in both cases, so this is not
  the union.
- A `label` attribute on a bare double survives a one-set plan and is lost
  from a plan with two or more sets, because a multi-set plan combines
  branches with `dplyr::union_all()` and `vctrs::vec_c()` drops attributes of
  bare vectors. A column that also carries a class keeps both class and
  attribute, which is why the issue's `value` column — classed `myunit` and
  labelled at once — read as preserved.

The subclass drop under `summarize_with_margins()` is likewise not
marginplyr's: `dplyr::summarise()` alone returns a bare `data.frame` from the
same subclassed input, because no `dplyr_reconstruct()` method exists for it.

### Verb-to-verb asymmetry is dplyr's composition, not marginplyr's

`expand_with_margins()` keeping an object-level attribute that
`summarize_with_margins()` drops is not an inconsistency between two
marginplyr verbs that marginplyr should resolve. It is the difference between
`dplyr::mutate()` and `dplyr::summarise()`, measured directly on a plain
`data.frame`: mutate, filter, and bind_rows keep object-level attributes and
summarise does not. Each Margin verb inherits the rule of the dplyr verb it
ends in, and a caller who knows dplyr already knows which one that is.

Aligning the verbs was considered in both directions and rejected.

Aligning downward — having `expand_with_margins()` strip what dplyr would
have kept — destroys information for symmetry alone, and makes marginplyr
worse than the dplyr pipeline it replaces.

Aligning upward — restoring attributes onto the result of
`summarize_with_margins()` — is worse than it looks. For a subclassed input,
whether an attribute survives is decided by that class's
`dplyr_reconstruct()` method, which is the class author's declaration about
which properties an aggregation invalidates. marginplyr reattaching an
attribute after the fact overrides a decision that is not marginplyr's to
make, and it would do so silently, on classes that did not exist when the
restoration was written. A `provenance` attribute naming the source rows is a
concrete case where surviving aggregation is the wrong answer, and marginplyr
cannot tell that attribute apart from one that should survive.

Neither alignment is available in full anyway. The bare-column-attribute row
above shows that marginplyr could not make attribute preservation
plan-independent without reimplementing vctrs' combining rules for arbitrary
user vectors — which is the guarantee this ADR exists to refuse.

### `nest_by_with_margins()` returns a row-wise data frame, by contract

`nest_by_with_margins()` calls `dplyr::rowwise()` unconditionally, so a plain
`data.frame` input returns a `rowwise_df`. This is not a class-preservation
failure and it is not changed. There is no row-wise plain `data.frame` in
dplyr: `rowwise_df` is a `tbl_df` subclass, and `dplyr::nest_by()` returns
one from a plain `data.frame` for the same reason. The row-wise shape is the
verb's deliverable, exactly as a grouped result is `dplyr::group_by()`'s.
marginplyr constructs it, so marginplyr promises it.

The element class inside the list column is the opposite case. Elements are
tibbles for a local input because `dplyr::pick()` returns a tibble, and data
tables under dtplyr because that is what the data.table translation produces.
marginplyr chooses neither. The asymmetry is therefore documented as observed
backend behavior and not promised: what is promised is that each element is a
data frame holding the input's non-key columns, alongside the existing
statement that the list column's `vctrs_list_of` subclass is not part of the
API.

Normalizing the element class was rejected. Locally it would mean coercing
away from the tibble that dplyr chose; under dtplyr it would mean mapping a
conversion over every element after collection, an allocation per group to
satisfy an aesthetic, and for `nest_with_margins()` — which returns a lazy
dtplyr step — it would mean collecting a result the caller asked to keep
lazy. A caller who needs uniform elements writes
`lapply(result$data, tibble::as_tibble)`, which costs the same and is opt-in.

## Implementation consequences

No source change is required; this ADR ratifies existing behavior and forbids
a class of future change.

`finalize_margin_operation()` stays as it is: ungroup, restore factors,
reorder Margin columns. It gains no attribute-capture step, no
`dplyr_reconstruct()` call, and no template argument. Backend adapters
likewise gain none. A future reconstruction hook is a change to this decision
and needs a superseding ADR, not a patch.

`restore_margin_factors()` remains the only place marginplyr rebuilds a
column's type, and its justification is narrow and stated: marginplyr
decomposed those factors during preparation and inserts the Margin label as a
level, so nothing else can rebuild them. That justification does not extend
to any other attribute, because marginplyr removes no other attribute.

## Documentation consequences

The `@return` text of the Margin verbs must state the rule without promising
class preservation:

- `summarize_with_margins()` and `expand_with_margins()`: an ungrouped data
  frame, or a lazy table for a lazy input, whose class and attributes follow
  the same rules as `dplyr::summarise()` and `dplyr::mutate()` respectively;
  the input's class and object-level attributes are not guaranteed to be
  preserved.
- `nest_with_margins()`: unchanged in shape, with the same class and
  attribute caveat.
- `nest_by_with_margins()`: always a row-wise data frame grouped by the
  visible grouping columns and `.id`, whatever the input class.

The nesting verbs' shared "Relationship to tidyr and dplyr" section gains the
element-class note: tibbles for a local input, data tables under dtplyr, with
the `lapply()` coercion for callers who need one class.

The example comment in `R/nest_with_margins.R` that says the outer class "is
preserved" for a plain `data.frame` input is accurate for that example and
misleading as a general statement. It is rewritten to say that a plain
`data.frame` input produced a plain `data.frame` here, which is why the list
column prints flattened.

`design/architecture.md` gains a line at the `R/factor.R` module description
recording that factor restoration is the only type or attribute restoration
in the package, with a pointer to this ADR.

## Test consequences

Tests pin what this ADR promises and nothing else. The measured table above
is evidence for the decision, not a specification: a test asserting a row of
it would convert a described behavior into a guarantee by the back door, and
would fail as an unexplained marginplyr failure the day dplyr or vctrs
changed a rule marginplyr does not own. No test therefore asserts
object-level attributes, `data.frame` subclasses, or bare column attributes
through any verb.

Two tests in `test-grouping-interface.R` cover the promise:

- "Margin results take their class from the underlying dplyr verb" asserts
  that a plain `data.frame` stays a plain `data.frame` through
  `summarize_with_margins()`, `expand_with_margins()`, and
  `nest_with_margins()`. For the first two it asserts the literal class and
  the class of the corresponding bare dplyr expression side by side. The
  pair is the point: the literal assertion is what a reader checks, and the
  delegation assertion is what stays true if dplyr's own rule changes, so a
  future failure says which of the two happened.
- "`nest_by_with_margins()` is row-wise whatever the input class" asserts
  `rowwise_df`, the visible grouping columns, and that a list-column element
  is a data frame — the whole of what the element contract promises.

Both were confirmed to fail before passing, by injecting the regression each
exists to catch: an `as_tibble()` in `finalize_margin_operation()` for the
first, and `group_by()` in place of `rowwise()` for the second.

Classed-column round-tripping — `Date`, `POSIXct` with `tzone`, ordered and
unordered factors — is already covered by `test-grouping-interface.R` and
`test-factor.R` under ADR 0012, and needs nothing further here.

## Amendment: the cell expression names the converter, and it names the one each backend's own nesting gives

One passage is superseded, in *`nest_by_with_margins()` returns a row-wise data
frame, by contract*: "Elements are tibbles for a local input because
`dplyr::pick()` returns a tibble, and data tables under dtplyr because that is
what the data.table translation produces. marginplyr chooses neither." The
element classes it names are unchanged. What it gives as the reason for them is
not, and neither is its last sentence.

The cell can no longer be `pick(everything())`. dtplyr translates a `pick()`
standing where a value stands into a literal `data.table()` call carrying one
named argument per column, so a payload column named for one of that
function's formals is taken as that argument: `key` and `check.names` raise,
and `keep.rownames` and `stringsAsFactors` are absorbed and leave the column
out of every cell (#424). The cell names its columns into `list()`, whose only
formal is `...`, and converts what that returns.

So marginplyr chooses the converter, and chooses per backend the one that
answers what a caller nesting the same input without marginplyr would get:
`dplyr::as_tibble()` locally, where `dplyr::pick()` returned a tibble, and
`data.table::as.data.table()` under dtplyr, where `tidyr::nest()` translates to
`.SD` and yields a `data.table`. The promise boundary is where it was — what is
promised is that each element is a data frame holding the input's non-key
columns — but the observed behavior the reference describes is now marginplyr's
to keep in step with the backends rather than something falling out of a
translation. A backend whose own nesting verb changes what it gives is a change
to this ADR.

One cell is not the backend's: the one a nesting leaving no payload column
builds. The reference documents which class it is and why, in the paragraph
that promises its row count.

The rejected alternative stands unnarrowed: normalizing the element class is
still rejected, and each cost named for it is still a cost, because what is
rejected is one class across backends and not the naming of a converter.
