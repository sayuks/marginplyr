# Order Margin results by grouping structure

`.sort` returns to every Margin verb as an opt-in Margin order. It shares
only its name with the `.sort` that ADR 0001's amendment removed. That one
ordered by displayed values and sorted the Margin label as a string among
them; this one orders by the Grouping plan's structure, taking each
dimension's Grouping bit before that dimension's own value. This decision
records what the order is, how far it is promised, and where it is produced.

## Decision

### It is a different feature with the same name

The removed `.sort` was `dplyr::arrange(result, !!!margin_cols)`. It placed
`"Total"` wherever that string fell among the values, which is locale
dependent and answers no question a caller has. Removing it was correct and
this decision does not reverse that judgment.

The name is reused for three reasons. No released version carried the old
option: the `0.1.0` section of `NEWS.md` never mentioned it, and it existed
only in the development tree, so no caller holds the old meaning. The
caller's intent is unchanged — a report in a sensible order — and what was
wrong was the key, not the request. And the plain reading of `.sort = TRUE`
matches what now happens: rows ascend by their keys, with each margin row
beside the rows it summarizes rather than at whichever end its label sorts
to.

`.sort` is not a logical. It takes `"none"`, `"last"`, or `"first"`, which
is the shape `.duplicates` and `.margin_label_position` already use, and a
caller who writes `TRUE` from memory of the old option gets an error naming
the three choices.

### The order

Within each fixed `.by` key, every grouping dimension contributes three
terms in the result's own column order:

```
by1 … byN,  bit(d1), is.na(d1), d1,  bit(d2), is.na(d2), d2,  …,  [set_id]
```

The key is therefore the result's leading grouping columns, left to right,
each dimension preceded by its Grouping bit and its missingness. Setting
`.sort` to `"first"` reverses the Grouping bits alone; missingness and values
stay
ascending, because first and last position margins and not missing values.

Two rules follow rather than being stated separately. Fixed keys sort first
because they come first, which is also the partition Parent shares and Total
shares are calculated within. A Grouping set identifier is the final
tiebreaker when `.id` names one, which makes duplicate occurrences adjacent
and in plan order; without `.id` there is no column to break the tie and no
observable difference to break, because duplicate occurrences aggregate the
same rows into identical values.

Composite dimensions need no rule. Their columns enter and leave a grouping
set together, so their bits are always equal and the second bit term is
redundant rather than wrong.

The order is locale independent because a Margin label never competes with a
value: at `bit = 1` the dimension holds the label alone, and at `bit = 0` it
never holds the label. The same property settles `.margin_label_position`,
which positions a synthetic factor level and therefore moves `levels()` and
never marginplyr's row order. It reaches row order only in an
`dplyr::arrange()` the caller writes themselves.

### Factor dimensions sort by level

A factor dimension sorts by its restored levels, not by its rendering. This
is what the backends that can hold factors already do — `arrange()` orders a
factor by level on local data and dtplyr, and DuckDB orders an `ENUM` by its
definition — so value order would be the option that costs an explicit cast.
A caller who declared `small < medium < large` asked for that order, and
`can_restore_factors` is true only for local, dtplyr, and DuckDB, so no
backend is asked for an order it cannot express.

### Missing values come last, and that is promised

Dialects disagree by default: with `arrange(g)` DuckDB returns `NA` last and
SQLite returns it first, while local `arrange()` returns it last. Sorting by
`is.na(g)` ahead of `g` returns it last on all three.

The promise is made rather than declined because it costs one term per
dimension and `IS NULL` is standard SQL. Declining it would reinstate
precisely what `.sort` exists to remove: the same code producing a different
first row on a different backend.

One consequence is worth recording. ADR 0012 leaves a source missing value
and a `NULL` Margin label indistinguishable from the grouping column alone.
Under a Margin order they remain indistinguishable by value but separate by
position, because the Grouping bit puts them in different groups.

### It promises exactly what `dplyr::arrange()` promises

A Margin order is a property of the object a Margin verb returns. On local
and dtplyr inputs that is the row order; on dbplyr inputs it is the
outermost query's `ORDER BY`, which `collect()` observes.

Whether it survives further verbs is not promised. Measured against dbplyr
2.6.0 and DuckDB, `select()`, `filter()`, and `head()` keep it while
`mutate()`, `left_join()`, and a further `group_by()` and `summarise()` lose
it. That split is dbplyr's query flattening, which marginplyr does not own
and which changes between releases, so enumerating the safe verbs would put
another package's optimizer into this one's contract. dbplyr warns on each
of the three losing cases, and documentation says so without making it a
promise.

This is the whole promise because it is the promise callers already know.
`.sort` writes the `arrange()` a caller cannot write, and inherits its
contract unchanged.

### Where the order is produced

`finalize_margin_operation()` orders rows, after it places the grouping
columns. ADR 0001's original sentence gave the shared finalizer "the common
sorting semantics" and its amendment withdrew that clause; the placement was
right and only the key was wrong, so the responsibility returns to where it
started. No verb kind is passed in: the key is derived from the Grouping
plan, which the shared module already holds.

The implementation rule is that the sort key must be resolvable in the `FROM`
clause of the query that carries the `ORDER BY`. Three paths satisfy it:

- A native summary on DuckDB or PostgreSQL sorts by an `ORDER BY` over
  `GROUPING(d)` and `d`, which folds into the aggregate query. No internal
  column exists, so none has to be dropped.
- Every other path — the `UNION ALL` adapter, and `expand_with_margins()` on
  every backend, since it always uses that adapter — sorts by an expression
  over a per-branch identifier literal. The union is already a subquery, so
  the identifier stays resolvable in `FROM` and the outer `ORDER BY` reads it
  after the projection drops it.
- The nesting verbs admit only data frames and dtplyr steps, so they sort
  with an ordinary `arrange()`.

The rule is stated this way because the narrower one is false. Materializing
a sort key is not what breaks: arranging on a column the same query
summarized and then dropping it lets dbplyr flatten the projection into the
aggregate, leaving an `ORDER BY` that names a column no longer in `FROM`.
DuckDB rejects that query with a binder error rather than losing the order
quietly, and it is the shape a prototype reached first. Neither adapter has
it.

### It costs nothing under `.duplicates = "keep"`

`.sort` neither requires an internal identifier on a native backend nor
changes which adapter runs. Whether a plan keeping duplicates falls back to
`UNION ALL` is decided by `native_duplicate_sets` alone, which is why
PostgreSQL already falls back and DuckDB does not. The combination is
accepted with no diagnostic, because there is no cost to report.

### The default is `"none"`

Row order stays unspecified unless a caller asks for a Margin order. The
existing rule survives as the default rather than being withdrawn, which is
also why this decision narrows ADR 0001 and ADR 0009 rather than replacing
them.

A default of `"last"` was weighed against this. A Margin result is as often
joined or mutated as read, and sorting it is work the caller did not request;
an `ORDER BY` on a large lazy result is not free. The cost falls on whoever
is served less often, and that is judged to be the reader, who types one
argument.

The default is not data dependent. The removed option defaulted to
`is.data.frame(.data)`, so identical code ordered local results and left
lazy results unordered. Row order is where such a split is hardest to
notice.

## Considered options

**`.bits`, an argument adding one Grouping bit column per dimension, leaving
the ordering to the caller's `arrange()`.** It was the narrower change and
would have left ADR 0001 and ADR 0009 intact. Rejected on measurement: the
caller must drop those columns afterwards, and `arrange()` followed by
dropping a summarized column is exactly the shape dbplyr flattens, so the
order is lost on lazy backends. The trap moves out of marginplyr into caller
code that nothing designed. Keeping the bit columns to keep the order is not
the result the caller asked for.

**A per-dimension direction on `.sort`**, so that a dimension can descend
under a Margin order (#373). Rejected: it reaches a reversed dimension and not
the request one step past it, ordering dimensions by a measure, which needs a
value no ordering over the result's columns produces. A caller writes that key
themselves either way, and the documentation carrying it carries the reversed
dimension too, so the argument would not retire what it duplicates. It is also
the wrong shape for `match_margin_choice()`, which admits one string from a
fixed list, and it would have to name dimensions a composite has none of.

**Leaving both out and keeping row order unspecified.** Rejected: the
existing guidance tells a caller to add a `grouping_bit()` summary per
dimension and drop it again, which fails on lazy backends for the reason
above and is unavailable at all under `.margin_label = NULL`.

**`summarize_with_margins()` only, or the lazy-accepting verbs only.**
Rejected: the key comes from the Grouping plan and never from the verb, so
restricting it would put a verb kind into the shared module that ADR 0001
exists to keep out. Every other common option is on all four verbs, and
`nest_by_with_margins()` returns a row-wise result whose row order is
iteration order.

**A logical `.sort`, or a logical accepting `"last"` and `"first"` as
well.** Rejected: three states do not fit a logical, and the union type makes
every mention of the argument a disjunction that exists only to be collapsed.

**`.margin_order` or another new name.** Rejected: no caller holds the old
meaning, and a new name would signal a new concept to learn where there is
none.

**`margins_first` and `margins_last` as the choices**, to keep `.sort` from
sharing vocabulary with `.margin_label_position`. Rejected: the two options
answer the same question about different things, and identical words let one
sentence say so. The confusion they invite is that setting one sets the
other, which the entry above denies directly.

**Declining to promise where missing values sort.** Rejected above.

**Enumerating the downstream verbs that preserve the order.** Rejected
above.

**Erroring on `.sort` with `.duplicates = "keep"`.** Rejected above: the
combination has no cost to refuse.

**Coordinating `.sort` with `.margin_label_position`, or warning when they
disagree.** Rejected: they are independent, no combination is wrong, and
`.margin_label_position = "first"` without `.sort` is what a caller writing
their own `arrange()` wants.

## Amendment: one identifier produces the order on every backend

"Where the order is produced" gave the native summary an `ORDER BY` over
`GROUPING(d)` and `d` folded into its aggregate query, and recorded that it
therefore adds no internal column. That mechanism is withdrawn. The
placement decision and the key are unchanged; only how the Grouping bits reach
the `ORDER BY` is.

**dbplyr will not emit an `ORDER BY` inside a query it wraps.** Measured
against dbplyr 2.6.0: an `arrange()` applied to the aggregate query is
discarded, with an "ORDER BY is ignored in subqueries without LIMIT" warning,
as soon as a later verb wraps that query in a subquery — which labelling the
omitted dimensions and placing the grouping columns both do. Writing the
`ORDER BY` into the aggregate `select_query` from `sql_build()` reaches the
same place, because `sql_render()` strips a subquery's `ORDER BY` itself. The
narrower rule the decision above already states is what holds: the sort key
must be resolvable in the `FROM` clause of the query that carries the
`ORDER BY`, and `GROUPING(d)` is resolvable in the aggregate query alone.

**The Grouping set identifier is resolvable there instead.** The native
adapter already computes one for `.id`, as a `CASE` over `GROUPING()` calls
that folds into the aggregate query, and every Grouping bit is a function of
it. `finalize_margin_operation()` derives the bits from that column and drops
it again. This is the same expression-over-an-identifier the `UNION ALL`
adapter uses, so there is one mechanism rather than two, and the finalizer
holds the whole key for every path.

Three consequences are worth recording.

*The native summary gains one internal column* when `.sort` is not `"none"`
and `.id` does not already name one; it is dropped from the result. What that
criterion was protecting still holds: `GROUP BY GROUPING SETS` is unchanged,
and which adapter runs is decided by `native_duplicate_sets` and `.id` before
the identifier is allocated, so a Margin order still never costs the native
plan and still adds no fallback. A sort-only identifier does not have to
number occurrences, which is why it does not force the `UNION ALL` fallback
that `.id` forces under `.duplicates = "keep"`.

*The key really is the result's own leading grouping columns*, which the
in-aggregate `ORDER BY` could not have been. The aggregate query holds the
pre-label values, so a labelled dimension that is not already character would
have ordered by its input type there and as character on a local input — the
same code producing a different first row on a different backend, which is
what this decision exists to remove.

*Contextual shares compose.* They join the staged result, and no `ORDER BY`
a join reads survives into the query the caller receives, so ordering has to
come after them. The identifier they already stage is the one the finalizer
orders by, so that path adds no column either.

## Amendment: fixed keys order their missing values last too

The key above reads `by1 … byN` for the fixed keys, giving a missingness term
to the grouping dimensions alone. Every column in the key gets one instead:

```
is.na(by1), by1, …, is.na(byN), byN,
bit(d1), is.na(d1), d1,  bit(d2), is.na(d2), d2,  …,  [set_id]
```

A fixed key still takes no Grouping bit, because it is present in every
grouping set and never holds a Margin label. Only its missingness is new.

The original key follows from framing missingness as something a dimension
contributes alongside its Grouping bit. Read that way a fixed key has nothing
to contribute, because it has no bit — but the consequence is that a `.by`
column holding missing values is ordered by the dialect's own default, last on
a local input and on DuckDB and first on SQLite. That is exactly the
disagreement "Missing values come last, and that is promised" exists to
remove, reappearing one column to the left of where that entry was looking.
The promise is therefore made of the key as a whole: wherever a column appears
in it, its missing values sort last.

The cost is the same one term per column that entry already accepted, and
`IS NULL` is standard SQL. `"first"` still reverses the Grouping bits alone,
so a fixed key's missing values stay last whichever end the margins are at.

One reading is closed by this. The entry above says missing values come last
"within a Grouping bit group", which was true of dimensions and said nothing
about fixed keys; it now reads as the whole key, and `CONTEXT.md`'s **Margin
order** entry says the same.

## Amendment: a lazy result carries the order and records no window ordering

"It promises exactly what `dplyr::arrange()` promises" names `collect()` as
what observes the `ORDER BY` on a dbplyr input. `compute()` observes it too,
and the entry above did not say so because it could not: on dbplyr 2.6.0 a
Margin order made `compute()` fail outright for every `.sort` but `"none"`
(#102), on the very workflow the database-backends guide recommends for
keeping a result in the database.

Naming it is not the enumeration this decision rejected. That rejection is
about verbs applied *after* the result, whose outcome dbplyr's query
flattening decides and can change between releases. `compute()` renders this
query and materializes the rows it returns, so it observes the `ORDER BY` for
the same reason `collect()` does, and one warning about flattening does not
cover a call that never flattens anything.

**`arrange()` writes the key into two places.** Alongside the query's
`ORDER BY` it records a window ordering, which is what a window function
written over the result would order by. The previous amendment's rule holds
for the first and not for the second: the `ORDER BY` reads the Grouping set
identifier out of the `FROM` clause and survives the projection that drops
that column, while the window ordering is rewritten by the same projection,
losing every term that names the identifier. What stays recorded is a
truncated key — the displayed values and their missingness, with the Grouping
bits gone — which is not this decision's order and orders a margin row by
where its label falls.

**A Margin result therefore records no window ordering at all.** `compute()`
replays whatever is recorded through `window_order()`, which accepts a bare
column name or `desc()` of one and rejects everything else, so each missingness
term and each Grouping bit fails there; clearing it is what makes a sorted
result materializable. It also removes a claim that could only be wrong, since
no ordering over the columns a Margin result exposes reproduces the key. A
window function over a sorted lazy result needs `dbplyr::window_order()`,
exactly as it does over an unsorted one, and asking for a Margin order
discards any window ordering the input carried.

Two consequences are worth recording.

*The order the rows arrive in is untouched.* Clearing the window ordering
leaves the `ORDER BY` alone, so the rendered SQL is what it was and
`collect()` returns what it returned.

*Which results this reaches is read from the prepared backend kind*, as the
`records_window_order` capability, and not from the class of the finalized
result — ADR 0014's rule, for its reason. A local result and a dtplyr step
record no window ordering to clear, and the nesting verbs admit only those.

## Amendment: the projection that drops an unread identifier runs first

The amendments above place the staged identifier in the `FROM` clause of the
query carrying the `ORDER BY`, and rely on the `UNION ALL` or the aggregate
query to put it there. A plan holding one grouping-set occurrence has neither:
the portable adapter returns its single branch uncombined, so the identifier is
a literal the same query computes, and the projection that drops it wraps that
query and takes the ordering with it. `expand_with_margins()` reached this on
every dbplyr backend, and `summarize_with_margins()` on every backend without
native `GROUPING SETS` (#339).

**Such a plan does not read the identifier at all.** Every dimension is in the
same set as every other row's, so each Grouping bit is constant — the condition
the key already omits a bit term on — and the tiebreak orders by a column with
one value. The key is then the fixed keys and the dimensions alone.

**So the projection runs before the ordering whenever the key does not read the
identifier.** Nothing is left to drop afterwards, the `ORDER BY` is the
outermost one by construction, and no query level exists for dbplyr to discard
it from. Where the key does read the identifier the order of the two is
unchanged, and so is every plan holding two or more occurrences.

Two consequences are worth recording.

*The tiebreak is omitted rather than emitted over a constant.* `.id` naming the
identifier on a one-occurrence plan therefore drops it from the key too, which
is the same statement the paragraph above makes about `.id` on a plan with no
observable difference to break: one occurrence is one such plan.

*Which adapter runs is still decided before any of this.* The identifier is
allocated after that choice, so a one-occurrence plan that ran on the native
path still runs there and still stages the column; what changes is only where
the projection dropping it sits.

## Amendment: a caller writes the key too, and one clause is narrowed

`.sort`'s vocabulary stays `"none"`, `"last"`, and `"first"`; the direction
#373 asked for is written by the caller, and *Considered options* above records
why the argument was refused. What this amendment settles is how far a
caller-written key reaches, which two entries there answered on a measurement
that turns out to be narrower than they state.

**Both entries rest on "the caller must drop those columns afterwards, and
`arrange()` followed by dropping a summarized column is exactly the shape
dbplyr flattens".** Measured against dbplyr 2.6.0, dropping a *Grouping bit*
does not lose the order on either adapter for a plan whose result is wrapped by
the projection that places and labels the grouping columns: the bit is produced
by the aggregate query or by the `UNION ALL`, so it stays resolvable in the
`FROM` of the query carrying the `ORDER BY` after the projection stops
selecting it. That covers every `rollup()` and `cube()`, and the recipe #373
asked for is one.

Where no such projection exists the entries hold. A plan of one grouping-set
occurrence under `.margin_label = NULL` labels and casts nothing — the shape
the amendment above is about — so the caller's `arrange()` applies to the query
the summary ends in and the `select()` dropping the bits wraps it: RSQLite
rendered no `ORDER BY` at all, with dbplyr's "ORDER BY is ignored in subqueries
without LIMIT", while DuckDB kept it. A column the caller computed after the
summary is in that position on every plan, which is why the measure-ordered
recipe cannot drop its window column and DuckDB refuses the query with the
binder error this decision names.

**"Unavailable at all under `.margin_label = NULL`" is withdrawn.** The
`grouping_bit()` recipe runs there, and returned the intended order on DuckDB;
what the dimensions lose is their labels, not their bits.

So what `.sort` writes that a caller cannot is narrower than these entries say,
and the reason to keep writing it is the one the entries make second: a caller
who writes the key holds a helper column whose safety depends on the plan, and
`.sort` needs none. `investigation/writing-a-margin-order-key-by-hand.md` holds
the measurements, and `vignettes/recipes.qmd` shows what a caller writes.
