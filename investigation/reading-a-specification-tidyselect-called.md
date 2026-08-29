# Reading a specification tidyselect called

Investigated: 2026-08-29

Measured while deciding how a nested Grouping specification position could
refuse a specification stored as a function (#265). #190's refusal is written
from the condition tidyselect raises about a subscript it will not use; a
function is not such a subscript, so that condition never exists. The question
was where else the object can be read from, given that evaluating the caller's
argument a second time to find out is what ADR 0008 forbids.

## Environment

| Component | Version |
|---|---|
| R | 4.6.1 |
| tidyselect | 1.2.1 |
| rlang | 1.3.0 |
| vctrs | 0.7.3 |

## What tidyselect does with a function

`tidyselect:::as_indices_sel_impl()` branches on `is.function(x)` before
anything else. `is.function()` is not generic and the branch precedes every
other test, so no class, method, or attribute on the value changes the
decision: a function is applied as the predicate form of a selection, and a
value of any other type reaches `as_indices_impl()`, which is what raises the
`vctrs_error_subscript_type` condition carrying `i` and `subscript_arg` that
#190 reads.

The application is `map(data, predicate)` — `lapply()` — followed by a loop
calling `check_predicate_output()` over what it returned. Four shapes of a
`margin_grouping_spec` stored as a function were measured against
`inspect_grouping()`:

| Stored as | What the caller received before #265 |
|---|---|
| `function() 1` | `simpleError`: `unused argument (X[[i]])` |
| `function(x) stop("inside")` | whatever the body raised |
| `function(x) "no"` | tidyselect: `Predicate must return TRUE or FALSE, not a string.` |
| `function(x) TRUE` | no error; every column selected |

The first three name nothing the caller wrote. The fourth is a valid predicate
as well as a specification and produces no condition at all, so nothing on the
error path reaches it.

## Where the object is readable

`eval_select(allow_predicates = FALSE)` refuses a function instead of calling
one, but it refuses every function: `where(is.numeric)` returns a function and
was refused by the same branch, with a `tidyselect_error_predicates_unsupported`
condition carrying neither the value nor a subscript label. It answers the
diagnostic question by removing a documented selection form and without
identifying the object, so it answers neither half.

The frames tidyselect opened are the remaining source, and they outlive the
call: `as_indices_sel_impl()` binds the value to `predicate` before applying
it, and its frame is still on the stack for all three failing shapes above —
including the third, where the predicate has already returned. Read at the
moment the error unwinds, through a calling handler, the object is reachable
for each of them. Read after `eval_select()` returns, it is reachable for none.

The frames were identified by the namespace their function closes over —
`identical(environment(sys.function(i)), asNamespace("tidyselect"))` — rather
than by any private name. Scanning their bindings for a function carrying the
class answered `TRUE` for the three failing shapes and `FALSE` for an ordinary
selection, a `where()` predicate, a `where()` predicate that raises, a missing
column, and the non-function specification #190 already refuses.

Two properties of that scan were measured rather than assumed. `ls()` on a
frame lists `...`, which `get()` cannot read as a value; and a formal a caller
left missing is listed and raises when read. Both were reached in the shapes
above before they were excluded.

## A frame holding the object is not a frame that applied it

The scan as first written answered `TRUE` for five arguments in which nothing
was applied at all. `tidyselect::starts_with(f())`, `any_of(f())`,
`all_of(f())`, and `last_col(f())` each fail a type check — `match` must be a
character vector, `offset` a single integer, `all_of()`'s argument a subscript
— with the specification bound to a formal, of the helper or of an internal
function it delegates to: `all_of()` reaches `as_indices_impl()`, which is
tidyselect's and not exported, so excluding exported frames alone does not
reach it.

What separates the two is where the exported frame sits. `eval_select()` is
exported and is the entry, so every frame is under one; the first exported
function reached *after* it is a helper the caller wrote, and everything deeper
is that helper's. Stopping the scan there answered `FALSE` for all five and
left the three failing shapes above answering `TRUE`.

`where(fn)` was measured separately because it does not fail: it returns a
closure of its own that calls `fn`, so the specification is never a binding of
a tidyselect frame and the scan answers `FALSE` whatever the stopping rule is.

## What a scan cannot separate

A condition names the subscript it refused, so #190 can compare that label with
the argument's own and decline to speak for a part of an argument. A frame scan
has no such label. Measured against tidyselect's walk, `c(f(), grade)` and
`-f()` apply the predicate from a part, and both leave the same frames on the
stack as a bare `f()` does, so the two are indistinguishable from the frames
alone. What separates them is the caller's expression: `walk_data_tree()`
descends into `c`, `-`, `:`, `!`, `&`, `|`, `/`, and `(`, refuses `&&`, `||`,
`*`, `^`, and `~` before descending, and evaluates every other call whole.
