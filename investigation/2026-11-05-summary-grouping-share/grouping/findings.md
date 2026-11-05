# Grouping specification metadata and expansion boundaries

Investigated: 2026-11-05
Baseline: `f158df41168bbf0b64bce01518fcff1bd88cb11f`

R 4.6.1 ran on aarch64 macOS with dplyr 1.2.1, tidyselect 1.2.1,
rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1, DuckDB 1.5.5,
dbplyr 2.6.0, DBI 1.3.0 and pkgload 1.5.3. Standalone scripts
loaded the pinned source. The live DuckDB probe owned one in-memory
connection and removed its DBI trace at exit.

The November 4 Grouping family algebra, tidyselect, expansion-invariant
and nesting-boundary investigations identified the initial targets.
Merged #1284, #1286 and #1297 provided current controls. Earlier
measurement counts were kept separate from this investigation.

## Names-only literal and lexical selectors requested typed metadata

An immutable derived dtplyr step added `extra` with a counted character
callback. In `.by` and a `grouping_set()` argument, `c(a, NULL)`,
`c(a, all_of(NULL))`, and direct `.env$pick` or `.env[["pick"]]` with
`pick <- "a"` all resolved to `a` under ordinary tidyselect. All eight
cases agreed on eager input and safe dtplyr roots. All eight raised the
unsafe typed-metadata refusal on the derived input, even though a bare
`a` and `all_of(.env$pick)` selected it there. The true predicate control
also raised the unsafe-metadata refusal. The source callback was never called.

The absent-column counterparts on live DuckDB each made one
`dbSendQuery()` request containing `LIMIT 0` before reporting the
missing column. Bare and wrapped-pronoun controls reported the same
missing name without a request. The trace counted method entry and
did not measure rows scanned. ADR 0005 puts a locally decidable
error before backend metadata acquisition. The inspection reference
permits name-only inspection of a derived dtplyr input.

The literal classification treats `NULL` as needing types because R's
`is.atomic(NULL)` is false. The direct pronoun is outside the classifier
used by the wrapped, supported helper argument. One ticket can pin both
spellings and preserve typed controls without promising arbitrary caller
expressions.

## Explicitly qualified parentheses hid ordinary lexical evaluation

The caller bound `c <- function(...) is.character` and selected
`` base::`(`(c("a")) `` from an input with character `a` and integer `b`.
Ordinary tidyselect treated this as a call in the caller's environment,
used the returned predicate, and selected `a`. On a dtplyr root, the
same selector in `grouping_set()` reported the empty grouping set.
A counted predicate received four integer proxy values, two in each
pass. The explicit `where()` control received character and integer
columns and selected `a`.

Using the same selector as one branch of `grouping_sets()` with an
explicit empty `grouping_set()` caused a local false duplicate error
at positions 1 and 2. The predicate received two integer proxy values
before the refusal. The typed `where()` control returned `a` followed
by the empty set. Additional controls isolated the same boundary
through a caller-bound `.data` and, independently, a caller namespace
operator returning a predicate-producing replacement for the explicit
parenthesis. The minimized replay needs only the ordinary lexical
`c()` binding and no masked operator or constructor.

The classifier stripped an explicit base-qualified parenthesis and
treated its interior as a tidyselect walk. The actual selector still
carried the qualified call and tidyselect evaluated it as ordinary R.
The ordinary bare-parenthesis selection semantics, trusted metadata
when types are genuinely needed, and the grouping-plan correspondence
in ADR 0013 were the discriminants.

## An identity product inside a union lost the outer product's coverage

At 18 distinct keys and `R_MAX_VSIZE=96M`, inspection crossed a cube
with one grouping set containing all its keys under the duplicate-drop
policy. Each of the cube's 262,144 memberships becomes the same complete
set. A direct cube, a cube wrapped in a one-factor product, and a cube
wrapped in a single-branch union each returned the exact expected plan
in both factor orders. When a single-branch union wrapped the one-factor
product wrapping the cube, both factor orders reached the vector memory
limit. All eight six-key controls returned the exact same one-row plan
as the single grouping set, including Grouping bits, identifier, and
dimension order.

The enclosing product carries covered columns to a direct cube and
through an intervening union. A product used as a union branch is
expanded through the ordinary fallback first, with none of the
outer product's coverage. This follow-up distinguished that additional
nesting boundary from #1245 and #1274. The capped run established the
observed failure at that setting. It did not estimate a universal
supported width.

## Duplicate disposition

Read the bodies and available comments for #1265 (lexical pronouns
inside helpers), #1277 (direct selection-context names and indexed
lookups), #1274 (cube covered by a direct surrounding product), #1245
(repeated units and repeated product factors), #1288 (retention of an
ordinary selection returned by a constructor-shaped call), #1039
(qualified helper obtained through a masked namespace operator), #995
(implicit S3 and vctrs dispatch) and #1233 (bare binary subtraction).
PR #1286 described successful direct and union-wrapped cube controls.
The current reproductions kept those related cases as working controls
where applicable. The ordinary explicitly qualified wrapper used no
namespace-operator mask, S3 dispatch, or constructor-shaped selector.

Compared all five then-open issue titles, the empty open PR list, 280
recent closed issue records and 110 merged PR titles. All-state Issues
and PR searches included `qualified parenthesis`, `NULL tidyselect`,
`nested product cube`, and `union cube memory`. The first found
#1288 and #1297. The other three returned no matches. GitHub rate
limiting interrupted some additional lexical-selector and name-only
queries. The listing and targeted report comparison remained available.

## Replay

Run `name-only-selection.R` and `qualified-wrapper.R` from the pinned
source root with Rscript. Run `nested-cube.R` with `R_MAX_VSIZE=96M`;
`--small` selects the six-key controls. Observation mode asserted the
dated failures and controls. The `--assert` modes asserted the desired
behavior and were deliberately red. The finite fixtures did not cover
other package versions, hosted SQL, or arbitrary caller functions.

The publication-time listing surfaced two reports absent from the first
listing. #1306 used the same 18-key nested product, both factor orders and
96 MiB cap. The duplicate #1314 was closed in favor of #1306. #1304 concerned
a caller replacing the namespace operator inside a helper's argument. The
ordinary lexical c binding under a genuine qualified wrapper remained the
separate witness recorded in #1315. The NULL and direct lexical selection
forms were recorded in #1313.
