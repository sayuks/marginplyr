# Refuse a Grouping plan above a raisable limit

A Margin verb and `inspect_grouping()` refuse a Grouping plan holding more than
4,096 grouping-set occurrences. The refusal is a Package condition raised while
the plan is prepared, before a query is sent or a branch is built, and
`options(marginplyr.max_grouping_sets = )` moves or removes the limit. #1745
implements it.

The limit is one number for every backend and every verb, and it exists for one
reason: a plan is built in the caller's R session, its size is exponential in
what the caller wrote, and nothing downstream is positioned to stop it.

## What the limit is for

A `cube()` of n columns is 2^n occurrences, and the list is built in R whatever
the backend. Building it for 24 columns passed 1 GB of memory in about three
seconds on a two-row data frame, and a time limit around the call ended it on
time without bounding that memory. No backend sees such a request: PostgreSQL's
own refusal of more than 4,096 grouping sets is raised when the statement is
prepared, which is after the plan and the statement exist.

A caller does not have to write 24 names to get there. `cube(where(is.character))`
on a wide table does it, and so does an application that passes its user's column
choice to `cube(all_of())`. The limit turns that from a session that stops
answering into a condition, which is only true if refusing is cheap: the
refusal is decided without building the plan it refuses.

## What the limit is not

It is not a defence for an application that builds a Grouping specification from
untrusted input, and the documentation says so rather than letting the limit
imply it. At 4,096 occurrences a summary still took minutes to construct on the
slower paths, an expansion returns rows × occurrences, and a portable summary
reads its source once per occurrence. What bounds those is the application
limiting how many columns can be chosen, the database's own limits, and a time
limit around the call.

It is not a cost policy either. ADR 0020 and ADR 0025 decide when the caller's
data is read and by whom, and neither is extended here; the limit is on the
plan's size, which is known before any data is involved.

Nor does it stand in for a fix to how plans are composed. The audit's 14-column
SQLite case finished, and construction grew close to linearly in the number of
occurrences on every path, so there was no complexity defect for a limit to
hide.

## Why 4,096

The number is borrowed, not derived. PostgreSQL, SQL Server, BigQuery and
polars' SQL interface use it, PostgreSQL's source calls its own `CUBE` bound
arbitrary, and other engines chose 64, 128, 2,048 and 65,535. The measurements
support a range rather than a point: plan preparation stayed under two seconds
up to 65,536 occurrences and passed a gigabyte at 2^20.

Within that range 4,096 is the one value with a reason attached. PostgreSQL is
one of the two backends marginplyr sends native `GROUPING SETS` to, and its cap
is fixed in source; taking the same number means a plan marginplyr accepts is
within that path's cap. A higher default would leave a band where the
same call passes on a local data frame and fails on PostgreSQL.

The cost of the lower choice is small because little above it was cheap. Past
4,096 occurrences PostgreSQL refused, DuckDB's native summary ran out of memory
at 8,192 at two of the three thread counts tried, and no branch-building path
finished 16,384 in under 44 seconds.

## Why one limit, applied to the plan

The cost per occurrence differed by two to three orders of magnitude between
paths, so no single count is a performance threshold, and this limit does not
try to be one. It is placed on the stage that is identical everywhere: preparing
a 65,536-occurrence plan took the same time on every backend that reached it.

Leaving lazy backends to their engines was considered and does not reach the
problem. An engine's cap acts on execution; the plan and, on every path except a
native summary, one branch per occurrence are built in R first. PostgreSQL
refused a 16,384-occurrence expansion only after seven minutes of client-side
construction, and nothing refused on dtplyr, Arrow, SQLite or DuckDB's
expansion at any size reached. For a caller who raises the option, an engine's
own refusal stays an External condition and propagates unchanged.

A table of limits per backend was rejected: the figures belong to dbplyr's,
Arrow's and each engine's version, and would be this package's to keep current.

## The unit

The count is of the occurrences the plan holds, which is what
`inspect_grouping()` returns one row for. Branches and SQL bytes were the other
candidates; each exists on some paths only, and only once the cost it would
bound has been paid. Nor is it a count of the occurrences the caller wrote. #1245, #1274 and #1321 made a plan with few distinct grouping sets
complete under `.duplicates = "drop"` however many repeats were written, and a
count taken before that would refuse what they made work.

## Considered options

**No limit, documentation only.** The position this decision replaces. #980 kept
a plan-size policy out of its own scope without promising unbounded plans, so
nothing had decided it. It rests on the cost being the caller's own and
interruptible, which holds for the time a plan takes and not for the memory it
takes, nor for a caller who is an application acting on someone else's input.

**A warning.** Rejected. A warning arrives after the plan is built, which is
after the cost it would warn about.

**A limit with no way to raise it.** Rejected. A plan above 4,096 occurrences
finishes on a local data frame, and a caller who means to wait for one says so
once.

**An argument on each verb.** Rejected in favour of the option. The limit guards
a session, not one call's result, and an option changes only whether a condition
is raised, never what a successful call returns.

## Why the default refuses

Adding a refusal after release breaks calls that worked; removing or raising one
breaks nothing. Starting with the refusal is the choice that leaves both
directions open.

Evidence: `investigation/2026-11-21-unbounded-grouping-set-count.md` records the
measurements across backends and the engines' own limits from primary sources.

## Amendment: a product under `.duplicates = "drop"` (2026-11-21)

Under `"drop"` a Cartesian product is deduplicated one factor at a time, and a
later factor can merge sets an earlier prefix kept apart:
`grouping_spec(cube(a1, ..., a13), grouping_sets(grouping_set(a1, ..., a7),
grouping_set(a8, ..., a13)))` holds 191 sets after a prefix of 8,192. Counting
the plan exactly there means building the prefix, which is the cost the limit
exists to refuse, so #1745 refuses a product whose deduplicated prefix passes
the limit, and such a plan is refused although it would be within it. The
message names the constructor whose family passed the limit, `cube()` or
`grouping_spec()`, rather than a count of the plan. Every other refusal holds to the
unit above.

## Amendment: which refusal comes first (2026-11-22)

A refusal preflight raises, such as an empty or named constructor argument,
comes before this one, and so does one a family raises about itself before it
is built, such as a column one `cube()` selects in more than one argument.
Under `"keep"` and `"error"`, expansion reads families in the order written
and builds each before reading the next, so a later family's own refusal comes after an earlier family's
refusal for size: `grouping_spec(cube(a, b, c, d), cube(x, x))` over a limit
of 8 is refused for the first cube. A `grouping_sets()` union whose branches
pass the limit likewise stops before its next branch, since reading it would
mean building it, or, for a selection that needs a lazy input's column types,
querying the input first.

## Amendment: what a `"drop"` product costs (2026-11-22)

Under `.duplicates = "drop"` a product crosses every prefix set with a factor
at once (#1749). A `cube()` is crossed one unit at a time, so its cost follows
the sets kept. A family resolved in full, such as a `rollup()` or the written
sets of a `grouping_sets()`, is crossed pair by pair, so its cost follows the
occurrences `"keep"` would write, while the limit bounds only the sets kept:
`grouping_spec(rollup(a1, ..., a1000), rollup(a1, ..., a1000))` keeps 1,001
sets and took about four seconds. For such a family, construction is close to
linear in the occurrences written, not in those kept.

Crossing every prefix set at once decides two refusals. A `cube()` crossed
with more than one prefix set is built only as part of the product, so a
refusal there names `grouping_spec()`. A product nested in a `grouping_sets()`
branch is crossed with the enclosing prefix sets factor by factor, so its
prefix counts toward the limit as it would if the product were written flat:
such a plan is refused once that prefix passes the limit, although a later
factor would merge it back within it. With the limit raised past that prefix,
such a plan costs what its flat form costs: `grouping_spec(cube(a1, ..., a8),
grouping_sets(grouping_spec(cube(b1, ..., b12), grouping_sets(...))))`, whose
inner union holds the twelve sets of eleven `b` columns, keeps 3,328 sets
after a prefix of 2^20 and took about a minute nested or flat.
