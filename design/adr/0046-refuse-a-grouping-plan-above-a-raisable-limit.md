# Refuse a Grouping plan above a raisable limit

A Margin verb and `inspect_grouping()` refuse a Grouping plan holding more than
4,096 grouping-set occurrences. The refusal is a Package condition raised while
the plan is prepared, before a query is sent or a branch is built, and
`options(marginplyr.max_grouping_sets = )` raises or removes the limit. #1745
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
answering into a condition that names the count.

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

## Why 4,096

The number is borrowed, not derived. PostgreSQL, SQL Server, BigQuery and
polars' SQL interface use it, PostgreSQL's source calls its own `CUBE` bound
arbitrary, and other engines chose 64, 128, 2,048 and 65,535. The measurements
support a range rather than a point: plan preparation stayed under two seconds
up to 65,536 occurrences and passed a gigabyte at 2^20.

Within that range 4,096 is the one value with a reason attached. PostgreSQL is
one of the two backends marginplyr sends native `GROUPING SETS` to, and its cap
is fixed in source; taking the same number means a plan marginplyr accepts is a
plan that native path accepts. A higher default would leave a band where the
same call passes on a local data frame and fails on PostgreSQL.

The cost of the lower choice is small because little above it was cheap. Past
4,096 occurrences PostgreSQL refused, DuckDB's native summary ran out of memory
at 8,192, and at 16,384 every other path took 81 seconds or more.

## Why one limit, applied to the plan

The cost per occurrence differed by two to three orders of magnitude between
paths, so no single count is a performance threshold, and this limit does not
try to be one. It is placed on the stage that is identical everywhere: preparing
a 65,536-occurrence plan took the same time on every backend that reached it.

Leaving lazy backends to their engines was considered and does not reach the
problem. An engine's cap acts on execution; the plan and, on every path except a
native summary, one branch per occurrence are built in R first. PostgreSQL
refused a 16,384-occurrence expansion only after seven minutes of client-side
construction, and dtplyr, Arrow, SQLite and DuckDB's expansion have no cap of
their own to defer to.

A table of limits per backend was rejected: the figures belong to dbplyr's,
Arrow's and each engine's version, and would be this package's to keep current.

## The unit

The count is of the occurrences the plan holds, which is what
`inspect_grouping()` returns one row for, and not of the occurrences the caller
wrote. #1245, #1274 and #1321 made a plan with few distinct grouping sets
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

## Why now

Adding a refusal after release breaks calls that worked; removing or raising one
breaks nothing. The limit therefore goes in before the first CRAN release, where
both directions stay open.

Evidence: `investigation/2026-11-21-unbounded-grouping-set-count.md` records the
measurements across backends and the engines' own limits from primary sources.
