# Separate Package conditions from internal invariants

marginplyr raises a Package condition — an error inheriting `marginplyr_error`
— exactly when the caller can avoid it by rewriting the call within the
documented public interface. Everything else uses bare `stop()` or
`stopifnot()`: guards on invariants that are unreachable through the public
interface, and guards on defects in marginplyr itself or an upstream package,
where no change to the call helps. `marginplyr_error` is the only promised
public class; narrower subclasses stay implementation details. External
conditions propagate unchanged, and rethrows preserve the original condition
rather than reclassifying it.

The predicate is avoidability, not the origin of the cause. Arrow Parent shares
are rejected with a Package condition even though the message names marginplyr's
own limitation, because omitting `share_of_parent()` avoids it. The dbplyr
query-representation guard is not a Package condition even though its message is
written for a reader, because no call rewrite avoids it. `R/grouping-plan.R`
already embodied this split before it was written down: an unknown Grouping
specification kind reaching `validate_grouping_spec_early()` is a Package
condition, while the same check inside `expand_grouping_family()` is a backstop
behind that gate and stays `stop()`.

The base R `simpleError` class is deliberately not inherited. It was a
transitional shim that kept assertions written in f7ac9e7 green during the
expand–migrate–contract sequence, and it produced conditions claiming to be both
`simpleError` and `rlang_error` while carrying rlang fields a `simpleError`
handler does not expect.

## Considered Options

**Origin of the cause** — Package condition when the caller's input is at fault,
`stop()` when marginplyr or an upstream package is at fault. Rejected: it
misclassifies both boundary cases. Arrow Parent shares would become `stop()`
even though the caller can avoid them, and a forged Grouping specification would
become a Package condition even though the documented interface cannot produce
one.

**Catchability** — every error marginplyr can emit inherits `marginplyr_error`,
including internal invariants. This is the widest reading of "package-generated
errors" in the release specification. Rejected: it makes `marginplyr_error` mean
both "you can fix your call" and "marginplyr is broken", so a handler cannot use
the class to decide whether correcting the call is worthwhile, and it promises
internal invariants as public interface.

## Consequences

The rule is not mechanically enforced. `abort_marginplyr()` is the only
constructor for Package conditions, so the audit is a review of the bare `stop()`
and `stopifnot()` sites, each of which must be unreachable from the public
interface or attributable to a defect. Both halves of the boundary need the same
review attention: a Package condition demoted to `stop()` silently drops out of
the public contract, and an invariant promoted to `abort_marginplyr()` silently
enters it.
