# Scope and alternatives for competing-condition preservation

Investigated: 2026-10-01
Repository snapshot: `15f6064310ad45f6548c0299d92e908617b088e9`
Scope: design investigation for #756/#764; no contract or production change

## Finding

The requirement to retain every original failure raised by an arbitrary outer
warning handler was an additional marginplyr policy, not a requirement imposed
by R, rlang, or dplyr. It was nevertheless explicitly accepted in
[ADR 0035](../design/adr/0035-preserve-competing-conditions-at-interruption.md),
the [specification](../design/specs/competing-conditions.md), and
[#756, User Story 9][issue]. Dropping it silently would not complete #756.
Reopening this explicitly unimplemented part was a design option, not a claim
that its acceptance tests had passed.

The evidence supported separating resource restoration and package-controlled
condition selection from guarantees about execution inside caller handlers.
No supported transparent implementation of the latter was established by the
[public acceptance oracle](condition-handler-public-acceptance-2026-10-01.md)
or the [native API experiments](condition-handler-native-api-2026-10-01.md).
Those counterexamples bounded the mechanisms tested; they did not prove that
every possible architecture was impossible.

## What the upstream contracts said

R specified that an invoked calling handler sees only handlers below itself.
Consequently an error raised inside an older caller warning handler bypasses
newer package catchers. A returning handler resumes dispatch; a restart can
transfer control to its establishment point. Respecting this dispatch means
accepting that the caller can take control outside the package's active
catchers. This is an inference from [R's documented rules][conditions], not
an upstream requirement for marginplyr's chosen error precedence.

R's warning conversion was a different boundary: only after warning handlers
returned did `warning()` apply `warn = 2`; `muffleWarning` bypassed that default
action. Protecting a pending interrupt from package-initiated conversion
therefore did not require intercepting failures inside an older handler.
The distinction followed from [`warning()`][warning] and was observed by the
public acceptance oracle's returning-handler and muffling controls.

rlang's [`try_fetch()` reference][try-fetch] and inspected
[`cnd-handlers.R`][rlang-source] retained calling-handler behavior. Recovery,
chaining, and inspection did not give it a stronger guarantee about failures
inside an already selected outer handler.

dplyr documented buffering warnings to avoid flooding the console and exposing
the original warnings through [`last_dplyr_warnings()`][dplyr-warnings]. In
the inspected [`summarise_cols()`][dplyr-summary], `signal_warnings()` followed
the evaluation block; it was not an exit action. An error or interrupt escaping
that evaluation block could not reach that later call. This was a source-flow
inference, not an upstream promise about every dplyr path. The
[comparison bundle](competing-condition-contract-alternatives-2026-10-01.rds)
recorded the corresponding probe on R 4.6.1, dplyr 1.2.1, and rlang 1.3.0:
one warning observation on success, zero after a later error or interrupt.
Its withr 3.0.3 comparison also observed a deferred cleanup error replace a body
interrupt without retaining it as a parent. These printed observations showed
no universal competing-cause guarantee; they were not a conformance suite or
evidence for other versions. The sources did not establish a general R/dplyr
requirement to notify every buffered warning on cancellation.

## Which guarantees justify their cost

This is a design assessment, not a new policy. Owned SQLite work, outer
transactions, and failed cleanup determine whether database state is trustworthy
independently of replay; [ADR 0031][adr31] already owned those guarantees.
Converting cancellation into an ordinary warning-derived error also frustrates
an interrupt handler, the package-controlled problem in [#756][issue]. Keep
those guarantees and the protected ownership boundaries.

Retaining a callback's own failure would improve diagnostics, but requiring
transparent capture of *any* outer callback while preserving all native handler
and restart semantics adds a different control-flow responsibility. It was not
needed to restore the savepoint. The oracle's failures still preserved input,
options, and a subsequent valid summary; they failed condition selection and
cause retention, not the oracle's state-restoration controls. This distinction
does not make the lost cancellation desirable; it identifies the tradeoff that
a revised contract would need to disclose.

## Alternatives and their exact scope

### A. Bound preservation by R's available-handler boundary

Keep normal warning replay, handler observation, muffling, ordering, and
`warn = 2`. Retain replay failures and further interrupts when package handlers
remain available. Explicitly exclude failures and transfers from an older
caller handler that bypass those handlers; let R propagate them normally.
This preserves the [native behavior][conditions] and avoids manipulating
caller handlers. It gives up retaining the first cancellation or preceding
execution error in precisely those bypass cases demonstrated by the oracle.

This smallest proposal still requires amending ADR 0035 and testing the limit.
The general nonlocal-transfer exclusion cannot silently erase the explicit
handler-error requirement in the same specification.

### B. Retain buffered diagnostics without signaling them during failure

After an abnormal outcome is established, attach the buffered warning records
to that outcome as structured diagnostics and do not call `warning()` during
its propagation. Healthy warning replay remains ordinary, including conversion
under `warn = 2`. This proposed design removes warning callbacks as a source of
replacement during that phase; it does not guarantee control over arbitrary
outer interrupt handlers or other user exit actions.

Unlike discarding warnings, this keeps the records already collected. It still
changes delivery: warning handlers would not observe or muffle those records,
and native top-level interruption would not automatically display an attached
list. The field and its identity/count/context format would become an additional
interface; it needs documentation and tests. ADR 0035 rejected dropping buffered
warnings, but did not evaluate this separate delivery-and-retention tradeoff.

Skipping replay only when an interrupt is already pending is insufficient for
all #756 cases. A pending execution error followed by an outer warning-handler
interrupt can still lose its earlier cause. Avoiding that path requires applying
the proposal to every pending abnormal outcome, thereby revisiting
[#754's ordinary-error warning reporting][ordinary-error], or explicitly retaining
A's exception for ordinary-error replay. This is a broader change than A.
The comparison bundle's process-local prototype verified only the interrupt
case at `warn = 1/2`: the original interrupt, warning, and repeated count survived
without callback execution. It did not implement or verify the broader proposal.

### C. Require caller cooperation

The comparison bundle's `guard_callback()` wrapped the callback's body in a
caller-owned catcher. After its first failure, it muffled that warning and later
warnings, returning the callback failure separately from the operation outcome.
At `warn = 1/2`, controlled errors/interrupts and self-delivered SIGINT retained
the first cancellation without a package change. Original caller restart tokens
remained usable. This was opt-in: warning-only work could return a value alongside
a captured failure, and the caller had to decide how to handle that pair.

The `restart-protocol.R` prototype added a package replay restart instead.
A cooperating callback handed its exact failure to it; package replay retained
the error as `$replay_error` while keeping the first interrupt. Muffling, a
captured caller restart, and nested handler-availability controls passed.
Noncooperating callbacks, including an older callback reached by a nested
warning, still bypassed capture. The new restart would be an additional public
protocol, not a transparent implementation of the arbitrary-handler requirement.

### D. Change the runtime capability or the release constraint

The [handler-interleaving experiment](condition-handler-replay-2026-09-30.md)
captured original callback conditions and retained the measured native behavior,
but used unsupported R internals and caused an `R CMD check` WARNING. CRAN's
[public API policy][cran-policy] explicitly excludes that access. Moving the
same dependency into compiled code would not make it supported.

A proposed R extension could provide a scoped observer below existing calling
handlers or expose the original condition through a supported unwind API.
The documented [`R_UnwindProtect()`][r-exts] supplied only a jump boolean to
cleanup; an extension would need condition, transfer-kind, and resumption
semantics. This was an upstream proposal, not an available implementation API.

### E. Report each first occurrence immediately

Earlier reporting would avoid cancellation-time replay, but the final repeated
count is unavailable until later grouping sets run. Retaining the count would
need another report; dropping it would change [ADR 0021][adr21]. A `warn = 2`
conversion or failing handler could also stop an earlier branch before later
summary effects occur. These are consequences of changing the buffering order
in [`buffer_branch_warning()` and `report_branch_warnings()`][package-source],
not measured acceptance. This was a broader compatibility change than A.

## Recommendation and limits

Recommend A for a compatible library: keep the established resource guarantees
and ordinary diagnostic handling, and state the precise outer-handler limit.
Where a caller needs stronger behavior, C's caller-owned adapter supplied an
opt-in route; add a package restart only if there is demand for that protocol.
If the product requirement instead makes cancellation precedence during warning
delivery stronger than warning-handler notification, evaluate B for all pending
abnormal outcomes. Neither recommendation authorizes a policy edit by itself.

Any replacement for the earlier failed candidates must address their recorded
handler-availability, restart-token, and original-object counterexamples.
The bundled prototypes used controlled conditions and self-delivered SIGINT,
not supervisor checkpoints. They did not establish production acceptance or
other R versions, platforms, native statements, or arbitrary callback coverage.

## Reproduction

The bundle retained scripts, raw observations, output, environment, package
source hashes, and terminal statuses. The ecosystem probe printed comparisons;
the cooperative script asserted its cases and counterexamples. Independent
reproduction of each returned exit 0. Run from the candidate checkout; only the
extracted copy's original checkout path is adapted below. The archived bytes
remain unchanged. The process-local reporter substitutions are experiments,
not edits to package source or the accepted specification.

```r
checkout <- normalizePath(".")
bundle <- readRDS("investigation/competing-condition-contract-alternatives-2026-10-01.rds")
out <- tempfile("marginplyr-contract-alternatives-")
dir.create(out)
for (name in names(bundle$files)) writeBin(bundle$files[[name]], file.path(out, name))
probe <- readLines(file.path(out, "probe.R"))
probe[1L] <- paste0("pkgload::load_all(", encodeString(checkout, quote = '"'),
                   ", quiet = TRUE)")
writeLines(probe, file.path(out, "probe.R"))
setwd(out)
for (script in c("ecosystem-probe.R", "restart-protocol.R")) {
  stopifnot(system2(file.path(R.home("bin"), "Rscript"), script) == 0L)
}
```

[conditions]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html
[warning]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/warning.html
[try-fetch]: https://rlang.r-lib.org/reference/try_fetch.html
[rlang-source]: https://github.com/r-lib/rlang/blob/c6c65ba174bf4d06a91bf63e4e535d351c0d51fa/R/cnd-handlers.R
[dplyr-warnings]: https://dplyr.tidyverse.org/reference/last_dplyr_warnings.html
[dplyr-summary]: https://github.com/tidyverse/dplyr/blob/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f/R/summarise.R
[r-exts]: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Condition-handling-and-cleanup-code
[cran-policy]: https://cran.r-project.org/web/packages/policies.html
[issue]: https://github.com/sayuks/marginplyr/issues/756
[ordinary-error]: https://github.com/sayuks/marginplyr/issues/754
[adr31]: ../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md#catchable-interruption-amendment-2026-09-30
[adr21]: ../design/adr/0021-report-a-repeated-execution-condition-once.md
[package-source]: https://github.com/sayuks/marginplyr/blob/15f6064310ad45f6548c0299d92e908617b088e9/R/conditions.R
