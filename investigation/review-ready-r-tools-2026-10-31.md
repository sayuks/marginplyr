# R tools for Review-ready execution and shorter feedback cycles

Investigated: 2026-10-31

## Scope and method

This investigation read first-party documentation and source code for existing
R development, testing, pipeline, profiling, and process-management tools. It
supported the design discussion for [#1114](https://github.com/sayuks/marginplyr/issues/1114).
It did not install packages, execute tests or checks, benchmark a replacement,
or establish a performance gain from changing the runner language. Repository
observations used commit `a2820e0faf42472dcb714a2e7355e081a15122c6` and the
in-progress design worktree. The note recorded evidence, not adoption decisions.

The reading distinguished three different jobs: quick feedback while editing,
execution of a package check, and justification for omitting previously
verified work. The reviewed tools supplied useful parts of those jobs, but
their documentation did not establish a ready-made implementation of
marginplyr's admission, lifecycle, and evidence policies. That last statement
was a finding about the sources examined, not a survey of every R package.

## Focused testing and the package boundary

As read on the investigation date, `devtools::test()` supported filename-regex
filtering, and `test_active_file()` mapped an active source file to its
corresponding test file. That mapping provided convenient focused execution;
the documentation did not describe a transitive change-impact analysis over
all R package dependencies. [devtools testing reference](https://devtools.r-lib.org/reference/test.html)

`testthat::auto_test()` documented a conservative watcher: changed source code
was reloaded and all tests rerun; otherwise new or modified test files were
run. It therefore did not supply an automatic proof that only one test file
could be affected by an arbitrary source edit.
[testthat automatic testing](https://testthat.r-lib.org/reference/auto_test.html)

`find_test_scripts()` filtered test-file stems with a regular expression, and
`test_file()` selected one file while also running helper, setup, and teardown
files in its directory. These were selection mechanisms, not certifications
that an omitted file was unaffected.
[file discovery](https://testthat.r-lib.org/reference/find_test_scripts.html),
[single-file execution](https://testthat.r-lib.org/reference/test_file.html)

`test_local()` loaded a source package through `pkgload::load_all()` by default;
`test_package()` targeted an installed package, and `test_check()` integrated
with `R CMD check`. `load_all()` simulated installation and loading without
first installing the package, with documented differences from an installed
namespace. This supported an inexpensive development loop but did not itself
exercise the installed source-tarball boundary.
[testthat package entry points](https://testthat.r-lib.org/reference/test_package.html),
[pkgload loading](https://pkgload.r-lib.org/reference/load_all.html)

`devtools::check()` built a source package and checked it in an independent R
session using `pkgbuild` and `rcmdcheck`. `rcmdcheck()` ran the external check,
parsed failures, accepted explicit check/build arguments and environment, and
supported a timeout. Neither reference described a general cache that proved
an earlier package check valid after arbitrary repository or environment
changes. [devtools checking](https://devtools.r-lib.org/reference/check.html),
[rcmdcheck reference](https://rcmdcheck.r-lib.org/reference/rcmdcheck.html)

For marginplyr, the separation was already explicit: focused tests were a
feedback loop, while the selected Review-ready path supplied the review
boundary. [Local checks: Roles outside the boundary](../design/agents/local-checks.md#roles-outside-the-boundary)

## Scheduling and measurement before more parallelism

The testthat parallel guide documented file-level scheduling. Workers loaded
the package and setup files, then accepted test files as workers became free.
Worker state persisted across files, so tests needed to restore global state
and avoid shared temporary-resource collisions. Startup, shutdown, and
communication overhead meant that more workers were not an unconditional
speed improvement. [testthat parallel guide](https://testthat.r-lib.org/articles/parallel.html)

The documented `Config/testthat/start-first` field and `start_first` argument
prioritized glob-matched file names, then appended remaining files. They
provided a standard way to dispatch slow files early without reducing the
test set. [test-file ordering](https://testthat.r-lib.org/reference/find_test_scripts.html)

At the inspected repository revision, Full already used four test workers and
`DESCRIPTION` prioritized all previously measured files. The existing
2026-10-30 investigation reported two admitted measurements: test-stage elapsed
time changed from 570 to 478 seconds, and the first-worker-file to last-file
span changed from 563.212 to 472.880 seconds, a reported 16.04% reduction.
Those were historical measurements from that note, not new measurements from
this investigation. Two runs established neither a confidence interval nor
an optimal or portable ordering.
[existing ordering evidence](review-ready-test-order-2026-10-30.md),
[Full check policy](../design/agents/local-checks.md#boundary)

`SlowReporter`, available through `devtools::test(reporter = "slow")`, reported
test-level durations and provided a packaged starting point for finding slow
tests. The reference contained inconsistent descriptions of its default
threshold, so no threshold value was relied on here.
[testthat SlowReporter](https://testthat.r-lib.org/reference/SlowReporter.html)

`profvis` visualized samples from R's profiler and documented sampling
variability and limits on visibility inside native code. Its FAQ also
documented profiling gaps such as sleeping time and the need for source
references to display package source.
[profvis introduction](https://profvis.r-lib.org/articles/profvis.html),
[profvis FAQ](https://profvis.r-lib.org/articles/faq.html)

Inference: stage/file wall-clock timing was the appropriate first measurement
for a gate involving SQL engines, Quarto, and child processes; profiling the
identified R hotspot could then explain R-side work. An R flame graph alone
would not establish end-to-end check latency or the best worker count.
This inference combined the profiler limitations above with the existing
[worker-timing method](review-ready-test-order-2026-10-30.md#collection-and-reproduction).

The first-party *R Packages* testing guidance recommended self-sufficient and
self-contained tests, with setup and cleanup that did not leak interactive
state into automated runs. That guidance supported reliable focused execution
and file reordering; it did not justify deleting backend or regression
coverage to achieve a shorter run.
[R Packages: testing principles](https://r-pkgs.org/testing-design.html#high-level-principles-for-testing)

## Incremental pipelines: useful machinery, explicit test contracts

`targets` represented computations as dependency graphs and skipped targets
whose recorded commands, dependencies, outputs, and other configured cues
remained valid. File targets could track external files, and the manual
recommended tracking external scripts explicitly instead of assuming the
pipeline could discover their contents through an external command.
[targets cues](https://docs.ropensci.org/targets/reference/tar_cue.html),
[target construction and dependencies](https://books.ropensci.org/targets/targets.html)

The package-invalidation chapter said that package functions were not tracked
by default. Its `imports` mechanism required installed packages rather than
only `load_all()`. The reference documented limitations for namespaced
`pkg::fun()` calls and compiled code. Consequently, merely wrapping
`rcmdcheck::rcmdcheck()` in a target did not demonstrate that the package tree,
all checking tools, or external environment had been registered as inputs.
[package-based invalidation](https://books.ropensci.org/targets/packages.html#package-based-invalidation),
[targets imports limitations](https://docs.ropensci.org/targets/reference/tar_option_set.html)

There was an additional distinction between computation success and check
success. `rcmdcheck()` defaulted to `error_on = "never"` and returned parsed
failures; a target's `error` option governed R errors. Inference: a naive cached
target could store an object describing a failed package check without the
target computation itself failing. A wrapper would need to apply the package's
ERROR/WARNING/NOTE and terminal-result policy before issuing reusable success
evidence. [rcmdcheck error handling](https://rcmdcheck.r-lib.org/reference/rcmdcheck.html),
[targets error handling](https://docs.ropensci.org/targets/reference/tar_target.html)

`drake::make()` also skipped up-to-date work and tracked declared files and
global objects. Its official successor documentation marked drake superseded
for new projects, with maintenance continuing, and directed new projects to
targets. This made targets the relevant pipeline candidate to evaluate for a
new design, without implying that a pipeline framework was necessary for a
package-development runner.
[drake make](https://docs.ropensci.org/drake/reference/make.html),
[targets on drake's lifecycle](https://books.ropensci.org/targets/drake.html)

Inference: a pipeline could implement storage and invalidation for explicitly
registered coarse stages, but marginplyr would still have to define complete
inputs, trusted recognition, environment identity, time-dependent conditions,
successful execution, cleanup, and the distinction between executed and reused
work. The accepted evidence-reuse design required those properties and treated
build/check as one reuse unit. A cache hit alone was not that proof.
[ADR 0038: Evidence and invalidation](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md#evidence-and-invalidation),
[ADR 0038: Environment and time](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md#environment-and-time)

## Process and resource-management building blocks

The process-management reading found reusable components at several levels:

| Component | Documented facility as read on the investigation date | Interpretation for the runner |
| --- | --- | --- |
| `callr` and `processx` | Background R calls with result collection; external-process handles, output polling, exit status, signaling, and cleanup mechanisms. [callr](https://callr.r-lib.org/reference/r_bg.html), [processx](https://processx.r-lib.org/reference/process.html), [cleanup guide](https://processx.r-lib.org/articles/cleanup.html) | These supplied execution primitives; request identity, durable recovery, and confirmed release of shared capacity still required an explicit protocol. |
| `ps` | PID/start-time handles and environment-marker tree discovery and cleanup. The tree API documented macOS environment-inspection restrictions. [handles](https://ps.r-lib.org/reference/ps_handle.html), [tree cleanup](https://ps.r-lib.org/reference/ps_kill_tree.html) | Ownership checks could use these APIs, but their OS-specific preconditions and request-scoped behavior needed validation. |
| `crew` | Controllers submitted tasks to local workers; worker limits applied to controller auto-scaling, with documented exclusions for retry and manually launched workers. [local controller](https://wlandau.github.io/crew/reference/crew_controller_local.html) | Separate one-worker controllers in separate worktrees would not form one user-wide admission service. An outer worker limit would not cap every descendant of an internally parallel package check. |
| `mirai` and `future` | Persistent daemon/task execution and cancellation APIs. mirai documented that an interrupt did not guarantee task termination; future cancellation interrupted only when its backend supported it. [mirai daemons](https://mirai.r-lib.org/reference/daemons.html), [mirai cancellation](https://mirai.r-lib.org/reference/stop_mirai.html), [future cancellation](https://future.futureverse.org/reference/cancel.html) | Task cancellation could not by itself stand for confirmed shutdown of all owned check processes. Pool reuse was a possible optimization to measure, not a measured gain for this gate. |
| `parallelly` | Core-count inference considered scheduler/container limits and threads per worker. [availableCores](https://parallelly.futureverse.org/reference/availableCores.html) | Capacity discovery was not a reservation shared between independent CLI invocations; aggregate admission remained a separate responsibility. |

The interpretations in the table were deductions about how these documented
scopes compared with the repository contract, not claims that the tools were
defective. In particular, crew's own risk guide warned that crashes or network
errors could leave workers running. A new runner would still need cancellation,
manager-death, descendant, cleanup, and recovery fixtures rather than treating
worker-pool shutdown as equivalent evidence.
[crew known risks](https://wlandau.github.io/crew/articles/risks.html),
[shared admission: Cancellation and recovery](../tools/review-ready-admission.md#cancellation-and-recovery)

The locking reading also found a migration constraint: the reviewed filelock
Unix implementation used `fcntl(F_SETLK/F_SETLKW)`, whereas the inspected
Python runner used `fcntl.flock`. Linux's manual documented that flock and
fcntl locks did not interact on ordinary local filesystems. Therefore, using
the same lock pathname with filelock would not by itself coordinate the new R
runner with an old Python runner on Linux. Lock-family interoperability and
legacy-request migration needed an explicit solution and platform fixtures;
no interoperability experiment was performed in this investigation.
[filelock Unix implementation](https://raw.githubusercontent.com/r-lib/filelock/main/src/filelock-unix.c),
[Linux flock manual](https://man7.org/linux/man-pages/man2/flock.2.html),
[inspected admission implementation](../tools/review-ready-admission.py)

Even pipeline process status had narrower scope: `targets::tar_pid()` described
the most recent orchestrator for one project/store and recommended checking
whether it was alive before another run. Inference: selecting a common store
or reading that PID alone did not establish cross-clone, user-wide admission
or the repository's owned-descendant recovery contract.
[targets process status](https://docs.ropensci.org/targets/reference/tar_pid.html)

## Limits and implications for the design

The reviewed tools supplied focused tests, source loading, package-check
execution, duration reporting, profiling, and dependency-aware caching.
They addressed distinct costs. Reading their documentation did not establish
that changing Python supervision to R would shorten the package test stage,
or that adding targets would automatically satisfy the Review-ready reuse
contract. No new speedup estimate or replacement validation was produced.

For the design discussion, the evidence separated improving latency while
retaining every required test from deciding when prior stage evidence remained
applicable. The former already had a measured testthat scheduling example;
the latter remained governed by the explicit proof and invalidation rules in
[ADR 0038](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md).
