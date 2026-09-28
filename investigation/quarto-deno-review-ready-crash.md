# Quarto Deno crash during local Review-ready checks

Investigated: 2026-09-28
Primary input: [#712](https://github.com/sayuks/marginplyr/issues/712)

## Finding and limits

The investigation did not identify an upstream fix matched to the reported
crash. The two local reports agreed on the Deno executable image UUID and
leading Deno offsets. This established recurrence at the same crash site,
not its cause. An upstream macOS crash-report collection change described
similar intermittence, but supplied no matching UUID or native stack.
Neither a subsequent successful render nor an unsuccessful bounded attempt
to reproduce would establish that the failure had been corrected.

The upstream search and native-report inspection below ran without rendering
a document, running Review-ready, changing installed software, changing OS
reporting settings, or copying a native report or memory image.

## Local native-report evidence

The following existing reports were read in
`~/Library/Logs/DiagnosticReports/`; only identifying metadata and stack
offsets were recorded here.

| Report | Capture time | Faulting thread |
| --- | --- | --- |
| `deno-2026-09-24-091840.ips` | `2026-09-24 09:18:38.5802 +0900` | 11, `tokio-runtime-worker` |
| `deno-2026-09-28-101402.ips` | `2026-09-28 10:14:00.8546 +0900` | 12, `tokio-runtime-worker` |

Both recorded macOS 26.6.2, build `25G83`, and `EXC_BAD_ACCESS`, `SIGSEGV`,
`KERN_INVALID_ADDRESS at 0x0000000000000000`. Their arm64 Deno image UUID was
`4c4c4435-5555-3144-a10b-c1c14dc2d9bc`. The first frame had offset zero in a
different image; the following Deno offsets were identical:

```text
29888468 29886848 29888192 23717972 2798292
2796796 22493920 2794828 2880900
```

On the investigation date, the installed binary
`~/.local/share/quarto/1.10.18/bin/tools/aarch64/deno --version` reported
Deno 2.7.14, V8 `14.7.173.20-rusty`, and TypeScript 5.9.2. `dwarfdump --uuid`
on that binary returned the matching UUID. `nm -n` exposed only 170 named
symbols; `atos` on the first three Deno offsets, using the binary's
`0x100000000` image base, returned addresses without function names. Those
attempts did not identify a source function or establish an upstream match.

[#712](https://github.com/sayuks/marginplyr/issues/712) associated the September
28 failure with `recipes.qmd` in a source-tarball check of commit
`53446493aa2e4bf82e539000fe8629d3331a61f9`. It also recorded that a later render
and complete Review-ready invocation passed. The September 24 report's
relationship to Review-ready was not independently established. The reports
did not record enough shared cache or process state to test the
repeated-invocation hypothesis retrospectively.

## Upstream sources inspected

The search covered Quarto and Deno GitHub issues and pull requests using
`segfault`, `segmentation`, `SIGSEGV`, `macOS crash`, the UUID above, and the
first Deno offset `29888468`, together with web searches for the installed
versions. Exact UUID and offset searches in both issue trackers returned no
matches. This was a bounded search, not proof that no matching upstream report
existed. The most relevant results were:

- [Quarto #10426](https://github.com/quarto-dev/quarto-cli/issues/10426)
  recorded intermittent macOS Deno segfaults with Quarto 1.6.3 / Deno 1.41.0,
  and a later report with Quarto 1.6.39 / Deno 1.46.3. One reporter associated
  occurrences with stopping and immediately restarting preview. Maintainers
  lacked a consistent reproduction; closure did not identify a fix. This was
  evidence of an older similar symptom, not evidence for shared-state causation
  in marginplyr.
- [Quarto PR #14945](https://github.com/quarto-dev/quarto-cli/pull/14945),
  merged on 2026-09-24, added macOS crash-report collection to smoke-test CI.
  Its author described intermittent Deno exit 139 about 40 ms after startup,
  before reading the input, on different documents in successive attempts.
  It changed reporting, not the crash mechanism. Its body did not provide a
  native stack or UUID that could match the local reports. Its optional
  `RUST_BACKTRACE=full` setting was described as affecting Rust panics and
  potentially adding nothing for a segfault.
- [Quarto #10920](https://github.com/quarto-dev/quarto-cli/issues/10920)
  concerned FreeBSD, Quarto 1.6.15, and an unsupported Deno 1.46.2 build.
  It supplied no macOS 2.7.14 crash identity or applicable fix.
- [Deno #33374](https://github.com/denoland/deno/issues/33374) described
  intermittent SIGSEGV during `deno test` shutdown when
  `perf_hooks.monitorEventLoopDelay` activated a `tokio-eld` sampling task.
  [PR #33375](https://github.com/denoland/deno/pull/33375) fixed that
  use-after-free by updating `tokio-eld` to 0.3.0. The
  [Deno v2.7.14 lockfile](https://github.com/denoland/deno/blob/v2.7.14/Cargo.lock)
  already selected 0.3.0. A shared `tokio-runtime-worker` name did not match
  the local stack to that defect, and that patch was not an unapplied fix for
  the bundled release.
- [Deno PR #34561](https://github.com/denoland/deno/pull/34561) corrected
  arm64 Apple FFI fast-call stack argument alignment. Its author explicitly
  reported no observable change while V8's argument-count guard remained in
  place. Nothing inspected connected that code path to the local offsets.

The [Quarto v1.10.18 configuration](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/configuration)
selected Deno 2.7.14. Its
[1.10 changelog](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/news/changelog-1.10.md)
associated that update with a silent crash on older Windows builds, not this
macOS failure. The
[1.11.5 changelog](https://github.com/quarto-dev/quarto-cli/blob/v1.11.5/news/changelog-1.11.md)
and [Deno release history inspected at `b157cd27`](https://github.com/denoland/deno/blob/b157cd27e153f0a3e2ef10a6a31edfd105fcbf93/Releases.md)
did not establish a matching fix either. These findings did not justify a
particular version pin or skipping vignette checks.

## Portable native-report guidance verified

Apple's [Console guide](https://support.apple.com/guide/console/reports-cnsl664be99a/1.1/mac/26)
identified `.ips` as the crash-report extension, distinguished user and system
reports, and documented revealing a selected report in Finder. The candidate
`~/Library/Logs/DiagnosticReports/deno-*.ips` location was confirmed by the
local reports above and also used by Quarto PR #14945. A candidate file needed
correlation by time, process, and executable identity; its name alone did not
prove association with a check. Console's Crash Reports view and Reveal in
Finder provided a fallback when that candidate directory did not contain the
report.

Microsoft's [application-crash guidance](https://learn.microsoft.com/en-us/troubleshoot/windows-server/performance/troubleshoot-application-service-crashing-behavior)
identified the actual application crash as Event ID 1000, source
`Application Error`, in the `Application` log. Its example included time,
faulting application and module, exception code, offset, process ID, paths,
and report ID. Event Viewer at Windows Logs > Application therefore supplied
a native metadata lookup for the failing executable near the check time.
The document's separate dump-collection and registry-configuration steps
were not needed to inspect an existing event.

The [systemd v250 `coredumpctl` manual source](https://github.com/systemd/systemd/blob/v250/man/coredumpctl.xml)
distinguished `list` and `info`, which read saved journal metadata, from
`dump`, which extracted a memory image, and `debug`, which invoked a
debugger. It supported executable-name or path matches and `--since` /
`--until` bounds. Thus `coredumpctl --no-pager list deno` and
`coredumpctl --no-pager info deno`, narrowed to the failed invocation's time
and PID, were suitable manual metadata lookups. Metadata could outlive the
core file. Missing matches returned nonzero; visibility depended on journal
access. The hosted manual returned HTTP 403 during this investigation, so
the source XML was read through GitHub instead.

For all three platforms, an unavailable facility, missing event/report, or
access restriction left the native evidence unavailable; none implied that
the check had succeeded. The portable evidence was the retained check logs,
timestamp, committed SHA, executable/version metadata, and process error or
exit state. A user could consult their existing system reporting facility
without enabling dump collection or changing OS settings. This was a
conservative interpretation of the documented facilities, not a claim that
every OS installation generated a native crash report.

## Local experiments

The upstream-source investigation above performed no check or render
repetitions. Any same-SHA experimental run needed its order, start/end times,
versions, outcome, available report identity, and shared cache/process state
recorded here; fresh disposable check directories alone would not vary the
shared state identified in #712.
