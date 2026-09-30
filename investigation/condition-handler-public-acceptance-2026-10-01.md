# Public warning-handler acceptance and the global-handler candidate

Investigated: 2026-10-01
Source snapshot: 15f6064310ad45f6548c0299d92e908617b088e9
Scope: unresolved #756/#764 acceptance; no production implementation

## Public-operation result

The [executable acceptance oracle](condition-handler-public-acceptance-2026-10-01.R)
ran twelve cases through `summarize_with_margins()`. It terminated with exit
status **1**: six cases failed the accepted
[specification](../design/specs/competing-conditions.md), and six controls
passed. Failure is the oracle's result, not an expected-failure exemption or
complete acceptance. The measured package source was unchanged.

| Case | Result |
| --- | --- |
| External warning-handler error, `warn = 1/2` | Handler error escaped; first cancellation and `$replay_error` were lost |
| External warning-handler interrupt, `warn = 1/2` | Second interrupt escaped instead of the first |
| Execution error followed by external warning-handler interrupt, `warn = 1/2` | Interrupt escaped without its execution-error parent |
| Muffling, `warn = 1/2` | First interrupt retained; both distinct warnings replayed in order, with the repeated-warning count |
| Returning warning handler, `warn = 2` | Conversion error retained separately from first interrupt |
| Summary called from an already selected older warning handler | Selected and newer handlers remained unavailable during replay |
| Direct invocation of the registered callback | Callback remained available for replay |
| Caller restart token obtained before the summary | Original restart remained invocable and its handler ran once |

All cases reached the later branch after effects `2, 5, 2, 5, 2, 5`.
The input, warning option, and original error objects were unchanged; a later
valid summary succeeded in each case. Failure cases stopped after the first
replayed warning. The raw records retained original objects, parent chains,
environment payloads, effects, events, and per-assertion verdicts. The outcome
was captured before message rendering or the subsequent summary.

The availability and restart controls exercised the public operation, extending
the isolated-callback counterexamples in the
[earlier native-API investigation](condition-handler-native-api-2026-10-01.md).
They provided checks for a future candidate without asserting which private
handler structure it must use.

## Additional public-API candidate

A pre-registered `globalCallingHandlers(error = ...)` callback recovered an
external warning-handler error through a package restart when no older exiting
error handler was present. Adding the ordinary caller
`tryCatch(..., error = identity)` made that catcher receive the original error;
the global callback observed nothing. Registering a global handler from inside
a dynamic-handler scope was refused. These probes completed with exit 0,
confirming the candidate's counterexamples, not #756 acceptance.

The [R condition documentation][conditions] described global handlers as a
last resort after dynamic handlers. Its calling-handler restriction also
explained why an inner catcher was unavailable inside an older caller handler.
The [documented native cleanup APIs][cleanup] did not provide the escaping
condition to an unwind callback. An additional exit-frame probe found neither
the failed caller-handler frame nor a condition-valued `returnValue()`.

This candidate did not meet the contract. The probes did not establish that
every possible supported architecture was impossible. They supplied no reason
to weaken ADR 0035 or advertise its unimplemented guarantee.

## Reproduction and limits

Run the oracle from the candidate checkout, supplying an output path:

```sh
Rscript investigation/condition-handler-public-acceptance-2026-10-01.R /tmp/handler-acceptance.rds
```

Exit 0 requires all twelve cases to pass; the recorded snapshot returned 1.
This was a focused local-summary oracle, not the complete SQLite, native-hook,
or SIGINT acceptance matrix. It used controlled interrupts on R 4.6.1,
aarch64 macOS; it delivered no actual SIGINT and ran no other R version or OS.

The [compressed bundle](condition-handler-public-acceptance-2026-10-01.rds)
retained raw source, output, objects, terminal statuses, environment, and package
source hashes. Its bytes were checked after serialization. Extract it without
expanding the condition objects to text:

```r
bundle <- readRDS("investigation/condition-handler-public-acceptance-2026-10-01.rds")
target <- tempfile("handler-acceptance-")
dir.create(target)
for (name in names(bundle$files)) {
  writeBin(bundle$files[[name]], file.path(target, name))
}
observations <- readRDS(file.path(target, "acceptance.rds"))
vapply(observations$cases, `[[`, logical(1), "accepted")
```

Run the extracted `global-probe.R` in an independent R process with the
extraction directory as its working directory; it writes `observations.rds`.
The probe temporarily registers a global handler and restores the prior list
on its successful path.

[conditions]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html
[cleanup]: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Condition-handling-and-cleanup-code
