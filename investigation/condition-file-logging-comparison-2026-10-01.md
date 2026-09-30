# Comparing cooperative condition capture with file logging

Investigated: 2026-10-01
Base commit: `6092c36e1558023740bf294a7afe17ca868de77d`
Scope: #756/#764 alternatives; evidence and recommendation, not an adopted API

## Comparison

The [earlier alternatives](competing-condition-contract-alternatives-2026-10-01.md)
recommended ordinary R dispatch with an explicit outer-handler boundary (A),
plus caller cooperation where stronger capture was needed (C). The file proposal
was evaluated against that combination, not assumed to be an improvement.

| Proposal | Distinct benefit | Remaining limitation and added cost |
| --- | --- | --- |
| A plus a catcher inside the caller's callback | Preserved native dispatch; retained the operation condition and the cooperating callback's failure separately, without package-owned I/O | Required control of the callback; older noncooperating handlers and caller restarts could still bypass the catcher |
| Save the cooperative result afterwards | Persisted both captured outcomes and warnings the callback had observed; storage policy stayed with the caller | Nothing was saved by this step if control never returned; the helper did not collect every buffered warning after its first callback failure |
| Package snapshot before ordinary replay | Preserved an earlier interrupt/error and buffered warnings even when a later noncooperating handler replaced the outcome | Could not include a callback failure that had not happened; did not change the escaping condition; added serialization and file-failure policy |
| Package file output instead of replay during abnormal termination | Avoided that phase's warning callbacks | Removed their observation/muffling opportunity; changed warning delivery and needed a discoverable record plus explicit write-failure semantics |

Pre-replay logging therefore had a real advantage over saving a returned
cooperative result: it could retain evidence before an uncooperative transfer.
It was a complement to A, not a transparent implementation of the stronger
original-object condition contract. Replacing replay was a different delivery
policy, corresponding to alternative B; replacing it only for pending interrupts
left the preceding-error/handler-interrupt case outside that change.

For #764, the assessment favored A plus caller cooperation and optional
caller-owned persistence. It did not establish a need for package-owned
persistent diagnostics sufficient to justify a new file interface. A separate
requirement to recover earlier causes after uncooperative transfers would be a
reason to reconsider pre-replay logging. These were recommendations, not proof
that the unamended #756 specification was satisfied.

## Primary-source constraints

R documented [calling-handler availability][conditions] independently of output
destinations; writing a file did not alter that rule. R also documented that
[serialization][serialize] preserves reference sharing within one serialized
object, not across separate serializations, and may change format over time.
[R Internals][internals] described ordinary environment serialization through its
enclosure and contents. Consequently unrestricted condition serialization could
include more data than its printed diagnostic; this was a design implication,
not a measurement of real user secrets. [Writing R Extensions][pointers] stated
that an external pointer reloaded from a save was set to C `NULL`.

[`saveRDS()`][rds] offered object persistence, not a universal portable diagnostic
schema. Its documentation warned against using its files as a machine-interchange
format. [Connections documentation][connections] described close-time warnings
that could indicate write failure and the possibility of losing buffered output.
These sources supplied no crash-durability guarantee for the proposed logging.

## Measurements and reproduction

The [bundle](condition-file-logging-comparison-2026-10-01.rds) retained the probe,
the helper extracted unchanged from an uncommitted `recipes.qmd`, that complete
recipe source, source MD5 hashes, raw observations, output, and exit status 0.
The recipe SHA-256 was
`5f88bbfe6d37d22edbc5c0eba12a55ef263903bf51f29395bb08e4dc2e082f63`;
the base commit alone did not identify this dirty documentation snapshot.

The probe passed 31 named checks on R 4.6.1, aarch64 macOS. Public local summaries
demonstrated cooperative capture and pre-replay persistence. The latter replaced
the reporter only inside the probe process; it was not a package file option.
The RDS round trip
retained measured classes, messages, parent, payload contents, and internal
environment sharing, but not identity with the original environment. A serialized
SQLite connection payload restored invalid while its original remained valid.

At `warn = 1/2`, a naive failed file open invoked an outer warning handler whose
failure bypassed the writer's catcher. A logging-owned warning handler that
muffled that warning allowed the write error to be caught separately and the
pending interrupt retained. Thus conventional writer warnings were mitigable;
they were not evidence that file logging was impossible. A controlled interruption
from a serialization hook left an existing but unreadable file. No actual SIGINT,
disk-full fault, close-time fault, crash, concurrent writer, or other OS was tested.

```r
checkout <- normalizePath(".")
b <- readRDS("investigation/condition-file-logging-comparison-2026-10-01.rds")
out <- tempfile("condition-file-comparison-"); dir.create(out)
for (name in names(b$files)) writeBin(b$files[[name]], file.path(out, name))
setwd(out)
stopifnot(system2(file.path(R.home("bin"), "Rscript"), c("probe.R", shQuote(checkout))) == 0L)
```

## Acceptance needed for a package file interface

A future proposal would need to fix: append versus replacement delivery; capture
timing and record scope; a versioned schema and unsupported-payload behavior;
an explicit destination, collision/concurrency rules, retention and size bounds;
and failure status independent of the outcome already being propagated. Tests
would need to distinguish a completed readable record from partial output, retain
cleanup and transaction guarantees through writer failure/interruption, and
preserve promised warning/restart behavior. User-defined rendering or serialization
hooks would introduce another execution boundary. Bounded diagnostic fields and
unrestricted original objects were different contracts; neither could be called
crash-safe on the evidence collected here.

[conditions]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html
[serialize]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/serialize.html
[internals]: https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Serialization-Formats
[pointers]: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#External-pointers-and-weak-references
[rds]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html
[connections]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html
