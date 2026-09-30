# Source observations for the controlled signaling probe

Read: 2026-09-30. Measurements use installed R 4.6.1 and rlang 1.3.0; downloaded R trunk source is explanatory evidence, not an assertion about that binary build.

Base conditions documentation supports signaling S3 condition objects and exiting/calling handler dispatch by class. `signalCondition(rich)` is a generic notification rather than a default interrupt action. The measured route uses the documented rlang `interrupt()` only when generic notification returns; an exiting handler therefore receives the rich object before native fallback.

R `errors.c` functions `getInterruptCondition()`, `signalInterrupt()`, and `onintrEx()` create fresh native interrupt conditions, run matching handlers, then select native default actions. `signalInterrupt()` invokes the interrupt option when set; `onintrEx()` retains the transition fallback to the error option when the interrupt option is absent. Returning calling/global handlers receive a rich generic notification and then a bare native notification in the measured route. This is a disclosed protocol consequence, not a second user cancellation count.

The rlang 1.3.0 `R/cnd-signal.R` interrupt case calls `interrupt()` rather than forwarding the custom object. Its public interrupt documentation states custom interrupt objects cannot be supplied. Thus `cnd_signal(rich)` alone cannot preserve the rich cause fields.

The route demonstrates a supported mechanism using exported functions, no namespace tracing or modification, no R internals and no custom native extension. It does not prescribe implementation handler nesting or verify savepoint ownership, deferred SIGINT draining, warning-replay integration, second interrupt, native cancellation, other R/OS/dependency versions, or arbitrary handler nonlocal transfers.

`probe.R`, `run.py`, `results.json`, `summary.json`, `manifest.json`, and `environment.rds` hold runnable scripts, complete stdout/stderr/exit results, script hashes, and measured environment. `sources.json` holds source URLs, blob hashes, and the R mirror commit. Source `.c`/`.R` files are optional full-source audit evidence; API response blobs need not be archived.
