# Review-ready files and automatic cleanup candidates

Investigated: 2026-11-01
Revised: 2026-11-01 — inventory extended by investigation/review-ready-evidence-retention-2026-11-01.md after PR #1115

## Scope and method

This inventory examined the Review-ready implementation at
`a2820e0faf42472dcb714a2e7355e081a15122c6`, its registered artifact verifiers,
contract/process fixtures, and selected test/backend helpers. It combined
static source reading with read-only filesystem metadata observations. No
check, fixture, installation, recovery, or deletion was executed for this
investigation. The requested cleanup scope included all generated side
products, including logs, control records, copied sources, rendered documents,
temporary installations, backend data, and caches.

The tables inventoried fixed names where the producing code specified them.
Directories and patterns covered variable outputs from R CMD check, renderers,
tests, Git, and backend libraries. They were not claims that every third-party
version generated an identical finite filename list. The inventory also
distinguished a committed source file from the copy created in a request's
disposable checkout. Cleanup of the latter would not authorize deletion of the
original repository file.

The user had accepted automatic collection at runner startup and shutdown,
without cron or a resident service. The retention interval was still open;
30 days had not been accepted. Recommendations below were deletion candidates,
not deletion instructions or evidence that a particular existing directory was
safe to remove.

## Locations, classes, and proposed dispositions

| Name | Location or meaning |
| --- | --- |
| `S` | OS account home plus `.local/state/marginplyr/review-ready/`, shared across all clones and worktrees |
| `Q` | `S/requests/<32-hex-request-id>/` |
| `T` | One R process's `tempdir()`, usually an `Rtmp*` directory below the OS temporary root |
| `W` | `T/marginplyr-review-ready-<random>/`, the R gate's workspace |
| `A` | `W/check-attempt-<random>/`, one package-check attempt |
| `D` | `tools::R_user_dir("marginplyr", "cache")/review-ready-failures/<UTC-start>-<SHA-prefix>-<random>/` |
| `G` | `T/marginplyr-generated-<random>/`, generated-documentation verification |
| `V` | `T/marginplyr-site-<random>/`, site verification |
| `P` | `T/review-ready-roxygen-<random>/`, the R-source equivalence proof |
| `F` | A fixture-owned temporary root; prefixes are listed below |
| `U` | A Python `TemporaryDirectory()` root in OS temporary storage; its generic `tmp*` basename alone did not establish ownership |

`Temporary` meant remove the whole owned directory after its writers and
owned processes stopped; preserve enough ownership information to recover an
interrupted invocation. `Retained` meant a completed-history candidate for the
chosen retention period, including successful, failed, and cancelled outcomes.
`Protected` meant active or uncertain operating state whose age alone did not
establish safe deletion. `Shared` meant a cache or location requiring attribution
or a dedicated namespace before automatic removal. `External` meant an input,
host-owned artifact, or separately produced evidence outside automatic
Review-ready ownership.

Cleanup columns recorded the cleanup route expressed in the inspected code.
They did not constitute a new test of cleanup under SIGKILL, OS failure, or a
third-party crash.

## Supervision, control files, and the common installation

| Artifact and class | Creation/location and condition | Existing cleanup and persistence | Proposed disposition | Source |
| --- | --- | --- | --- | --- |
| `S/`, `S/requests/` infrastructure | Common CLI startup; status/recovery also established `S` | Containers remained | Keep small containers; collect individual requests | [admission], `main()` |
| `S/admission.lock`, control | Full admission/recovery; permanent shared lock target | Descriptor closed; file deliberately remained | Protected while that lock protocol operated. Retire only through an exclusive, drained migration | [admission], `lock_file()`, `acquire_full()`, `run_pinned()` |
| `Q/manager.lock`, control | Request creation and recovery | Descriptor closed; file remained with terminal request | Retained with completed request; remove the request only after excluding a live owner | [admission], `main()`, `recover_request()` |
| `Q/record.json`, outcome/control | Initial request, state transitions, terminal completion, recovery | Terminal record remained; no age/count collector | Retained after terminal handoff; Protected while unfinished, unreadable, or uncertain | [admission], `save()`, `persist()`, `finish_request()` |
| `Q/record.new`, interrupted-write intermediate | Replacement of `record.json` | Successful replace consumed it; interrupted write could leave it | Remove with the safely completed/recovered request, not by basename or modification time alone | [admission], `save()`, `active_records()` |
| `S/request.json`, compatibility control | Mirror while Full capacity was owned; older standalone request journal | Matching completion/recovery removed it | Protected until matching ownership was resolved; include in old-runner cutover | [admission], `persist()`, `remove_full_mirror()`, `recover_request()` |
| `S/request.new`, interrupted-write intermediate | Replacement of compatibility journal | Consumed by successful replace; interruption could leave it | Collect only after resolving corresponding old/new ownership | [admission], `save()` |
| `Q/output.log`, combined log | Supervisor appended gate and child stdout/stderr across worker epochs | Remained after terminal cleanup and recovery | Retained; all outcomes eligible after writers stopped and terminal handoff | [admission], `supervise()` |
| `Q/workspace/supervisor.py`, code snapshot | Request preparation copied launcher bytes | Entire workspace removed at confirmed completion; uncertainty preserved it | Temporary; remove whole workspace | [admission], `run_request()`, `clean_workspace()` |
| `Q/workspace/source/**`, copied checkout | Shared Git clone checked out pinned submitted SHA; included all tracked paths and `.git` | Whole workspace removed at completion; promotion removed this subtree before re-cloning | Temporary. Includes copied research data, documents, and generated artifacts; do not follow Git alternates into the original clone | [admission], `run_pinned()`, `run_request()` |
| `Q/workspace/selection.dcf`, private transport | Before gate start; contained request-bound selection | Removed with workspace | Temporary control data; omit secrets from inventory output | [admission], `run_pinned()`, `selection_transport()` |
| `Q/workspace/roxygen-path`, proof result | Supervised R-source proof | Removed with workspace | Temporary control data | [admission], `run_pinned()`; [roxygen], CLI boundary |
| `/tmp/marginplyr-review-<id>.sock`, IPC endpoint | Per-request Unix socket, including process fixtures | Owned socket unlinked by confirmed cleanup; interruption could leave it | Resolve request ownership and writer shutdown before unlinking | [admission], `run_request()`, `clean_workspace()` |
| `~/.local/bin/marginplyr-review-ready`, installed executable | Existing installer replaced standalone launcher | Persisted until another installation/manual removal | Old-runner migration artifact; remove/retire only at the accepted drained cutover | [installer], `main()` |
| `~/.local/bin/tmp*`, installer intermediate family | `NamedTemporaryFile` in destination directory | Successful replacement/finally removed intermediate; interruption could leave it | Only an attributed installer intermediate was a candidate. The broad `tmp*` namespace was not exclusively Review-ready-owned | [installer], `main()` |

Status and gate-proof/promotion traffic did not create a dedicated status
report file in the inspected implementation; status printed JSON to stdout.
Recovery updated existing state and removed owned workspace/socket files.
[admission], `main()`, `rpc_request()`, `recover_request()`

`cleanup = confirmed` did not mean that every generated file was deleted. In
the inspected request protocol it accompanied removal of the disposable
workspace and owned channel, while `record.json`, `output.log`, and
`manager.lock` remained in the terminal request directory. [admission],
`finish_request()`, `clean_workspace()`

## R gate, package build/check, and live reporting

| Artifact and class | Creation/location and condition | Existing cleanup and persistence | Proposed disposition | Source |
| --- | --- | --- | --- | --- |
| `W/source.tar`, archive | Every gate path archived committed source | Gate `on.exit()` removed `W` | Temporary, whole `W` | [gate], `archive_review_ready_source()`, `run_review_ready_check()` |
| `W/source/**`, copied sources and side effects | Every gate path; spelling/lintr and selected assertions used this copy | Removed with `W` | Temporary, including any test-created files inside this copy | [gate], `run_review_ready_check()` |
| `W/<package>_<version>.tar.gz`, source package | Full/Documentation `R CMD build` | Removed with `W`; retries used the same tarball | Temporary after final attempt and evidence extraction | [gate], `build_review_ready_tarball()`, `run_review_ready_rcmdcheck()` |
| `W/offline-repository/src/contrib/PACKAGES`, empty index | Package checking; one local index served four repository names | Removed with `W` | Temporary data, not a log or downloaded package cache | [gate], `review_ready_offline_repositories()` |
| `A/console.log`, combined R-level check output | Every source-tarball attempt | Removed with `W`; failed attempt copied it to `D` | Temporary original; retained copy covered by bundle policy | [gate], `review_ready_check_attempt()`, `copy_review_ready_logs()` |
| `A/check/test-progress.tsv`, progress events | Reporter start appended file-completion events and final marker | Removed with `W`; the failure-copy suffix filter did not include `.tsv` | Temporary telemetry; include its whole directory rather than only `*.log` | [progress], `review_ready_progress_check()`; [reporter], `review_ready_progress_reporter()` |
| `A/check/*.Rcheck/**`, check output family | Each Full/Documentation attempt; names varied by package/R/tool versions | Removed with `W`; selected text copied on failure | Temporary complete directory, including hidden/generated files | [gate], `review_ready_check_attempt()`; [rcmdcheck background source](https://raw.githubusercontent.com/r-lib/rcmdcheck/main/R/background.R), `rcc_init()` |
| `00check.log`, `00install.out`, `tests/*.Rout`, `tests/*.Rout.fail`, `tests/*.Rout.save`, `*-Ex.Rout`, `*-manual.log`, `Rdlatex.log`, `build_vignettes.log`, other `*.log` | Members of `*.Rcheck/**` depending on reached stages | Original files followed check-directory cleanup; only the failure-copy allowlist was retained | Temporary originals. Do not treat the listed names as an exhaustive third-party filename list | [gate], `copy_review_ready_logs()`; [CI helpers], `test_output_path()`; metadata observations below |
| Installed package, extracted package/test inputs, example scripts/output, manual/vignette HTML/PDF/TeX and support files under check/build directories | R CMD build/check, package installation, examples and vignette rebuilding | Enclosed by disposable build/check directories | Temporary directory families, regardless of extension | [gate], `build_review_ready_tarball()`, `review_ready_check_attempt()`; [R checking manual](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Checking-packages) |
| R-session `Rtmp*` children and package-specific temporary files | Each independent R process; package/backend output names were dynamic | Normal process/temp cleanup and explicit unlink hooks; interrupted leftovers were possible | Route future child temporary roots into the request workspace, then collect by ownership | [R temporary-directory documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/tempfile.html) |
| rcmdcheck temporary session-info output and synthetic profile | When its `R_TESTS` setup path was used | `rcc_parse_results()` removed `private$tempfiles`; interrupted parsing could leave them | Temporary; cover library-managed files through the owned child temporary root | [rcmdcheck background source](https://raw.githubusercontent.com/r-lib/rcmdcheck/main/R/background.R), `rcc_init()`, `rcc_parse_results()` |

The failure copier selected `console.log` and files below `check/` matching
`([.]log|[.]Rout([.]fail|[.]save)?)$`. It did not preserve every side product:
for example `test-progress.tsv`, `00install.out`, PDFs, databases, and package
installations were outside that suffix set. A cleanup inventory therefore had
to cover directory ownership, not reproduce this diagnostic-copy filter.
[gate], `copy_review_ready_logs()`

## Retained source-tarball failure and retry bundles

| Artifact and class | Creation/location and condition | Existing cleanup and persistence | Proposed disposition | Source |
| --- | --- | --- | --- | --- |
| `D/metadata.txt`, `error.txt`, `stdout.txt`, `stderr.txt`, `check-findings.txt`, `native-crash-guidance.txt` | Every failed check attempt with available diagnostic preservation; metadata included versions | Outside disposable workspace; no automatic pruning | Retained bundle candidate, with one policy for all outcomes including a later retry pass | [gate], `preserve_review_ready_failure()` |
| `D/console.log`, `D/check/**` matching the copy filter above | Available check/console/test/vignette text from failed attempt | No automatic pruning | Retained with the entire bundle | [gate], `copy_review_ready_logs()` |
| `D/attempt-identity.txt`, `attempt-selection.txt` | After successful preservation of a failed attempt | No automatic pruning | Retained with bundle; includes tarball/renderer identity and check path/arguments | [gate], `run_review_ready_rcmdcheck()` |
| First bundle's `retry-result.txt`, `retry-identity.txt`, conditional `retry-findings.txt` | Authorized native-crash retry, including pending state before attempt two | No automatic pruning; retry pass preserved first failure | Retained with the connected retry evidence, avoiding partial deletion during retry | [gate], `record_review_ready_retry()` |
| First bundle's `retry/console.log`, `retry/check/**` matching copy filter | Successful second attempt | No automatic pruning | Retained with first bundle | [gate], `record_review_ready_retry()` |
| A separate second `D` directory | Failed second attempt with preserved diagnostics | Both bundles remained; first summary named second | Collect related terminal bundles together, after their last writer and retention decision | [gate], `run_review_ready_rcmdcheck()`, `record_review_ready_retry()` |
| Partially written `D/**` | Preservation exception or abrupt termination | Available files could remain; incomplete preservation prevented retry | Attribute to a request, confirm no writer, then include in automatic cleanup rather than retaining partial bundles forever | [gate], `run_review_ready_rcmdcheck()` |

The inspected operational guide explicitly retained failed-attempt bundles
until manual removal. Automatic retention-based collection would amend that
policy; it would not be an already implemented behavior.
[local checks: Failed source-tarball checks](../design/agents/local-checks.md#failed-source-tarball-checks)

## Generation, site rendering, and eligibility proof

| Artifact and class | Creation/location and condition | Existing cleanup and persistence | Proposed disposition | Source |
| --- | --- | --- | --- | --- |
| `G/source.tar`, `G/source/**` | Mapped documentation-generation verification | Whole `G` unlinked on exit | Temporary complete copy | [generation], `run_review_ready_generated()` |
| `G/library/**`, including installed package and interrupted installation lock/metadata families | README generation installed the copied tree with an isolated library inherited by child processes | Whole `G` unlinked on exit | Temporary; collect library as a unit after package/render processes stopped | [generation], `run_review_ready_generated()` |
| `G/source/README.md`, `NAMESPACE`, `man/*.Rd`, `man/figures/**` and unexpected generation outputs | roxygen/README execution regenerated publications; stale README figures were removed before comparison | Removed with `G`; changes caused a mismatch rather than copying them into caller worktree | Temporary generated copies; original tracked artifacts were inputs | [generation], `run_review_ready_generated()`, `review_ready_generated_hashes()` |
| `G/source/**/.quarto/**`, rendered intermediates, knitr figure/support/cache families | Quarto/knitr during README generation; exact members depended on tools/content | Removed with `G`; `.quarto` was excluded only from publication comparison | Temporary; comparison exclusion did not make this cache persistent | [generation], `review_ready_generated_hashes()` |
| `V/source.tar`, `V/source/**`, `V/library/**` | Changed supported site configuration; installed submitted package before rendering | Whole `V` unlinked on exit | Temporary complete site workspace | [site], `verify_site_artifacts()` |
| `V/source/docs/**`, generated HTML/PDF/assets/search/support files; `altdoc/freeze.rds`, `altdoc/pkgdown.yml`, Quarto project/cache/freeze families when emitted | altdoc/Quarto site renderer; membership depended on renderer | Enclosed by `V` and deleted with it | Temporary rendered copies; do not remove caller's publication/source files | [site], `verify_site_artifacts()`; [site configuration](../altdoc/quarto_website.yml) |
| `P/1.tar`, `P/2.tar`, `P/base/**`, `P/submitted/**` | Supervised R-source equivalence proof | Whole `P` unlinked on exit | Temporary, including both archived source trees | [roxygen], `review_ready_roxygen_path()` |
| Proof's output result path | CLI proof result, normally `Q/workspace/roxygen-path` | Request workspace cleanup owned the production result | Temporary control data, not a permanent report | [roxygen], CLI boundary; [admission], `run_pinned()` |

## Contract, artifact, and package-test fixture families

| Artifact family and location | Creation condition and concrete members | Existing cleanup/persistence | Proposed disposition | Source |
| --- | --- | --- | --- | --- |
| `T/review-ready-progress-*/**` | Mock process boundary: `test-progress.tsv`, `fixture.Rcheck/tests/testthat.Rout` renamed to `.Rout.fail` | Local unlink-on-exit hook | Temporary whole fixture | [gate verifier], first progress-process fixture |
| `T/review-ready-progress-smoke-*/**` | Real small-package checks: `fixture/**`, `offline-repository/src/contrib/PACKAGES`; `pass/`, `fail/`, `worker/`, `teardown/`, `startup/`, their check outputs and `test-progress.tsv` | Local unlink-on-exit hook | Temporary whole fixture, including real temporary package installations | [progress fixtures], enclosing `local()` |
| `T/review-ready-suggests-*/**`, `review-ready-rin-*/**`, `review-ready-offline-*/**` | Dependency/guard source fixtures, `DESCRIPTION`, copied guard, R/Rin sources, and empty `PACKAGES` index | Script declared unlink-on-exit hooks; all within R session temp storage | Temporary whole fixtures | [gate verifier], dependency and offline-index fixtures |
| `T/review-ready-diagnostics-*/**` | `fixture.tar.gz`, source fixture, `workspace/`, exception workspace, `retained/**` bundles, `failed-cli.R`; synthetic `00check.log`, `recipes.log`, `tests/test.Rout.fail`, `core`, `native.ips`, Windows executable-path fixtures | Script declared cleanup of fixture root; retained bundles were fixture-local, not the real diagnostic cache | Temporary whole fixture. Synthetic native files were ordinary test data | [gate verifier], diagnostic/process fixtures |
| `T/review-ready-retry-*/case-*/**` | Mock tarball, `Quarto [fixture]/bin/{quarto,deno,quarto.js}`, `workspace/check-attempt-*/**`, `retained/**` and retry metadata | Local unlink-on-exit hook | Temporary whole fixture; fake executables were not installed tools | [gate verifier], `make_retry_fixture()` |
| `T/review-ready-selection-*` | Selection transport negative fixtures | Explicit unlink-on-exit hook | Temporary control/test data | [gate verifier], selection-transport fixture |
| `T/roxygen-eligibility-fixtures-*/**` | Disposable Git repository, `.git/**`, R sources, DESCRIPTION/NAMESPACE, added/deleted input variants; nested proof workspaces as above | Local unlink-on-exit hook | Temporary complete repository | [roxygen fixtures], enclosing `local()` |
| `T/documentation-assertions-*/**` | Helpers/setup/teardown and selected/unselected synthetic test files | Local unlink-on-exit hook | Temporary complete fixture | [documentation fixtures], assertion fixture |
| `T/documentation-check-fixtures-*/**` | `valid`, `malformed-help`, `codoc`, `example` source packages, tarballs, offline index, `check/*.Rcheck/**` | Local unlink-on-exit hook | Temporary including actual build/check artifacts | [documentation fixtures], package-check fixture |
| `T/documentation-generation-fixture-*/**` | Temporary Git package, generated README/help/figures/NAMESPACE, `.quarto` and other renderer outputs, synthetic `R/generated.R`; nested `G` workspaces | Local unlink-on-exit hook | Temporary complete fixture; no original repository artifacts | [documentation fixtures], generation fixture |
| `T/marginplyr-coverage-policy-*/**` | `R/f.R`, `codecov.yml`, `.covrignore` exclusion cases | Script declared unlink-on-exit hook | Temporary test inputs | [coverage verifier], top-level fixture |
| `T/doc-reference-paths-*` | NUL-delimited `git ls-files` capture | Function unlink-on-exit hook | Temporary file, not a user report | [references], `read_tracked_paths()` |
| Path from `GITHUB_STEP_SUMMARY` | Reference/context/invocation/site verifiers appended Markdown only when this environment variable was set | No Review-ready deletion; normally a CI job-owned file | External unless explicitly redirected into the request; do not delete inherited job summary | [CI helpers], `write_step_summary()` |
| `T/marginplyr-release-fixture-*/**`; `T/cran-release-old-*`, `cran-release-new-*`; `cran-release-write-*` beside fixture files | Release-helper verifier: synthetic repository/Git metadata, modified DESCRIPTION/NEWS/README copies, preview inputs and atomic-write intermediates | Root/temporary-file unlink hooks | Temporary fixtures; separate formal release evidence was outside this run | [release verifier]; [release helpers], `release_print_diff()`, `release_write_atomic()` |
| `T/unsafe-output*`, `T/safe-output-link*`; `T/marginplyr-checktor-baseline-*/**` | Preflight path-validation and baseline negative fixtures, including a symlink | R temp storage; baseline root had unlink hook | Temporary only with known ownership; never follow fixture symlink into arbitrary data | [preflight verifier], output-path and baseline fixtures |
| `T/marginplyr-preflight-fixture-*/**` | Synthetic package/repository, tarballs, unpacked trees, candidate check/library directories, `preflight-driver.R`, evidence subdirectories | Script declared fixture-root unlink hook | Temporary complete fixture, not a real retained CRAN preflight | [preflight verifier], preflight CLI/build/evidence fixtures |
| Preflight fixture logs and evidence members | `success.log`, `unsafe-output.log`, `interrupt.log`, `mutation.log`, `clean.log`, `dirty.log`, `wrong-root.log`, `command-failure.log`; `summary.md`, `results.dcf`, `steps.tsv`, `worktree-before.txt`, `worktree-after.txt`, `candidate-library-identity.dcf`, `SHA256SUMS`, `git-archive.log`, `build.log`, `checktor-report.md`, `checktor.log`, `rcmdcheck-console.log`, conditional diagnostic/NOTE tables and full check directories | Inside fixture roots, with parent cleanup; exact reached members depended on case | Temporary parent directory; avoid an extension-only allowlist | [preflight verifier]; [preflight helpers], `write_preflight_evidence()`, `build_candidate_tarball()`, `run_preflight_pipeline()` |
| Arrow dataset/test files in `T` | `margin-partitions-*`, `margin-hidden-partitions-*`, `margin-directory-*`, `margin-directory-nested-*`, temporary `*.parquet`, partition subdirectories, `facts.parquet`, `data.parquet`, `row-*.parquet`, numeric `*.parquet` | Test/helper cleanup and R-session temporary storage; dynamic members varied | Temporary request-owned test storage after all backend readers/writers stopped | [Arrow helpers]; `tests/testthat/test-arrow-*.R` |
| SQLite and other backend temporary data in `T` | Temporary `*.sqlite` and backend-generated sidecars/journals/spill files when emitted | Explicit connection teardown/unlink in fixtures and normal R temporary storage | Temporary directory family, including variable backend sidecars, after disconnection/shutdown | [SQLite helper], `sqlite_interrupt_fixture()`; `tests/testthat/test-sqlite-*.R` |
| DuckDB extension/secret/temp storage below test R temporary directories | Guarded DuckDB tests used `shared_home = FALSE` | Connection shutdown and R temp lifecycle | Temporary owned test storage. Do not infer permission to erase shared `~/.duckdb` | [DuckDB helper], `duckdb_test_connection()`; [DuckDB storage test] |
| Temporary `*.rds` checkpoints and dynamic fixture sources/data | Independent-process condition/query tests, qualified-selection fixtures, and other suite-created temp files | Explicit `on.exit(unlink(...))`/test teardown or R temp lifecycle | Temporary test/child temporary root, without assuming every generated basename | [process test], `competing_condition_process()`; [sent-query process test], `sent_query_warning_process()` |
| `U/{repo,exempt,roxygen,worktree,clone,source}/**`, including `.git/**` and copied contract inputs | Python admission/reference/artifact fixtures created real Git worktrees and clones, renamed/symlink/submodule inputs and synthetic source/publication files | unittest cleanup/`TemporaryDirectory` contexts removed fixture roots; admission teardown stopped jobs first | Temporary entire fixture after owned processing stopped; never traverse Git alternates or symlinks into original sources | [admission fixtures], `Lifecycle.setUp()`, `tearDown()`, `References`; [artifact fixtures], `Artifacts.setUp()` |
| `U/home/**`, `U/bin/**`, fixture-installed commands and simulated `.local/state/**` | Private user-home adapter, test installer, and `Rscript`/Git/process/launcher shims | Removed with `U`; fixture installation did not replace real account installation | Temporary code/control/data copies | [admission fixtures], `Lifecycle.setUp()`, installer/entry-point tests |
| `U/log-*`, `backend.log`, `boot-*`, `delayed-read.log`; `*.lock`, `child.pid`, `manager.pid`, `ps-count`, `release-clone`, `preparation-*/child.pid` | Process fixture output, synthetic capacity/identity observations and synchronization files | Removed with `U` after job/child cleanup | Temporary whole fixture. A PID file alone did not authorize signaling a possibly reused PID | [admission fixtures], `start()`, management-parent/boot/promotion/cleanup fixtures |
| `U/check/fixture.Rcheck/00check.log`, `U/summary.md`, modified workflow/configuration files | CI artifact checker and log-summary fixtures | Some files explicitly removed during negative cases, remainder removed with `U` | Temporary fixtures; their summary file was fixture-owned, unlike an inherited CI summary | [artifact fixtures], `test_ci_summary_runner_preserves_status_and_all_diagnostic_details()` |
| `/tmp/marginplyr-review-<fixture-id>.sock` | Process fixtures used the production socket naming rule outside their simulated home | Admission fixture teardown explicitly unlinked recorded channels; abrupt death could leave them | Attribute to fixture request and exclude live users before deletion | [admission fixtures], `tearDown()`; [admission], `run_request()` |
| `<checkout>/tools/__pycache__/*.pyc` | Python source loaders in installation, registry loading, and fixtures could write bytecode when enabled/writable | No explicit bytecode collector; copied checkout cleanup removed enclosed caches | Temporary when enclosed in an owned copy. Original-checkout cache required attribution before deletion | [installer], `main()`; [CI artifacts], `registry()`; [admission fixtures], module loading |

Contract fixtures could generate real source packages and installed/check trees
even though they did not run marginplyr's full suite. Their temporary roots
therefore belonged in the inventory alongside logs. Verifiers that only read
configuration and printed diagnostics had no separately assigned report file;
their stdout/stderr flowed into the parent supervisor log. [gate],
`run_review_ready_verifiers()`; [CI artifacts], `verify()`, `run()`

## Shared caches, legacy artifacts, and external boundaries

| Artifact family and location | Source/observation and condition | Existing cleanup/persistence | Candidate boundary |
| --- | --- | --- | --- |
| Quarto project `.quarto/**`, freeze/support/output families inside `G`, `V`, build/check, and fixture copies | Renderer output inside owned source/project copies | Enclosing workspace cleanup | Include all owned copy contents, including hidden/cache files |
| macOS `~/Library/Caches/quarto/{file-storage,jt,sass}/**`; other renderer-created entries under the effective Quarto cache root | These subdirectories were observed locally. Installed Quarto 1.10.18's `quartoCacheDir()` selected the OS user cache; Linux implementation used `XDG_CACHE_HOME` or `~/.cache` | No Review-ready cleanup in inspected scripts | Shared. Identify a supported isolated namespace or exclusive attribution before automatic removal; changing `TMPDIR` alone did not relocate this shared cache |
| Deno's effective cache, such as macOS `~/Library/Caches/deno/**` or a selected `DENO_DIR` | Quarto/Deno execution; the earlier crash investigation separately changed `DENO_DIR` while Quarto's cache stayed shared | No general Review-ready cache collector | Include a request-owned `DENO_DIR` if configured; shared existing cache required separate ownership rules |
| lintr's effective `cache_directory`, conditional on enabling caching | Read-only installed metadata for lintr 3.4.0 showed `lint(cache = FALSE)` by default; `lint_package()` forwarded additional arguments. The gate supplied no `cache` argument, and the inspected repository had no `.lintr*` file | Cache generation was not established for this baseline; the default was disabled | An additional candidate if caching were enabled later. Place it in an owned namespace before collecting it; do not classify an unobserved cache as an existing output |
| `TMPDIR/marginplyr-admitted-*/source/**` from pre-current supervision | Historical legacy v1 `run_gate()` at commit `0897d03b` used a separate temporary source clone | Old normal completion removed it; uncertain execution could preserve it. Inspected v1 recovery removed the journal without recovering a workspace path absent from that journal | Migration-only legacy candidate after attribution and stopped ownership; generic prefix matching was insufficient |
| R cache `review-ready-build-failure-ec50f436/{console.log,identity.json}` | Two files, 20,709 bytes, found in the account cache; producing source path was not established | No collector established by this investigation | Historical/manual candidate requiring provenance, not an asserted current-runner output |
| Other account cache directories, including `arrow-layout-invariance`, `duckdb-2-compatibility`, `live-postgres*`, and `cran-preflight` | Root metadata scan found other task/tool names; Review-ready production was not established | Their own task/tool lifecycle | Do not purge the package's entire cache root as though every directory came from this gate |
| `investigation/sqlite-interruption-recovery-2026-09-30/{review-ready.log,suite-coverage.log,site.log,focused-tests.log,harness-development.rds,acceptance.rds,archive-hashes.json,manifest.json,summary.json}` | Committed, mixed manual investigation outputs | Git-tracked historical artifacts | Separate repository-maintenance decision, including citation updates where required; ordinary runtime GC must not delete them |
| `investigation/2026-10-30-review-ready-order/{baseline,descending}/*.tsv`, `files.csv`, `environment.txt` | Committed instrumentation and timing evidence | Git-tracked historical artifacts | Same distinction; their copies inside an owned request checkout were temporary |
| OS crash reports (`deno-*.ips`, platform event records/coredump metadata), possible crash dumps | Host crash facilities, not explicit Review-ready collection | OS/operator policy; failure bundles only wrote guidance | External. No Review-ready ownership or automatic-deletion authority established |
| Runner/helper source files, Git originals, installed Quarto/Deno/R distributions, existing developer package libraries | Inputs to execution; copied sources and temporary installed libraries were listed separately above | Maintained/installed independently | External inputs, not general side-product deletion candidates |

The Quarto finding came from read-only inspection of the installed
`~/.local/share/quarto/1.10.18/bin/quarto.js`, in functions corresponding to
the [v1.10.18 app-directory implementation](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/src/core/appdirs.ts).
The installed bundle had no `QUARTO_USERDATA` or `QUARTO_CACHE_DIR` override
strings. That search did not prove every future or undocumented override
impossible. The prior [native-crash investigation](quarto-deno-review-ready-crash.md)
also recorded the shared Quarto cache during isolated-Deno-cache experiments.
The lintr settings reference documented a separate
[cache_directory setting](https://lintr.r-lib.org/reference/default_settings.html).
That available setting did not establish that the inspected
`lintr::lint_package()` invocation wrote a cache. The baseline observation
combined the installed function formals, the call in [gate]
`review_ready_source_steps()`, and a hidden-file inventory of the repository.

## Filesystem metadata observations

Read-only scans on the investigation date found these aggregates. Paths,
request secrets, and request IDs were not dumped into the note.

| Location | Observation |
| --- | --- |
| Account Review-ready state | 24 request directories; 3,791 files and 34,123,062 bytes across the scanned state |
| Request states | 23 terminal requests with confirmed cleanup and no workspace; one running request with pending cleanup and a workspace |
| Running request workspace | 3,711 files, 32,130,654 bytes; included a full tracked source/research copy |
| Control-file families | 24 `manager.lock`, 23 `output.log`; no `record.new`; shared `admission.lock` and compatibility `request.json` existed, no `request.new` |
| Real failure-bundle root | 175 files, 6,235,896 bytes |
| Failure-file basenames | `00check.log`, `console.log`, `marginplyr-Ex.Rout`: 14 each; the fixed metadata/error/stdout/stderr/check-findings/native-crash-guidance/attempt-identity families: 13 each; `marginplyr-manual.log`: 13; `Rdlatex.log`: 12; `testthat.Rout.fail`: 9; `testthat.Rout`: 5; `build_vignettes.log`: 1; each retry result/identity/findings file: 1 |

These were point-in-time metadata observations of concurrently changing
storage, not proof that every existing file belonged to a completed request.
In particular, most scanned bytes were in the running request's workspace;
their presence did not demonstrate a terminal-cleanup leak. Basenames such as
research CSV files could be tracked source copies rather than logs.

## Development worktree and proposed artifact location

The dedicated development worktree for the discussion was
`/Users/sayuks/Documents/marginplyr/.worktrees/issue-1114`. It held the branch's
source and design changes. It was distinct from the check's disposable copies
at `Q/workspace/source`, `W/source`, `G/source`, `V/source`, and `P` above.
Automatic cleanup of a check did not authorize deleting that development
worktree or its original clone.

A proposed replacement layout could gather check-owned outputs beneath the
existing OS-account-home shared state:

```text
~/.local/state/marginplyr/review-ready/
  requests/<request-id>/
    record.json     # request identity, lifecycle and terminal result
    logs/           # combined output, progress and transcripts
    diagnostics/    # failed/retried attempt bundles
    work/           # pinned clones, archives, builds, renders, temp libraries
    tmp/            # child R/OS temporary files and fixture/backend data
    cache/          # caches with supported per-request isolation
```

This was a proposal, not an implemented or approved path migration. Actual
paths in the earlier tables described the inspected implementation. A request
layout alone did not relocate tools that selected shared external cache paths;
Quarto and OS crash-report ownership still required the boundaries above.

## Deletion candidates and remaining boundaries

The following proposal grouped artifacts by ownership instead of requiring an
ever-growing list of suffixes:

1. Collect the complete request-owned temporary tree after confirmed shutdown:
   copied Git checkouts, source archives/tarballs, check attempts, progress,
   rendered documents and intermediates, fixture repositories/data, temporary
   installed libraries, and request-specific caches. Use recoverable ownership
   records to handle interruptions.
2. Apply the selected finite retention policy to complete terminal request
   directories and failure/retry diagnostic bundles. Include partial bundles
   after attributing them and excluding active writers. No day count was
   selected by this investigation.
3. Retain only operating information needed while an invocation was live or
   its shutdown uncertain. Startup collection could first establish shutdown
   and recover it; age alone could not make a live process disappear. A shared
   empty lock target was operating infrastructure, not an accumulating history.
4. Attribute or isolate shared caches before including them. Removing only
   the namespace owned by a request or by the Review-ready tool could satisfy
   the cleanup objective without deleting other tools' data. Existing unowned
   temporary names needed evidence before retrospective removal.
5. Keep originals and externally owned artifacts outside the automatic sweep:
   original clones/worktrees and their tracked publications, installed tool
   distributions and developer libraries, separate formal release evidence,
   CI-owned summaries, and OS-generated crash reports.

Inference from the R temporary-directory contract: a future runner could set
request-scoped `TMPDIR` before each child R process started, so gate, fixture,
backend, and library-generated temporary files would be owned under one root.
Changing `TMPDIR` after R started would not relocate that session's `tempdir()`.
This covered libraries that used R/OS temporary storage, not every shared
application cache such as Quarto's separate cache above. It was a design
option, not an implemented or tested cleanup guarantee.
[R temporary-directory documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/tempfile.html)

The future evidence-reuse design required available origins and refusal of an
older success after a later matching failure. Finite collection could make
evidence unavailable and cause re-execution; it must also invalidate dependent
reuse and prevent deletion of a failure record from reviving an older retained
success. This inventory did not implement evidence reuse or its collection.
[ADR 0038: Evidence and invalidation](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md#evidence-and-invalidation)

## Revisions (2026-11-01)

The [evidence-storage investigation](review-ready-evidence-retention-2026-11-01.md)
extended this inventory to merged PR #1115 at
`be9e81e9c3d977b8a2607f3971468b5acf64faf1`. Evidence reuse had been implemented
there, adding authentication keys, complete issuance inventory, stage
observations, terminal evidence, reports, and timestamp inspection outputs.
The original findings above described the earlier pinned SHA. The added
investigation listed the new artifacts and established why independent file
deletion needed coordinated evidence retirement to preserve failure vetoes.

[admission]: ../tools/review-ready-admission.py
[installer]: ../tools/install-review-ready.py
[gate]: ../tools/review-ready-check-lib.R
[progress]: ../tools/review-ready-progress.R
[reporter]: ../tools/review-ready-progress-reporter.R
[generation]: ../tools/review-ready-generated.R
[site]: ../tools/verify-site-artifacts.R
[roxygen]: ../tools/review-ready-roxygen.R
[gate verifier]: ../.github/scripts/verify-review-ready-check.R
[progress fixtures]: ../tools/test-review-ready-progress.R
[roxygen fixtures]: ../tools/test-review-ready-roxygen.R
[documentation fixtures]: ../tools/test-review-ready-documentation.R
[coverage verifier]: ../.github/scripts/verify-coverage-policy.R
[references]: ../.github/scripts/verify-doc-references.R
[CI helpers]: ../.github/scripts/ci-helpers.R
[CI artifacts]: ../tools/verify-ci-artifacts.py
[admission fixtures]: ../tools/test-review-ready-admission.py
[artifact fixtures]: ../tools/test-ci-artifacts.py
[release verifier]: ../.github/scripts/verify-cran-release.R
[release helpers]: ../tools/cran-release.R
[preflight verifier]: ../.github/scripts/verify-cran-preflight.R
[preflight helpers]: ../tools/cran-preflight-lib.R
[Arrow helpers]: ../tests/testthat/helper-arrow-directory-partitions.R
[SQLite helper]: ../tests/testthat/helper-sqlite-interruption.R
[DuckDB helper]: ../tests/testthat/helper-duckdb.R
[DuckDB storage test]: ../tests/testthat/test-duckdb-storage.R
[process test]: ../tests/testthat/test-competing-process.R
[sent-query process test]: ../tests/testthat/test-sent-queries-process.R
