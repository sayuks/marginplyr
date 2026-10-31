# PR #1115 contract coverage in the repository-runner design

Investigated: 2026-11-01

## Scope and method

The audit compared [PR #1115][pr] at merge
`be9e81e9c3d977b8a2607f3971468b5acf64faf1` with the accepted repository-runner
design in the dedicated `design/1114-review-ready-language` worktree. The
maintainer confirmed that #1115 was the intended reference.

The inspection read the baseline [ADR][adr], [follow-up specification][followups],
[local checks][local checks], [shared admission][shared admission],
[evidence implementation][evidence], [admission implementation][admission],
[R gate library][gate], [evidence fixtures][fixtures],
[admission fixtures][admission fixtures], and [R gate verifier][gate verifier].
The audit was a static contract comparison. It did not execute a check or
fixture, implement a replacement, inspect a running request, or delete data.
It did not establish behavioral equivalence of an R implementation.

## Findings and design corrections

The runner design had covered the major inherited contracts: cumulative
obligations, original execution origins, actual-work capacity, child-environment
normalization, before/after identities, stage observations, issuance ordering,
failure vetoes, recovery, and the new retention/rotation/isolation intersections.
Many detailed conditions had been inherited through broad references, without
a case-level migration requirement. That left room for a port to preserve the
headlines while dropping their refusals or boundary cases.

The audit identified three concrete clarifications for the design:

1. **Comparison failure and missing authority needed separate treatment.**
   The baseline's `select_path()` selected Full for unavailable comparisons,
   retained no usable base, and required the CI artifact route when present.
   That route could refuse an unresolved comparison; Full did not manufacture
   evidence. The new design's broad source/objects-unavailable stop wording
   could be read as removing that conservative selection. The runner
   specification clarified it while retaining the accepted setup stop when
   the new main-based manager/registration authority itself was unavailable.
   Missing local main was an explicit authority-model amendment.
2. **Behavioral migration needed an inventory obligation.** Same user/repository
   and ancestor eligibility, Full-to-Documentation asymmetry, whole-source and
   external-input closure, exact NOTE/retry eligibility, authorized terminal
   outcome/observation consistency, pinned-origin revalidation, and the
   distinction between unavailable cache reads and interrupted publication
   were made explicit in the implementation acceptance matrix. Every baseline
   regression case needed preservation, an equivalent replacement, or an
   explicit design amendment, with corresponding new assertions recorded.
3. **Authentication and mixed-origin GC needed concrete acceptance cases.**
   The design required a versioned encoding/authentication contract, but had
   not named known-answer, wrong-key, tampering, schema, private-key metadata,
   and renewal verification cases individually. It also needed a retained
   request with both an expired inherited unit and a valid newly executed unit.
   Those were added. The baseline's `hmac.compare_digest` verification
   protections still required an evidenced R comparison mechanism; package
   adoption, equal-output tests, and timing measurements alone did not
   establish that mechanism.

The audit found no additional maintainer policy choice necessary to preserve
these existing behaviors. The corrections specified acceptance obligations;
they did not demonstrate that a replacement met them. The durable contract
and amendments belonged in the
[runner specification](../design/specs/review-ready-repository-runner.md),
with [ADR 0039](../design/adr/0039-run-review-ready-from-the-target-worktree.md)
owning the architectural decisions. The baseline sources above retained
ownership of their inherited rules, avoiding a second full textual copy.

## Evidence-fixture inventory

The pinned evidence fixture module declared 37 test methods. Its `load_tests()`
explicitly selected methods defined on its three case classes, so this count
did not include every inherited admission test. The admission fixture module
separately declared 70 methods. Counts were observations of that SHA, not a
coverage target or proof that assertions were equivalent.

The table mapped every declared evidence method to acceptance-case families
in the runner specification. It recorded design coverage, not an R test result.
The baseline admission and R gate cases also remained part of the required
implementation migration inventory. [fixtures], [admission fixtures], [gate verifier]

| Baseline evidence test method | Runner acceptance family |
| --- | --- |
| `test_1107_correction_preserves_strict_assertion_and_other_code` | Source and contract identity |
| `test_literal_vector_whitespace_preserves_the_dependency_regression` | Source and contract identity |
| `test_timestamp_boundaries_unknown_r_and_rollback` | Time-dependent eligibility |
| `test_r_probe_refuses_unregistered_date_and_clock_inputs` | Time-dependent eligibility; unknown source and external inputs |
| `test_r_probe_does_not_invent_an_empty_date_input` | Time-dependent eligibility |
| `test_mixed_branch_keeps_obligations_and_reuses_actual_origin` | Existing stage reuse; origin eligibility |
| `test_description_reporter_unknown_paths_modes_and_assertion_weakening_invalidate` | Source and contract identity |
| `test_missing_corrupt_partial_incompatible_and_nonancestor_records` | Origin eligibility; authentication protocol |
| `test_documentation_never_discharges_full_repository_has_no_package_claim` | Stage boundaries |
| `test_later_failure_blocks_older_pass_and_unknown_notes_are_not_reusable` | Evidence ordering and publication; NOTE and retry eligibility |
| `test_clock_rollback_cannot_hide_a_later_same_input_failure` | Evidence ordering and publication |
| `test_missing_later_failure_or_index_cannot_restore_an_older_success` | Evidence ordering and publication |
| `test_issuance_refuses_a_failure_after_the_last_worker_validation` | Evidence ordering and publication |
| `test_retry_requires_complete_success_and_preserved_first_attempt` | NOTE and retry eligibility |
| `test_same_version_installed_bytes_and_change_metadata_are_inputs` | Environment contents and changes |
| `test_missing_referenced_origin_never_becomes_an_execution` | Origin eligibility; evidence retirement |
| `test_partial_completion_and_cleanup_cannot_issue_success` | Origin eligibility; stage observation and recovery |
| `test_other_clone_is_eligible_but_same_tree_nonancestor_is_not` | Origin eligibility; real reuse after replacement |
| `test_external_symlink_and_committed_attributes_cannot_supply_source_identity` | Unknown source and external inputs |
| `test_git_replacement_cannot_hide_a_cumulative_package_change` | Unknown source and external inputs; comparison unavailable with accepted manager available |
| `test_external_git_attributes_cannot_change_archive_behind_a_reused_unit` | Unknown source and external inputs |
| `test_execution_ids_do_not_invalidate_but_other_or_unknown_values_do` | Child environment |
| `test_claude_session_ids_do_not_invalidate_but_shell_settings_do` | Child environment |
| `test_older_probe_cannot_silently_ignore_the_metadata_audit` | Child environment |
| `test_failed_stage_survives_cleanup_failure_and_recovery` | Stage observation and recovery |
| `test_failure_before_first_unit_accepts_the_empty_r_report` | Stage observation and recovery |
| `test_unobserved_failed_running_request_fences_an_older_success` | Stage observation and recovery; evidence ordering and publication |
| `test_unobserved_running_request_without_worker_result_is_uncertain` | Stage observation and recovery; evidence ordering and publication |
| `test_missing_indexed_origin_journal_does_not_block_executed_cleanup` | Cache reads versus interrupted writes |
| `test_pending_success_observation_is_not_an_execution_origin` | Stage observation and recovery |
| `test_delayed_success_cleanup_cannot_hide_a_later_stage_failure` | Evidence ordering and publication |
| `test_recovery_completes_only_the_interrupted_observation_index` | Stage observation and recovery |
| `test_metadata_environment_is_shared_by_probe_gate_and_descendants` | Child environment |
| `test_1107_new_format_baseline_then_actual_contract_without_full_replay` | Real reuse after replacement; source and contract identity |
| `test_missing_baseline_runs_full_and_reuse_cancellation_keeps_origin` | Real reuse after replacement; cancellation, manager death, spawn-registration gap |
| `test_environment_change_during_reuse_fails_without_full_fallback` | Environment contents and changes; cache reads versus interrupted writes |
| `test_terminal_publication_failure_cannot_return_a_limited_pass` | Cache reads versus interrupted writes; evidence ordering and publication |

## Required implementation evidence

The audit's static mapping could not establish native process ownership,
filesystem locking, durable interruption handling, authentication comparison,
GC transactions, or cross-clone reuse under private renderer paths. Dedicated
process/contract fixtures and real new-runner executions were required by the
specification. The new retention and key-renewal protocol had no completed
implementation or executed fixtures in this audit.

Successful porting would need case-level assertions and actual results for the
baseline corpus plus the new retirement, rotation, renderer, and main-authority
cases. Old Python evidence could not serve as newly executed R evidence.
The design-level conclusion was that the inherited obligations and amendments
had been identified; implementation safety remained to be established.

[pr]: https://github.com/sayuks/marginplyr/pull/1115
[adr]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md
[followups]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/design/specs/review-ready-verification-followups.md
[local checks]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/design/agents/local-checks.md
[shared admission]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-admission.md
[evidence]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-evidence.py
[admission]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-admission.py
[gate]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-check-lib.R
[fixtures]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/test-review-ready-evidence.py
[admission fixtures]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/test-review-ready-admission.py
[gate verifier]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/.github/scripts/verify-review-ready-check.R
