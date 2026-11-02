# Retiring installed Review-ready admission

The maintainer accepted the simplified repository-owned runner in
[#1114](https://github.com/sayuks/marginplyr/issues/1114).
[Local checks](../design/agents/local-checks.md) and the
[runner guide](review-ready-runner.md) describe its target behavior.

## Drained cutover

Until the maintainer completes this transition, the existing user-wide
instruction still requires `/Users/sayuks/.local/bin/marginplyr-review-ready`.
An updated checkout alone does not retire the installed command or authorize
concurrent use of both admission generations.

After all old waiting/running checks finish or their owners cancel them and
confirm owned work has stopped, adopt the repository R entry, uninstall the
legacy CLI, and update repository plus user-wide Codex/Claude instructions
together. Remove obsolete checker-owned coordination state and diagnostics
only after shutdown, using established ownership rather than a matching name.
Keep unrelated tools, libraries, shared caches, research and OS reports.
Record the actual Mac acceptance and terminal-handle evidence before claiming
rollout complete.

Old worktrees and their existing sessions are outside migration scope. This
cutover requires no inventory, individual refresh, compatibility launcher,
status/recovery adapter or automatic migration service for them.

## History

The installed generation coordinated a heavy slot and later added automatic
classification, stage reuse, authenticated evidence and strict recovery.
Those mechanisms are retired by the 2026-11-02 simplification. Its historical
contract is preserved in [the earlier guide](https://github.com/sayuks/marginplyr/blob/1a9dd00a/tools/review-ready-admission.md);
[ADR 0038](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md)
records the superseded evidence decision. Historical commands are not current
setup instructions.
