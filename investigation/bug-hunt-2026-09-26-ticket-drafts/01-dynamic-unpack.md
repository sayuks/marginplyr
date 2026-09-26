# Preserve ordinary .fns evaluation with dynamically false across unpacking

Investigated: 2026-09-26
Status at preparation: proposed; publication awaited breakdown approval.

## What to build

**Priority:** P2

Keep ordinary local `across()` evaluation unchanged when `.unpack` is an expression, delayed binding, or active binding that evaluates to FALSE. An ordinary function factory must run as dplyr requires, and its returned function must determine the same values as the literal-FALSE control.

At the investigated baseline, one row with x = 1 and a factory that adds its invocation number returns x = 2 after one factory call under ordinary dplyr and literal-FALSE Margin evaluation. Dynamic FALSE returns x = 3 after two calls. Two groups amplify the extra calls. A clean archive of ac088ec passes; the change in c8b6b1c introduces this regression.

The repair must retain actual-value collision protection for dynamically unpacked frames. Merely removing that protection would reopen #683 and #685.

Evidence: [Investigation](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.md), finding 1; executable probe `unpack` in [executable probes](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.R). Contracts: ordinary function evaluation under ADR 0019 and actual-value frame checking under ADR 0028.

## Acceptance criteria

- [ ] The single-set reproduction returns x = 2 with one function-factory invocation, matching ordinary top-level dplyr and the literal-FALSE Margin control.
- [ ] Expressions, delayed bindings, and active bindings returning FALSE preserve ordinary values, names, and evaluation counts. Cover both one function and a function list.
- [ ] More than one local group does not introduce per-group evaluation of an argument dplyr expands once. Cover tibble, data.frame, and eager data.table inputs through their supported paths.
- [ ] The same evaluation contract holds with an unrelated contextual share.
- [ ] Dynamic TRUE and string unpacking still refuse collisions with public grouping keys, allocated internal grouping keys, the requested identifier, and an already defined share source. Existing #683/#685 regression tests continue to pass.
- [ ] Ordinary invalid-unpack diagnostics and caller-named packed summaries keep their existing behavior.
- [ ] Regression tests use public Margin calls and compare both observable output and callback counts. No lazy-input read is added.

## Blocked by

None (can start immediately).
