# Preserve dynamically named .data summary outputs through portable evaluation

Investigated: 2026-09-26
Status at preparation: proposed; publication awaited breakdown approval.

## What to build

**Priority:** P2

Allow an ordinary summary dynamically named `.data` to pass through Margin evaluation as an output column, as it does in ordinary dplyr. Captured summary names must not become formal arguments before dplyr captures the summary dots.

With one row grouped by g, dynamically naming n() as .data works in ordinary dplyr but a Margin rollup raises `formal argument ".data" matched by multiple actual arguments`. The shared portable summary path affects local input and SQLite; the same naming can already work in alternate share/native paths.

This is a call-construction defect, separate from SQL identifier equality. The existing intentional refusals of .by and .groups are outside the requested expansion and must remain unchanged.

Evidence: [Investigation](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.md), finding 6; probe `dot_data` in [executable probes](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.R). The public summary reference defines its dots as name-value pairs used by dplyr::summarize(); ADR 0016 additionally governs result class and attributes.

## Acceptance criteria

- [ ] The local reproduction returns g and .data with detail and Grand total counts both 1, rather than a formal-argument error.
- [ ] Both dynamic-name syntax and spliced named quosures preserve the .data output name and values.
- [ ] Supported local, dtplyr, SQLite, and native/portable DuckDB summary paths behave consistently with their ordinary-dplyr controls.
- [ ] Cover an ordinary .data summary and a contextual-share output of that name where the backend supports the operation, so the repair does not break already successful alternate paths.
- [ ] Grouping, source-data arguments, public error attribution, and other ordinary summary outputs remain intact.
- [ ] Existing accepted neighboring names such as .env and .drop remain outputs, while deliberate package refusals for .by and .groups remain unchanged.
- [ ] Regression coverage uses the public interface and preserves lazy construction on lazy backends.

## Blocked by

None (can start immediately).
