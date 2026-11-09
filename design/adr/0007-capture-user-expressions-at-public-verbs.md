# Capture user expressions at public verbs

Margin verbs capture `.by`, `.grouping`, and summary expressions exactly once
at the public call boundary, then pass those quosures into operation
preparation and verb-specific execution. Internal helpers do not recapture or
re-inject them through wrapper layers, preserving the caller environment and
keeping tidy-evaluation and error contexts stable while the lifecycle is
refactored. The public verb's call context is also propagated explicitly to
internal helpers that raise Package conditions so the new seam does not
expose lifecycle helper calls. `nest_with_margins()` and
`nest_by_with_margins()` capture independently at their public boundaries and
pass those quosures to one private nest pipeline; neither public verb
re-injects expressions through the other.

This decision fixes where an expression is captured and whose environment it
keeps. Which names *inside* a captured expression are resolved from that
environment, and which are recognized by spelling and rewritten before
anything runs, is ADR 0019.

## Amendment: public injection keeps its written evaluation boundary

Accepted on 2026-11-09. Public capture retains ordinary rlang injection
semantics: an outer `!!` or `!!!` can change language before a factory runs.
Inline and previously evaluated factories need not agree across different
injection boundaries. Preserve caller-owned language against additional
marginplyr rewriting and keep already captured quosures opaque; neither
promise disables injection the caller wrote.

The `do.call`/`alist` empty-splice observation receives no implementation
repair: ordinary `rlang::enquo()` produces the same difference. Replacing
public capture with `enquo0()` would remove supported injection, while
protecting `alist()` alone would give that spelling a special interpretation.
The [boundary specification](../specs/grouping-context-and-factor-schema-boundaries.md#2-public-injection-follows-its-written-evaluation-boundary)
records the adopted scope. Existing authored-constructor empty-argument rules
continue within their own capture boundary.
