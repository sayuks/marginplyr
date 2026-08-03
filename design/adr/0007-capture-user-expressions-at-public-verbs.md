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
