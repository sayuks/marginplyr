# Use an explicit margin-operation lifecycle

Margin verbs explicitly prepare one opaque margin operation, pass it to their
verb-specific execution, and finalize the raw result through the shared
module. We rejected a callback-based runner because the explicit
prepare–execute–finalize sequence keeps tidy evaluation, call stacks, and
error locations easier to understand while retaining a small shared
interface.
