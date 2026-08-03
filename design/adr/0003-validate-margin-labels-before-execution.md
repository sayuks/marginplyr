# Validate Margin labels before execution

Operation preparation normalizes a Margin label and constructs its typed
column metadata, but each verb-specific execution applies all semantic label
rules after its schema-aware preflight and immediately before low-level
execution. The Margin operation module still owns backend, factor, and
collision validation; delaying them preserves the existing error and query
order so an invalid summary is rejected before label validation can fail or
an opt-in collision query contacts a lazy backend.
