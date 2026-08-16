# Validate Margin labels before execution

Operation preparation normalizes a Margin label and constructs its typed
column metadata, but each verb-specific execution applies all semantic label
rules after its schema-aware preflight and immediately before low-level
execution. The Margin operation module still owns backend, factor, and
collision validation; delaying them preserves the existing error and query
order so an invalid summary is rejected before label validation can fail or
an opt-in collision query contacts a lazy backend.

## Amendment: one half of the collision check contacts nothing

The order above is unchanged. What changed is that "an opt-in collision query
contacts a lazy backend" now describes only half of the collision check.

A Margin label equal to a declared factor level is found in the typed metadata
ADR 0002 already acquires, so that collision is rejected on every backend
whatever `.check_margin_label` says, and no query is issued to find it. Only a
label equal to an observed value requires reading, and that read is what
`.check_margin_label` opts into. Both halves still run after the schema-aware
preflight and immediately before low-level execution, for the reason stated
above. See ADR 0020.
