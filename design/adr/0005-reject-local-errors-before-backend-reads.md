# Reject locally detectable errors before backend reads

Margin verbs reject invalid arguments, unsupported local contexts, and
incompatible input grouping before acquiring backend metadata or executing a
Margin label validation query. Schema-dependent checks follow metadata
acquisition, but preparation must not contact a backend for an error that can
already be determined from the call and input metadata available locally.

Backend-specific admission that depends on completed schema-aware planning is
also allowed after the one typed metadata snapshot, provided it runs before
query construction or execution. Arrow Parent shares use this boundary: their
grammar, source, dependency, naming, and Grouping-plan checks retain
precedence, then a valid request is rejected before ordinary-summary staging.
The existing Arrow schema snapshot is the only backend read performed for that
rejection.
