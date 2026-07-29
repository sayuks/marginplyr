# Reject locally detectable errors before backend reads

Margin verbs reject invalid arguments, unsupported local contexts, and
incompatible input grouping before acquiring backend metadata or executing a
margin-label validation query. Schema-dependent checks follow metadata
acquisition, but preparation must not contact a backend for an error that can
already be determined from the call and input metadata available locally.
