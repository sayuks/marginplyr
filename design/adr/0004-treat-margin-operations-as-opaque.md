# Treat prepared margin operations as opaque values

Public verb implementations and tests pass a prepared margin operation as a
whole rather than reading its fields. Its representation remains a
package-private implementation detail, with field access localized to the
margin-operation module and dedicated execution adapters; this prevents the
new seam from becoming a shallow data bag and allows grouping-plan and backend
internals to evolve independently. The value retains only canonical state
needed for execution and finalization; original quosures, input grouping
metadata, and options whose validation or compilation has completed are
discarded rather than stored beside their derived forms.
