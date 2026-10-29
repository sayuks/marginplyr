# Supplemental triage: dtplyr `.SD` summary outputs

Investigated: 2026-10-29
Repository snapshot: `4c63bb9`

The coordinator's unusual-name matrix exposed failures when `.SD` named an
ordinary summary, share source, or share output. Independent triage classified
these as an upstream restriction, rather than a new package finding.

An ordinary grouped dtplyr summary admitted `.SD = sum(v)`, but an ordinary
**unpartitioned** summary of the same name failed with `A column may not be
called .SD. That has special meaning.` The no-fixed-key rollup contained that
same unpartitioned Grand total branch. The difference between those ordinary
branches explained why a grouped-only control had initially suggested new
Margin support was missing.

Retaining a fixed key `f` made both ordinary branch grains valid: detail groups
used `f,g` and the Grand total used `f`. On this supported boundary,
`dtplyr-sd-output-controls.R --assert` passed 60 comparisons against local
results: empty/populated input, ordinary `.SD` outputs and both contextual-share
helpers in `.SD` source/output roles, text/typed-missing labels, and all three
Margin-order choices. Twenty-four controls also passed: the ordinary detail
and grand summaries, a single-set Margin summary, `.id = ".SD"` shares, source
isolation, and the unpartitioned ordinary/Margin refusals. No error remained on
the supported boundary. Construction retained lazy dtplyr steps.

The script recorded `dtplyr-sd-output-results.csv` and
`dtplyr-sd-outputs-report.log`. Its assertion mode exited 0. The larger matrix's
`.SD` signals did not justify a new ticket.

Closed #696's full body and empty comment list were read: its promised sources
were `.I`, `.GRP`, and `.NGRP`, not an unpartitioned `.SD` source. The special-name
refusal tests covered invalid dot-dot source/cardinality/name cases; they did
not assert a blanket `.SD` prohibition. No public documentation imposing such
a blanket prohibition was found. Closed #704 concerned generated expansion
projection, while open #1047 concerned **input measures** feeding otherwise
ordinary share-source names. Neither changed the ordinary unpartitioned-summary
boundary established here.
