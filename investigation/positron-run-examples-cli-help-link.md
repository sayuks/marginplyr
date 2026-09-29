# A cli help link shown literally in Positron Run Examples

Investigated: 2026-09-29

On 2026-09-29, a user running Positron 2026.09.01 (user setup), build 2,
reported that **Run Examples** for `marginplyr::share_of_parent()` showed the
link around `dplyr::mutate()` as text resembling
`ESC]8;;x-r-help:dplyr::mutate ... ESC]8;;`, while the same rejection called
in Positron's **R Console** displayed `dplyr::mutate()` normally as a help
link. The two user-provided screenshots establish the display difference; they
do not establish which Positron component produced it. This note records that
observation and the R-side reproduction. It makes no implementation decision.

## R-side path and reproduction

At repository HEAD `109ff895d1d91e8ec2541f630a8b53fe6f56e99c`, the
rejected-call example invokes `share_of_parent(revenue)` in `R/share.R`.
`share_of_parent()` and `share_of_total()` both put
`{.fun dplyr::mutate}` in their rewrite bullet. `abort_marginplyr()` in
`R/conditions.R` expands that cli template with `cli::format_inline()` when
raising the condition, then calls `rlang::abort()`. ADR 0024 owns that timing
decision. The markup is therefore part of the authored diagnostic, while the
hyperlink bytes depend on cli's output configuration at raise time.

Measured with R 4.6.1, cli 3.6.6, and `pkgload::load_all(".", quiet = TRUE)`
against that HEAD. For each helper, the condition was caught and
`conditionMessage()` was checked for an OSC 8 opener (`ESC]8;`) and the
`x-r-help:dplyr::mutate` URI. `cli.num_colors` was fixed at `1L`:

| `cli.hyperlink` | `cli.hyperlink_help` | OSC 8 and help URI, for both helpers |
| --- | --- | --- |
| `FALSE` | `FALSE` | absent |
| `FALSE` | `TRUE` | absent |
| `TRUE` | `FALSE` | absent |
| `TRUE` | `TRUE` | present |

In all four cases, `cli::ansi_strip(message, link = TRUE)` retained the
readable `dplyr::mutate()` in the rewrite. The experiment isolates the
specialized help link from color: it did not require ANSI colors to be enabled,
and `cli.hyperlink_help = TRUE` alone did not emit a link in this session. It
does not reproduce Positron's **Run Examples** rendering outside that IDE.

The [cli configuration reference](https://cli.r-lib.org/reference/cli-config.html#hyperlinks)
documents OSC 8 hyperlinks, the general `cli.hyperlink` capability, the
specialized `cli.hyperlink_help` capability used by `{.fun}`, and the default
`x-r-help:{topic}` URI. [Positron issue #1906](https://github.com/posit-dev/positron/issues/1906)
describes its R runtime handling of `x-r-help` links. That issue concerns
opening a link; it does not establish how **Run Examples** displays a captured
diagnostic.

## Limit of the finding

The visible `x-r-help:dplyr::mutate` fragment matches cli's documented link
format and the bytes produced by the local reproduction. The different
appearance in the two Positron surfaces is consistent with **Run Examples**
showing an OSC 8 sequence literally while the R Console interprets it. The
screenshots and local R experiment do not identify whether Positron captures,
transforms, or renders the sequence differently, nor do they rule out a
surface-specific cli configuration. No Positron source path was verified for
this observation.
