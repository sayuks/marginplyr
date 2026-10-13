# Source locations at SQL audit capture

Investigated: 2026-10-01
Scope: source-location evidence; no adopted audit contract or implementation
Environment: macOS, R 4.6.1 (2026-06-24), separate Rscript processes

## Findings

The experiments distinguished a call's source reference from the source of the
function being called. With retained source, `attr(sys.call(i), "srcref")`
identified the call site; `attr(sys.function(i), "srcref")` identified the
function definition. The latter was not a substitute for the SQL request's
call site.

| Executed case | Observed source information |
|---|---|
| `source(file, keep.source = TRUE)` | `wrapper()` had the temporary file and line 4; its `capture_site()` call had that file and line 2; the direct call had line 5 |
| `sys.source(file, keep.source = TRUE)` | The same call-site file and lines were available |
| Either source function with `keep.source = FALSE` | Calls and function definitions had no source references |
| `Rscript file.R`, default options | `interactive()` was `FALSE`, `keep.source` and `keep.source.pkgs` were `FALSE`; `commandArgs()` held `--file=<temporary script>`, but the calls had no source references |
| `Rscript file.R` with an initial `options(keep.source = TRUE)` | The call inside the single-line wrapper had a source reference with line 1 and an empty filename; the top-level wrapper call had none |
| `eval(parse(text = "capture_site()", keep.source = TRUE))` | The evaluated call had line 1 and filename `<text>` |

These were counterexamples to treating a missing filename as console input, or
the startup `--file` argument as the file containing every later SQL call.
Source information was conditional metadata, not a universal execution-origin
identifier. An interactive session could also source a file. Function aliases,
anonymous calls and method dispatch made the spelling in the call stack
different from a unique canonical function name.

The RStudio execution guide stated that selected editor lines were inserted
into the console, while Source saved a temporary file and sourced it. Thus an
editor file, console transport and a temporary source reference were distinct.
No IDE execution was measured in this campaign.

Inspection of installed `evaluate::evaluate()` found a `filename = NULL`
argument. `evaluate::parse_all.character()` constructed a `srcfilecopy` from
the supplied filename and parsed the input text. This established an evaluator
metadata boundary, not a guarantee that R Markdown or Quarto supplied the
original document's absolute line numbers. No document render was executed.

## Primary sources read

- [R source documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/source.html)
  described parsing and sequential evaluation, with `keep.source` controlling
  source retention.
- [R sys.source documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sys.source.html)
  documented sourcing into an environment and its separate `keep.source`
  argument.
- [R source references](https://stat.ethz.ch/R-manual/R-devel/library/base/html/srcfile.html)
  described source ranges, copied text, real-file metadata and source aliases.
- [R call-stack documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sys.parent.html)
  distinguished calls, functions and frames, and warned that lazy evaluation
  observes the stack when evaluated rather than when an argument was written.
- [R commandArgs documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/commandArgs.html)
  identified its information as session-start arguments.
- [RStudio execution guide](https://docs.posit.co/ide/user/ide/guide/code/execution.html)
  distinguished selected-line execution, Source and document rendering.

## Reproduction

The following compact replay driver represents the tested arrangements; it
consolidates the two executed drivers and was not rerun after consolidation.
Temporary filenames varied between processes. Run it with `Rscript` from
stdin; it starts child Rscript processes for actual script-file invocation.

```r
capture_site <- function() {
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    call_ref <- attr(calls[[i]], "srcref")
    function_ref <- attr(sys.function(i), "srcref")
    print(list(
      call = paste(deparse(calls[[i]]), collapse = " "),
      call_ref = as.integer(call_ref),
      call_file = if (is.null(call_ref)) NULL else
        utils::getSrcFilename(call_ref, full.names = TRUE),
      function_ref = as.integer(function_ref),
      function_file = if (is.null(function_ref)) NULL else
        utils::getSrcFilename(function_ref, full.names = TRUE)
    ))
  }
}

path <- tempfile(fileext = ".R")
writeLines(c(
  "wrapper <- function() {", "  capture_site()", "}",
  "wrapper()", "capture_site()"
), path)
for (keep in c(FALSE, TRUE)) {
  source(path, local = new.env(), keep.source = keep)
  sys.source(path, envir = new.env(), keep.source = keep)
}
eval(parse(text = "capture_site()", keep.source = TRUE))
unlink(path)

script <- tempfile(fileext = ".R")
capture_code <- deparse(capture_site)
capture_code[[1L]] <- paste0("capture_site <- ", capture_code[[1L]])
script_code <- c(
  'print(list(interactive = interactive(), keep.source = getOption("keep.source"), args = commandArgs()))',
  capture_code,
  "wrapper <- function() { capture_site() }",
  "wrapper()",
  'eval(parse(text = "capture_site()", keep.source = TRUE))'
)
for (keep in c(FALSE, TRUE)) {
  writeLines(c(if (keep) "options(keep.source = TRUE)", script_code), script)
  system2(file.path(R.home("bin"), "Rscript"), script)
}
unlink(script)
```

## Capture implications and limits

A source-location capture could report the available call-site metadata and
leave unknown fields missing. The evidence did not support replacing missing
fields with `console`, selecting an IDE cursor position as the executed line,
or inferring an original document location from an evaluator fragment.

A call-stack snapshot taken at each request would describe that request's
active R calls, including reentry. A result's construction stack and its later
collection stack would answer different questions. Deferring the snapshot
would risk recording a different active stack.

The amount of stack extraction and deparsing would depend on stack depth and
call expression size; no timing benchmark was run. Saving `sys.frames()` would
retain live environments and potentially inputs or connections, beyond the
location information examined here. Calls and scalar source metadata were
sufficient for these experiments. This note adopted no storage policy.
