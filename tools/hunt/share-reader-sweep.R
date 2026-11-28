#!/usr/bin/env Rscript
# Which exported functions read a name they are handed as text or as a held
# value, and what does a later summary get when that name is an earlier share
# and also an input column? `design/agents/bug-hunt.md` says when to run it.
#
#   Rscript tools/hunt/share-reader-sweep.R REPO OUT
#
# REPO is a marginplyr checkout, loaded with pkgload; OUT receives one
# `<package>.found` file of reading calls per package and `verdicts.tsv`.
#
# A worker calls every exported function of a package with each argument shape
# below. It therefore runs in a throwaway directory, in a child process the
# parent restarts past a call that kills it, and skips the names in `unsafe`.
# Run it where the network and the home directory are out of reach: the list
# is what was needed on R 4.6 with the packages below, not a proof.
args <- commandArgs(trailingOnly = TRUE)
probe <- "marginplyr_probe"

unsafe <- c(
  # End, block or signal.
  "q", "quit", "stop", "abort", "interrupt", "Sys.sleep", "readline",
  "readLines", "scan", "menu", "askYesNo", "select.list", "invokeRestart",
  "Recall", "on.exit", "repeat", "while", "for", "if", "function", "pskill",
  "setTimeLimit", "setSessionTimeLimit", "browser", "recover", "dump.frames",
  # Session state another call would inherit.
  "debug", "debugonce", "undebug", "debugcall", "undebugcall", "debugger",
  "trace", "untrace", "tracemem", "untracemem", "gcinfo", "gctorture",
  "gctorture2", "Rprof", "Rprofmem", "options", "set.seed", "Sys.setenv",
  "Sys.unsetenv", "Sys.setlocale", "Sys.setLanguage", "attach", "detach",
  "unloadNamespace", "sink", "setwd", "rc.options", "rc.settings",
  "environment<-", "body<-", "formals<-", "parent.env<-", "rm", "remove",
  "makeActiveBinding", "lockBinding", "lockEnvironment", "unlockBinding",
  "local_options", "local_envvar", "local_dir", "local_seed", "with_dir",
  "local_interactive", "with_interactive", "defer", "local_bindings",
  "closeAllConnections", "registerS3method", "setHook", "reg.finalizer",
  "Sys.setFileTime", "Sys.chmod", "Sys.umask", "enc2native",
  # Files, processes, editors and the network.
  "system", "system2", "shell", "shell.exec", "unlink", "file.remove",
  "file.rename", "file.copy", "file.symlink", "file.link", "file.append",
  "edit", "fix", "vi", "emacs", "pico", "xemacs", "xedit", "View", "page",
  "file.edit", "data.entry", "dataentry", "de", "install.packages",
  "update.packages", "remove.packages", "download.file", "download.packages",
  "available.packages", "old.packages", "new.packages", "packageStatus",
  "CRAN.packages", "contrib.url", "chooseCRANmirror", "chooseBioCmirror",
  "setRepositories", "browseURL", "browseEnv", "browseVignettes", "url",
  "url.show", "curlGetHeaders", "socketConnection", "serverSocket", "socketAccept",
  "socketSelect", "make.socket", "read.socket", "write.socket", "nsl",
  "file", "gzfile", "bzfile", "xzfile", "pipe", "fifo", "unz", "gzcon",
  "rawConnection", "textConnection", "open", "stdin", "tar", "untar", "zip",
  "unzip", "package.skeleton", "prompt", "promptData", "promptPackage",
  "promptImport", "Sweave", "Stangle", "example", "demo", "help.start",
  "bug.report", "create.post", "help.request", "RSiteSearch", "RShowDoc",
  "vignette", "news", "savehistory", "loadhistory", "save.image",
  "sys.save.image", "source", "sys.source", "check_installed", "is_installed",
  "txtProgressBar", "winProgressBar", "srcfile", "getSrcref", "citation",
  "person", "nlm", "optim", "optimize", "optimise", "uniroot", "integrate"
)

shapes <- list(
  string = quote(F("marginplyr_probe")),
  symbol = quote(F(held_symbol)),
  language = quote(F(held_call)),
  formula = quote(F(held_formula)),
  strings = quote(F(c("marginplyr_probe", "marginplyr_probe"))),
  list = quote(F(list("marginplyr_probe"))),
  name_get = quote(F("marginplyr_probe", get)),
  get_name = quote(F(get, "marginplyr_probe")),
  getstr_name = quote(F("get", "marginplyr_probe")),
  getstr_list = quote(F("get", list("marginplyr_probe"))),
  one_get_name = quote(F(1, get, "marginplyr_probe")),
  env_name = quote(F(environment(), "marginplyr_probe")),
  name_env = quote(F("marginplyr_probe", environment())),
  env = quote(F(environment())),
  template = quote(F("{marginplyr_probe}")),
  code = quote(F("sum(marginplyr_probe)")),
  code_text = quote(F(text = "marginplyr_probe")),
  symbol_env = quote(F(held_symbol, environment())),
  language_env = quote(F(held_call, environment())),
  lambda = quote(F(held_lambda)),
  one_lambda = quote(F(1, held_lambda))
)
held <- list(
  held_symbol = quote(marginplyr_probe),
  held_call = quote(sum(marginplyr_probe)),
  held_formula = ~ marginplyr_probe,
  held_lambda = ~ get("marginplyr_probe")
)

worker <- function(package, out) {
  done <- file.path(out, paste0(package, ".done"))
  current <- file.path(out, paste0(package, ".current"))
  found <- file.path(out, paste0(package, ".found"))
  skip <- c(unsafe, if (file.exists(done)) readLines(done))
  work <- file.path(tempdir(), "work")
  dir.create(work)
  setwd(work)
  frame_parent <- list2env(held)
  reads <- function(call) {
    read <- FALSE
    frame <- new.env(parent = frame_parent)
    makeActiveBinding(probe, function() {
      read <<- TRUE
      1
    }, frame)
    setTimeLimit(elapsed = 2, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    try(suppressWarnings(suppressMessages(
      utils::capture.output(eval(call, frame))
    )), silent = TRUE)
    read
  }
  for (name in sort(getNamespaceExports(package))) {
    value <- getExportedValue(package, name)
    if (!is.function(value) || name %in% skip || grepl("^node_", name)) next
    writeLines(name, current)
    head <- call("::", as.name(package), as.name(name))
    for (shape in names(shapes)) {
      call <- do.call(substitute, list(shapes[[shape]], list(F = head)))
      if (isTRUE(tryCatch(reads(call), error = function(cnd) FALSE))) {
        cat(paste(deparse(call, width.cutoff = 500L), collapse = " "), "\n",
            sep = "", file = found, append = TRUE)
      }
    }
    cat(name, "\n", sep = "", file = done, append = TRUE)
  }
  file.remove(current)
  invisible(NULL)
}

if (identical(args[[1L]], "--worker")) {
  worker(args[[2L]], args[[3L]])
  quit(status = 0L)
}

repo <- normalizePath(args[[1L]])
out <- args[[2L]]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
script <- normalizePath(sub("^--file=", "", grep(
  "^--file=", commandArgs(FALSE), value = TRUE
)))
packages <- c("base", "stats", "utils", "methods", "rlang", "purrr", "glue",
              "withr", "magrittr", "tidyselect", "vctrs", "tibble", "dplyr",
              "tidyr", "pillar", "lifecycle", "cli")
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) next
  current <- file.path(out, paste0(package, ".current"))
  repeat {
    system2(file.path(R.home("bin"), "Rscript"),
            c(shQuote(script), "--worker", package, shQuote(out)),
            stdout = FALSE, stderr = FALSE, stdin = "")
    if (!file.exists(current)) break
    # The call that killed the worker is passed over.
    cat(readLines(current), "\n", sep = "",
        file = file.path(out, paste0(package, ".done")), append = TRUE)
    cat("worker died in ", package, "::", readLines(current), "\n", sep = "")
    file.remove(current)
  }
}
found <- unlist(lapply(Sys.glob(file.path(out, "*.found")), readLines))
cat(length(found), "reading calls\n")

suppressMessages(pkgload::load_all(repo, quiet = TRUE))
input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4),
                        marginplyr_probe = c(100, 200, 300))
invisible(list2env(held, environment()))
# Evaluate `call` in `env` with nothing printed.
quietly <- function(call, env) {
  sink(nullfile())
  on.exit(sink(), add = TRUE)
  suppressWarnings(suppressMessages(eval(call, env)))
}
verdict <- function(text) {
  call <- parse(text = sprintf(paste0(
    "summarize_with_margins(input, s = sum(v), ",
    "marginplyr_probe = share_of_total(s), z = %s, .grouping = rollup(g))"
  ), text))[[1L]]
  setTimeLimit(elapsed = 20, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  tryCatch({
    quietly(call, globalenv())
    "value"
  }, marginplyr_error = function(cnd) "refused", error = function(cnd) {
    paste("error:", substr(gsub("\\s+", " ", conditionMessage(cnd)), 1, 70))
  })
}
verdicts <- vapply(found, verdict, character(1))

# A returned value counts only if it follows the input column: the same call
# over two inputs that differ in that column alone.
later_summary <- function(text, column) {
  input$marginplyr_probe <- column
  call <- parse(text = sprintf(paste0(
    "summarize_with_margins(input, s = sum(v), ",
    "marginplyr_probe = share_of_total(s), z = %s, .grouping = rollup(g))"
  ), text))[[1L]]
  tryCatch(quietly(call, environment())$z, error = function(cnd) "error")
}
# cli numbers the elements it emits, so a value holding such an id differs
# between any two calls.
counted <- function(value) {
  is.character(unlist(value)) && any(grepl("^cli-[0-9]+-[0-9]+$", unlist(value)))
}
follows <- vapply(found[verdicts == "value"], function(text) {
  first <- later_summary(text, c(100, 200, 300))
  !counted(first) && !identical(first, later_summary(text, c(5, 6, 7)))
}, logical(1))
verdicts[verdicts == "value"][follows] <- "follows the input column"

result <- data.frame(call = found, verdict = verdicts, row.names = NULL)
utils::write.table(result, file.path(out, "verdicts.tsv"), sep = "\t",
                   row.names = FALSE, quote = FALSE)
cat(sum(verdicts == "refused"), "refused,",
    sum(startsWith(verdicts, "error")), "ended in an error,",
    sum(verdicts == "value"), "returned a value that does not follow the",
    "input column,", sum(follows), "follow it:\n")
writeLines(paste0("  ", found[verdicts == "follows the input column"]))
