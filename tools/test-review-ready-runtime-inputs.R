# Hold unrelated installations fixed while observing real Python startup/cache
# inputs and Git's reported installation behind an unchanged PATH launcher.
run_runtime_input_contracts <- function() {
  fixture <- rr_mkdir(tempfile("runner-runtime-inputs-"))
  on.exit(unlink(fixture, recursive = TRUE, expand = FALSE), add = TRUE)
  invisible(openssl::openssl_config())
  requireNamespace("processx", quietly = TRUE)
  values <- rr_render_env(rr_mkdir(file.path(fixture, "request")))
  values[["PYTHONPATH"]] <- ""
  values[["PYTHONPYCACHEPREFIX"]] <- rr_mkdir(file.path(fixture, "external-bytecode"))
  python <- unname(Sys.which("python3"))
  compile <- paste(c("import importlib.util, pathlib, py_compile, sysconfig",
    "source = pathlib.Path(sysconfig.get_path('stdlib')) / 'colorsys.py'",
    "cache = importlib.util.cache_from_source(str(source))",
    "pathlib.Path(cache).parent.mkdir(parents=True, exist_ok=True)",
    "py_compile.compile(str(source), cfile=cache, doraise=True)", "print(cache)"), collapse = "\n")
  cached <- trimws(rr_run(python, c("-c", compile), env = values)$stdout)
  expect(file.exists(cached) && startsWith(cached, paste0(values[["PYTHONPYCACHEPREFIX"]], "/")),
    "The actual Python runtime creates the selected external stdlib cache")

  observer <- rr_environment
  adapter <- new.env(parent = environment(observer))
  environment(observer) <- adapter
  adapter$rr_renderer <- function() list(identity = list())
  adapter$rr_git_environment <- function(...) list()
  adapter$rr_native_file <- function(...) FALSE
  adapter$rr_directory_identity <- function(...) list(digest = "unchanged installation", natives = character())
  adapter$rr_linkage <- function(...) "unchanged native linkage"
  actual_run <- rr_run
  adapter$rr_run <- function(command, args, ...) {
    if (length(args) && identical(args[[1L]], "tools/review-ready-environment.R")) {
      actual <- rr_child_env()
      audit <- list(version = 4L, names = as.list(names(rr_metadata)),
        present = as.list(names(rr_metadata)[nzchar(actual[names(rr_metadata)]) & !is.na(actual[names(rr_metadata)])]))
      return(list(stdout = rr_json(list(unregistered = list(), metadata_audit = audit,
        libraries = list(), runtime_roots = list(), configuration = list(), clock = list(), r = "fixture"))))
    }
    actual_run(command, args, ...)
  }
  observe <- function() rr_in_environment(values, observer(candidate))
  before <- observe()
  original <- rr_read(cached)
  writeBin(c(original, charToRaw("changed bytecode")), cached)
  expect(!identical(before$digest, observe()$digest),
    "External bytecode changes invalidate the environment even for an unimported stdlib module")
  unlink(cached)
  expect(!identical(before$digest, observe()$digest),
    "Removing externally cached bytecode invalidates the environment inventory")
  startup <- rr_mkdir(file.path(fixture, "startup"))
  writeLines("fixture_startup = True", file.path(startup, "sitecustomize.py"))
  values[["PYTHONPATH"]] <- startup
  rejects(observe(), "Unregistered Python startup/module input")
  values[["PYTHONPATH"]] <- ""

  installation <- rr_mkdir(file.path(fixture, "git-installation"))
  execution <- rr_mkdir(file.path(installation, "libexec/git-core"))
  actual_git <- file.path(rr_mkdir(file.path(installation, "bin")), "git")
  writeLines("First Git implementation", actual_git)
  git <- rr_git_environment
  git_adapter <- new.env(parent = environment(git))
  environment(git) <- git_adapter
  git_adapter$rr_comparison_inputs <- function(...) invisible(TRUE)
  git_adapter$rr_account_home <- function() fixture
  git_adapter$rr_git_raw <- function(...) {
    do.call(c, lapply(c("local", "file:fixture", "user.name\nFixture"),
      function(field) c(charToRaw(field), as.raw(0))))
  }
  git_adapter$rr_git <- function(root, ...) {
    args <- c(...)
    if (identical(args, "--exec-path")) return(execution)
    if (identical(args[[1L]], "rev-parse")) return(file.path(fixture, "absent-attributes"))
    stop("Unexpected Git metadata query")
  }
  git_adapter$rr_run <- function(...) list(status = 1L, stdout = "")
  adapter$rr_git_environment <- git
  before <- observe()
  writeLines("Changed Git implementation", actual_git)
  expect(!identical(before$digest, observe()$digest),
    "Git implementation bytes behind an unchanged PATH launcher invalidate the environment")
  unlink(actual_git)
  rejects(observe(), "Unresolved Git implementation from --exec-path")
  cat("Python external cache/startup and actual Git implementation input contracts passed.\n")
}
run_runtime_input_contracts()
