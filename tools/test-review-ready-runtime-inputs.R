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
  cached <- trimws(rr_run(python, c("-S", "-c", compile), env = values)$stdout)
  expect(file.exists(cached) && startsWith(cached, paste0(values[["PYTHONPYCACHEPREFIX"]], "/")),
    "The actual Python runtime creates the selected external stdlib cache")

  observer <- rr_environment
  adapter <- new.env(parent = environment(observer))
  environment(observer) <- adapter
  fixed_tool <- file.path(fixture, "unchanged-artifact-tool")
  writeLines("Unrelated artifact-tool bytes", fixed_tool)
  lookup <- Sys.which
  adapter$Sys.which <- function(names) {
    found <- lookup(names)
    found[names %in% c("jarl", "quarto", "actionlint")] <- fixed_tool
    found
  }
  adapter$rr_renderer <- function() list(identity = list())
  adapter$rr_git_environment <- function(...) list()
  adapter$rr_native_file <- function(...) FALSE
  adapter$rr_directory_identity <- function(...) list(digest = "unchanged installation", natives = character())
  adapter$rr_linkage <- function(...) "unchanged native linkage"
  # Cache cases omit host site startup; the refusal case loads its own startup module normally.
  python_environment <- rr_python_environment
  python_adapter <- new.env(parent = environment(python_environment))
  environment(python_environment) <- python_adapter
  python_adapter$rr_run <- function(command, args, env, ...) {
    flags <- if (identical(env[["PYTHONPATH"]], "")) "-S" else character()
    rr_run(command, c(flags, args), env = env, ...)
  }
  adapter$rr_python_environment <- python_environment
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

  tree <- rr_mkdir(file.path(fixture, "runtime-tree"))
  expect(nzchar(rr_directory_identity(tree)$digest), "A readable empty runtime directory has an identity")
  nested <- rr_mkdir(file.path(tree, "nested"))
  writeLines("Readable runtime asset", file.path(nested, "asset"))
  for (denied in c(tree, nested)) {
    inspect <- rr_directory_identity
    inventory <- new.env(parent = environment(inspect))
    environment(inspect) <- inventory
    inventory$list.files <- function(path, ...) {
      if (identical(path, denied)) return(character())
      base::list.files(path, ...)
    }
    rejects(inspect(tree), "External directory inventory unavailable")
  }
  Sys.chmod(nested, "0000")
  tryCatch({
    if (file.access(nested, 4L) != 0L) {
      rejects(rr_directory_identity(tree), "External directory inventory unavailable")
      rejects(rr_directory_identity(nested), "External directory inventory unavailable")
    }
  }, finally = Sys.chmod(nested, "0700"))

  empty <- file.path(fixture, "empty-file")
  expect(file.create(empty) && identical(rr_file_identity(empty)$digest, rr_hash(raw())),
    "An empty regular file remains a supported input")
  alias <- file.path(fixture, "regular-alias")
  expect(file.symlink(empty, alias) && identical(rr_file_identity(alias)$resolved, normalizePath(empty)),
    "External regular-file symlink targets remain supported")
  pipe <- file.path(fixture, "input.fifo")
  expect(system2(Sys.which("mkfifo"), shQuote(pipe)) == 0L, "Create a real nonregular input")
  inspect <- rr_file_identity
  environment(inspect) <- new.env(parent = environment(inspect))
  environment(inspect)$file <- function(...) stop("A nonregular input reached stream open")
  rejects(inspect(pipe), "Unregistered external file type")
  rejects(rr_file_identity("/dev/null"), "Unregistered external file type")
  cat("Python external cache/startup and actual Git implementation input contracts passed.\n")
  cat("Unavailable runtime root/descendant inventories are refused; readable empty directories are accepted.\n")
  cat("Regular empty files and symlinks are accepted; FIFO and device streams are refused before opening.\n")
}
run_runtime_input_contracts()
