# Exercise the pre-R Quarto installer through its process boundary.
main <- function() {
  workspace <- tempfile("quarto-setup-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  bin <- file.path(workspace, "bin")
  dir.create(bin)
  log <- file.path(workspace, "commands")
  state <- file.path(workspace, "installed")
  fixture <- c(
    "#!/usr/bin/env Rscript",
    "args <- commandArgs(TRUE)",
    "name <- basename(sub('^--file=', '', grep('^--file=',",
    "  commandArgs(FALSE), value = TRUE)))",
    "cat(name, paste(args, collapse = ' '), '\\n',",
    "  file = Sys.getenv('QUARTO_TEST_LOG'), append = TRUE)",
    "mode <- Sys.getenv('QUARTO_TEST_MODE')",
    "if (name == 'wget') { cat('504 Gateway Time-out\\n'); quit(status=4L) }",
    "if (name == 'gh') {",
    "  if ('Accept: application/octet-stream' %in% args) {",
    "    if (mode == 'download-failure') quit(status = 42L)",
    "    if (mode == 'empty-download') quit(status = 0L)",
    "    cat('fixture-binary')",
    "  } else if (any(endsWith(args, '/latest'))) {",
    "    cat(if (mode == 'invalid-version') 'nightly' else 'v1.11.9')",
    "  } else {",
    "    if (mode == 'missing-asset') quit(status = 0L)",
    "    cat(if (mode == 'invalid-url') 'https://example.com/asset' else",
    "      'https://api.github.com/repos/quarto-dev/quarto-cli/releases/assets/1')",
    "  }",
    "} else if (name == 'sudo') {",
    "  path <- args[[3L]]",
    "  stopifnot(identical(readChar(path, 14L), 'fixture-binary'))",
    "  if (mode == 'install-failure') quit(status = 43L)",
    "  writeLines('installed', Sys.getenv('QUARTO_TEST_STATE'))",
    "} else if (name == 'quarto') {",
    "  stopifnot(file.exists(Sys.getenv('QUARTO_TEST_STATE')))",
    "  cat(if (mode == 'wrong-version') '1.10.0' else '1.11.9')",
    "} else stop('Unexpected fixture command')"
  )
  for (name in c("gh", "sudo", "quarto", "wget")) {
    path <- file.path(bin, name)
    writeLines(fixture, path)
    Sys.chmod(path, "0755")
  }
  env <- Sys.getenv()
  env[c("PATH", "RUNNER_TEMP", "QUARTO_TEST_LOG", "QUARTO_TEST_STATE")] <-
    c(paste(bin, env[["PATH"]], sep = .Platform$path.sep),
      workspace, log, state)
  check <- function(ok, detail) {
    if (!isTRUE(ok)) stop(detail, call. = FALSE)
  }
  run <- function(os = "Linux", arch = "X64", mode = "") {
    unlink(c(log, state))
    env[c("RUNNER_OS", "RUNNER_ARCH", "QUARTO_TEST_MODE")] <-
      c(os, arch, mode)
    result <- processx::run(
      "bash", ".github/actions/setup-quarto/setup.sh", env = env,
      error_on_status = FALSE, timeout = 30000, cleanup_tree = TRUE
    )
    list(result = result, commands = if (file.exists(log)) {
      readLines(log, warn = FALSE)
    } else character())
  }
  # The exact failed transport remains red while the API route is tested.
  old <- processx::run("wget", paste0(
    "https://github.com/quarto-dev/quarto-cli/releases/download/",
    "v1.11.9/quarto-1.11.9-linux-amd64.deb"
  ), env = env, error_on_status = FALSE, timeout = 30000)
  check(old$status == 4L && grepl("504", old$stdout), old$stdout)
  for (case in list(c("Linux", "X64", "linux-amd64.deb", "dpkg -i"),
                   c("Linux", "ARM64", "linux-arm64.deb", "dpkg -i"),
                   c("macOS", "ARM64", "macos.pkg", "installer -pkg"))) {
    x <- run(case[[1L]], case[[2L]])
    output <- paste(x$result$stdout, x$result$stderr)
    check(x$result$status == 0L, output)
    check(any(grepl(case[[3L]], x$commands, fixed = TRUE)), output)
    check(any(grepl(case[[4L]], x$commands, fixed = TRUE)), output)
    check(!any(startsWith(x$commands, "wget ")), output)
    check(file.exists(state), output)
    check(!length(list.files(workspace, pattern = "^quarto[.]")),
          "Installer temporary directory was not collected")
  }
  for (mode in c("invalid-version", "missing-asset", "invalid-url", "empty-download",
                 "download-failure", "install-failure", "wrong-version")) {
    x <- run(mode = mode)
    check(x$result$status != 0L, paste("Accepted failure:", mode))
    if (mode %in% c("invalid-version", "missing-asset", "invalid-url",
                    "empty-download", "download-failure")) {
      check(!any(startsWith(x$commands, "sudo ")),
            paste("Installer ran after invalid download:", mode))
    }
    check(!length(list.files(workspace, pattern = "^quarto[.]")),
          "Failure left installer temporary files")
  }
  x <- run("Plan9")
  check(x$result$status != 0L, "Unsupported platform accepted")
  x <- run(arch = "unknown")
  check(x$result$status != 0L, "Unsupported architecture accepted")
  action <- yaml::read_yaml(".github/actions/setup-quarto/action.yaml")
  windows <- action$runs$steps[[1L]]
  check(identical(windows[["if"]], "runner.os == 'Windows'") &&
          identical(windows$uses, "quarto-dev/quarto-actions/setup@v2"),
        "Windows must retain the upstream Scoop installation route")
  cat("Quarto setup command fixtures passed.\n")
}
main()
