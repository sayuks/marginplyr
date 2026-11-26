# Installs throwaway packages into `<root>/lib` and returns that library, for a
# test that needs what only an installed namespace has: exports and imports
# that stay lazy-load promises until forced, or a foreign namespace exporting
# tidyselect's helper names. `packages` is a named list in installation order,
# so a package follows the fixtures it imports; each element holds `namespace`,
# the NAMESPACE lines, and `code`, the lines of its one R file. The caller owns
# `root`: it loads the fixtures with `load_fixture_package()` or by putting the
# library on `.libPaths()`, and unloads and deletes them.
install_fixture_packages <- function(root, packages) {
  lib <- file.path(root, "lib")
  dir.create(lib, recursive = TRUE)
  for (name in names(packages)) {
    pkg <- file.path(root, name)
    dir.create(file.path(pkg, "R"), recursive = TRUE)
    namespace <- packages[[name]]$namespace
    # `R CMD check --as-cran` sets `_R_CHECK_INSTALL_DEPENDS_`, under which
    # INSTALL lazy-loads with only DESCRIPTION's declared dependencies
    # visible, so every package an import directive names is declared.
    directives <- regmatches(
      namespace, regexec("^\\s*import(From)?\\(\\s*([^,)[:space:]]+)", namespace)
    )
    imports <- unique(vapply(
      Filter(length, directives), function(match) match[[3L]], character(1)
    ))
    writeLines(c(
      paste("Package:", name), "Version: 0.0.1", "Title: Fixture",
      "Description: Fixture package.", "License: MIT",
      if (length(imports)) paste("Imports:", paste(imports, collapse = ", ")),
      paste0("Authors@R: person('A', 'B', email = 'a@example.com', ",
             "role = c('aut', 'cre'))")
    ), file.path(pkg, "DESCRIPTION"))
    writeLines(namespace, file.path(pkg, "NAMESPACE"))
    writeLines(packages[[name]]$code, file.path(pkg, "R", "fixture.R"))
    status <- system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "INSTALL", "--no-byte-compile", "-l", shQuote(lib),
        shQuote(pkg)),
      stdout = TRUE, stderr = TRUE, env = paste0("R_LIBS=", shQuote(lib))
    )
    if (!is.null(attr(status, "status"))) {
      stop(paste(status, collapse = "\n"))
    }
  }
  lib
}

# Loads the fixture `name` from `lib`, installed by
# `install_fixture_packages()`. The name reaches `loadNamespace()` as a
# variable because R CMD check's package-usage scan reads a literal there as
# an undeclared dependency.
load_fixture_package <- function(name, lib) {
  loadNamespace(name, lib.loc = c(lib, .libPaths()))
}
