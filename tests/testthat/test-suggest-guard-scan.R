# `optional_suggest_spec()` marks a package no job withholds `asserted = FALSE`.
# One outside the hard dependency closure is still absent from the jobs that
# run only the CRAN tier (`.github/workflows/release-matrix.yaml`'s header), so
# an unguarded reference to it fails nowhere and this gate reads the test
# sources for one instead (*Structural gates* in `design/architecture.md`).
#
# The rule it holds is deliberately blunt: inside a `test_that()` call, a
# `skip_if_suggest_absent()` or `suggest_available()` naming the package as a
# literal must come earlier in the same call. A guard on another package does
# not count even when that package imports this one, and a reference outside
# any `test_that()` -- in a helper or at top level -- is never guarded.

# The packages one DESCRIPTION dependency field names, other than `R`, read the
# way the shipped guard reads Suggests.
field_packages <- function(field) {
  if (is.na(field)) {
    return(character())
  }
  setdiff(suggest_guard()$marginplyr_suggest_names(field), "R")
}

# Every package `roots` require through Depends and Imports, recursively, read
# from the installed packages' own metadata. Every one of them is installed
# wherever these tests run, `_R_CHECK_DEPENDS_ONLY_` included, so one that is
# not means this is not the closure a check installs, and the gate refuses to
# answer rather than guess. LinkingTo is left out: a binary install does not
# bring it.
hard_dependency_closure <- function(
  roots = field_packages(dcf_field(metadata_path("DESCRIPTION"), "Imports"))
) {
  closure <- character()
  pending <- roots
  while (length(pending) > 0L) {
    package <- pending[[1L]]
    pending <- pending[-1L]
    if (package %in% closure) {
      next
    }
    location <- find.package(package, quiet = TRUE)
    if (length(location) == 0L) {
      stop(sprintf(
        paste0(
          "{%s} is a hard dependency but is not installed, so the closure ",
          "cannot be computed."
        ),
        package
      ))
    }
    fields <- utils::packageDescription(
      package,
      lib.loc = dirname(location[[1L]]),
      fields = c("Depends", "Imports"),
      drop = FALSE
    )
    closure <- c(closure, package)
    pending <- c(pending, unlist(lapply(fields, field_packages)))
  }
  closure
}

# The Suggests this gate covers: tracked, never withheld by a job, and outside
# the hard closure. Empty is a legitimate answer, reached if such a package
# ever enters the closure.
unwithheld_suggests <- function(known = optional_suggests(),
                                closure = hard_dependency_closure()) {
  setdiff(names(known)[!known], closure)
}

suggest_guard_calls <- c("skip_if_suggest_absent", "suggest_available")
namespace_loading_calls <- c(
  "library", "require", "requireNamespace", "loadNamespace"
)

# The arguments of the call expression `call` in parse data `data`, as the ids
# of their expressions named by the argument name, or "" when positional.
call_arguments <- function(data, call) {
  children <- data[data$parent == call, , drop = FALSE]
  children <- children[order(children$line1, children$col1), , drop = FALSE]
  # The first child is the expression naming the function.
  children <- children[-1L, , drop = FALSE]
  ids <- integer()
  labels <- character()
  label <- ""
  for (i in seq_len(nrow(children))) {
    if (children$token[[i]] == "SYMBOL_SUB") {
      label <- children$text[[i]]
    } else if (children$token[[i]] == "expr") {
      ids <- c(ids, children$id[[i]])
      labels <- c(labels, label)
      label <- ""
    }
  }
  stats::setNames(ids, labels)
}

# The name the text of a symbol or string token spells, its quotes removed.
unquoted <- function(text) {
  as.character(str2lang(text))
}

# The package an argument expression names as a string literal, or, when
# `symbols` is TRUE, as a bare symbol too; NA when it is anything else.
literal_package <- function(data, expression, symbols = FALSE) {
  inner <- data[data$parent == expression, , drop = FALSE]
  if (nrow(inner) != 1L) {
    return(NA_character_)
  }
  if (inner$token == "STR_CONST" || (symbols && inner$token == "SYMBOL")) {
    return(unquoted(inner$text))
  }
  NA_character_
}

# The references to `packages` in one source that no guard covers, as
# `file:line {package}`.
unguarded_in_source <- function(file, packages) {
  data <- utils::getParseData(parse(file, keep.source = TRUE))
  if (is.null(data)) {
    return(character())
  }
  parent_of <- integer(max(data$id))
  parent_of[data$id] <- data$parent
  ancestors <- function(id) {
    found <- integer()
    id <- parent_of[[id]]
    while (id != 0L) {
      found <- c(found, id)
      id <- parent_of[[id]]
    }
    found
  }
  source_order <- function(ids) {
    rows <- match(ids, data$id)
    data$line1[rows] * 1e6 + data$col1[rows]
  }

  # Every call expression, by the name of the function it calls: the token's
  # parent is the expression naming the function, and its parent the call.
  callees <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
  call_ids <- parent_of[parent_of[callees$id]]
  calls_to <- function(names) call_ids[callees$text %in% names]

  # The package side of `::` and `:::` is the operator's first sibling, a
  # symbol or a string, either of which may carry quotes.
  operators <- data[data$token %in% c("NS_GET", "NS_GET_INT"), , drop = FALSE]
  sides <- data[data$parent %in% operators$parent, , drop = FALSE]
  sides <- sides[order(sides$line1, sides$col1), , drop = FALSE]
  sides <- sides[!duplicated(sides$parent), , drop = FALSE]
  named <- vapply(sides$text, unquoted, character(1), USE.NAMES = FALSE)
  reference_ids <- sides$id[named %in% packages]
  reference_packages <- named[named %in% packages]
  for (call in calls_to(namespace_loading_calls)) {
    # Every one of these takes the package as its first formal, `package`.
    arguments <- call_arguments(data, call)
    named <- arguments[names(arguments) == "package"]
    chosen <- if (length(named) > 0L) {
      named
    } else {
      arguments[!nzchar(names(arguments))]
    }
    if (length(chosen) == 0L) {
      next
    }
    package <- literal_package(data, chosen[[1L]], symbols = TRUE)
    if (package %in% packages) {
      reference_ids <- c(reference_ids, call)
      reference_packages <- c(reference_packages, package)
    }
  }

  tests <- calls_to("test_that")
  guards <- calls_to(suggest_guard_calls)
  # What each guard names: its literal arguments other than the two seams.
  guarded <- lapply(guards, function(guard) {
    arguments <- call_arguments(data, guard)
    arguments <- arguments[!names(arguments) %in% c("known", "suggests")]
    vapply(arguments, function(id) literal_package(data, id), character(1))
  })
  # A guard reaches the test it sits directly in and every test nested inside
  # that one, so only its innermost enclosing test is kept.
  guard_tests <- vapply(guards, function(guard) {
    c(intersect(ancestors(guard), tests), NA_integer_)[[1L]]
  }, integer(1))

  covered <- vapply(seq_along(reference_ids), function(i) {
    enclosing <- intersect(ancestors(reference_ids[[i]]), tests)
    any(vapply(seq_along(guards), function(j) {
      reference_packages[[i]] %in% guarded[[j]] &&
        guard_tests[[j]] %in% enclosing &&
        source_order(guards[[j]]) < source_order(reference_ids[[i]])
    }, logical(1)))
  }, logical(1))

  ranked <- order(source_order(reference_ids))
  ranked <- ranked[!covered[ranked]]
  sprintf(
    "%s:%d {%s}",
    basename(file),
    data$line1[match(reference_ids[ranked], data$id)],
    reference_packages[ranked]
  )
}

# Every reference in `files` to one of `packages` that no guard covers, as
# `file:line {package}`. `files` and `packages` are parameters so that the
# gate's own tests can drive it over fixtures with a fixed candidate set.
unguarded_suggest_references <- function(files, packages) {
  if (length(packages) == 0L) {
    return(character())
  }
  # A reference spells the package name, so a source that never does needs no
  # parse.
  mentions <- vapply(files, function(file) {
    text <- readLines(file, warn = FALSE)
    any(vapply(packages, function(package) {
      any(grepl(package, text, fixed = TRUE))
    }, logical(1)))
  }, logical(1))
  unlist(lapply(files[mentions], unguarded_in_source, packages = packages))
}

test_that("no test source references an unwithheld Suggest without its guard", {
  # The sources sit beside this file whenever the suite runs, so the scan needs
  # no installed copy; an empty listing would pass every verdict, so it fails.
  sources <- list.files(".", pattern = "^(test|helper)-.*[.]R$")
  expect_gt(length(sources), 1L)

  packages <- unwithheld_suggests()
  found <- unguarded_suggest_references(sources, packages)
  expect(
    length(found) == 0L,
    paste0(
      "These references reach a Suggest no job withholds and the hard ",
      "dependency closure does not supply, so a check without it skips them ",
      "or never loads them. Call `skip_if_suggest_absent(\"<package>\")` or ",
      "`suggest_available(\"<package>\")` earlier in the same `test_that()`:\n",
      paste0("- ", found, collapse = "\n")
    )
  )
})

test_that("the candidate set is computed from the installed closure", {
  expect_false("DBI" %in% unwithheld_suggests())
  expect_identical(
    unwithheld_suggests(
      known = c(arrow = TRUE, DBI = FALSE, bit64 = FALSE),
      closure = c("dbplyr", "DBI")
    ),
    "bit64"
  )
  expect_error(
    hard_dependency_closure("marginplyrAbsentSentinel"),
    "closure cannot be computed"
  )
})

# Puts a package of each name in `fields`, its DESCRIPTION carrying that
# element's fields, in a fresh library prefixed to the library paths until
# `env` exits.
local_fixture_library <- function(fields, env = parent.frame()) {
  library <- withr::local_tempdir(.local_envir = env)
  for (package in names(fields)) {
    dir.create(file.path(library, package))
    writeLines(
      c(paste("Package:", package), "Version: 1.5", fields[[package]]),
      file.path(library, package, "DESCRIPTION")
    )
  }
  withr::local_libpaths(library, action = "prefix", .local_envir = env)
}

test_that("a two-sided constraint in an installed Imports is one entry", {
  local_fixture_library(list(
    marginplyrFixtureA = "Imports: marginplyrFixtureB (>= 1.0, < 2.0)",
    marginplyrFixtureB = character()
  ))

  expect_identical(
    hard_dependency_closure("marginplyrFixtureA"),
    c("marginplyrFixtureA", "marginplyrFixtureB")
  )
})

test_that("a LinkingTo package absent at run time is outside the closure", {
  local_fixture_library(list(
    marginplyrFixtureC = "LinkingTo: marginplyrAbsentSentinel"
  ))

  expect_identical(
    hard_dependency_closure("marginplyrFixtureC"),
    "marginplyrFixtureC"
  )
})

# Writes each element of `sources` to a file of that name in a fresh directory
# and returns the paths, so the scan reads fixtures the way it reads the suite.
# The fixtures are strings, so the scan over this file does not report them.
fixture_sources <- function(sources, env = parent.frame()) {
  directory <- withr::local_tempdir(.local_envir = env)
  paths <- file.path(directory, names(sources))
  for (i in seq_along(sources)) {
    writeLines(sources[[i]], paths[[i]])
  }
  paths
}

unguarded_reference <- c(
  "test_that(\"fixture\", {",
  "  expect_true(TRUE)",
  "  bit64::as.integer64(\"1\")",
  "})"
)

test_that("an unguarded reference is reported in any test file", {
  paths <- fixture_sources(list(
    "test-share.R" = unguarded_reference,
    "test-share-integer64-keys.R" = unguarded_reference
  ))

  expect_identical(
    unguarded_suggest_references(paths, "bit64"),
    c("test-share.R:3 {bit64}", "test-share-integer64-keys.R:3 {bit64}")
  )
})

test_that("only an earlier guard naming the package covers a reference", {
  scan <- function(lines) {
    unguarded_suggest_references(
      fixture_sources(list("test-fixture.R" = lines)),
      "bit64"
    )
  }
  in_test <- function(...) c("test_that(\"fixture\", {", ..., "})")
  use <- "  bit64::as.integer64(1)"

  expect_identical(
    scan(in_test("  skip_if_suggest_absent(\"bit64\")", use)),
    character()
  )
  expect_identical(
    scan(in_test(
      "  skip_if_suggest_absent(\"dtplyr\", \"bit64\")",
      "  bit64:::as.integer64(1)"
    )),
    character()
  )
  expect_identical(
    scan(in_test(
      "  if (suggest_available(\"bit64\")) {",
      "    bit64::as.integer64(1)",
      "  }"
    )),
    character()
  )
  # dtplyr's own Imports do not stand in for a guard naming bit64.
  expect_identical(
    scan(in_test("  skip_if_suggest_absent(\"dtplyr\")", use)),
    "test-fixture.R:3 {bit64}"
  )
  expect_identical(
    scan(in_test(use, "  skip_if_suggest_absent(\"bit64\")")),
    "test-fixture.R:2 {bit64}"
  )
  # A guard in a different test does not reach this one.
  expect_identical(
    scan(c(
      in_test("  skip_if_suggest_absent(\"bit64\")"),
      in_test(use)
    )),
    "test-fixture.R:5 {bit64}"
  )
  # Nor does one in a test nested inside it, while one in an enclosing test
  # reaches a nested test.
  expect_identical(
    scan(c(
      "test_that(\"outer\", {",
      "  test_that(\"inner\", skip_if_suggest_absent(\"bit64\"))",
      use,
      "})"
    )),
    "test-fixture.R:3 {bit64}"
  )
  expect_identical(
    scan(in_test(
      "  skip_if_suggest_absent(\"bit64\")",
      "  test_that(\"inner\", {",
      use,
      "  })"
    )),
    character()
  )
  # A variable is not a literal, whatever it holds.
  expect_identical(
    scan(in_test(
      "  bit64 <- \"bit64\"",
      "  skip_if_suggest_absent(bit64)",
      "  bit64::as.integer64(1)"
    )),
    "test-fixture.R:4 {bit64}"
  )
})

test_that("references outside a test are never guarded", {
  paths <- fixture_sources(list("helper-fixture.R" = c(
    "skip_if_suggest_absent(\"bit64\")",
    "keys <- function() bit64::as.integer64(1)",
    "library(bit64)"
  )))

  expect_identical(
    unguarded_suggest_references(paths, "bit64"),
    c("helper-fixture.R:2 {bit64}", "helper-fixture.R:3 {bit64}")
  )
})

test_that("attaching and loading calls naming the package are references", {
  paths <- fixture_sources(list("test-fixture.R" = c(
    "test_that(\"fixture\", {",
    "  library(bit64)",
    "  require(\"bit64\")",
    "  requireNamespace(\"bit64\", quietly = TRUE)",
    "  loadNamespace(package = \"bit64\")",
    "  requireNamespace(\"dtplyr\")",
    "})"
  )))

  expect_identical(
    unguarded_suggest_references(paths, "bit64"),
    sprintf("test-fixture.R:%d {bit64}", 2:5)
  )
})

test_that("a quoted package name is a reference", {
  paths <- fixture_sources(list("test-fixture.R" = c(
    "test_that(\"fixture\", {",
    "  `bit64`::as.integer64(1)",
    "  \"bit64\":::as.integer64(1)",
    "})"
  )))

  expect_identical(
    unguarded_suggest_references(paths, "bit64"),
    sprintf("test-fixture.R:%d {bit64}", 2:3)
  )
})

test_that("comments, strings and packages in the closure are not reported", {
  paths <- fixture_sources(list("test-fixture.R" = c(
    "# bit64::as.integer64(1) and library(bit64)",
    "test_that(\"fixture\", {",
    "  expect_match(\"bit64::as.integer64\", \"bit64\")",
    "  DBI::dbConnect",
    "})"
  )))

  expect_identical(
    unguarded_suggest_references(paths, unwithheld_suggests()),
    character()
  )
  expect_identical(
    unguarded_suggest_references(paths, character()),
    character()
  )
})
