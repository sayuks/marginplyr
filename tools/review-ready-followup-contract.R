# Independently checks the review dependency expectation and its negative cases.
# The launcher separately protects every byte outside the registered vector.
source("tools/dependency-requirements.R")
description <- read.dcf("DESCRIPTION")
declared <- dependency_requirements(description[[1L, "Config/Needs/review"]])$package
generation <- paste(readLines("tools/review-ready-generated.R"), collapse = "\n")
for (call in c("roxygen2::roxygenise()", "devtools::build_readme(quiet = FALSE)")) {
  if (!grepl(call, generation, fixed = TRUE)) {
    stop("Unrecognized documentation tool use; review dependency contract unresolved.", call. = FALSE)
  }
}
if (!all(c("devtools", "roxygen2") %in% declared)) {
  stop("Declared review dependencies omit documentation tools actually used.", call. = FALSE)
}
expression <- parse(".github/scripts/verify-cran-preflight.R")
target <- Filter(function(call) {
  is.call(call) && identical(call[[1L]], as.name("expect_identical")) &&
    length(call) == 4L && identical(call[[2L]], quote(review_requirements$package)) &&
    identical(call[[4L]], "the review dependency set")
}, as.list(expression))
if (length(target) != 1L) stop("Protected review dependency assertion is missing.", call. = FALSE)
expected <- target[[1L]][[3L]]
if (!is.call(expected) || !identical(expected[[1L]], as.name("c")) ||
    !all(vapply(as.list(expected)[-1L], is.character, logical(1)))) {
  stop("Review dependency expectation must be a literal vector.", call. = FALSE)
}
expected <- eval(expected, baseenv())
if (!identical(declared, expected)) stop("Independent review dependency mismatch.", call. = FALSE)
# Use the verifier's actual assertion and dependency reader in a private scope;
# neither expected vectors nor a successful changed verifier prove strictness.
local({
  assertion <- expression[[which(vapply(as.list(expression), function(call) {
    is.call(call) && identical(call[[1L]], as.name("<-")) &&
      identical(call[[2L]], as.name("expect_identical"))
  }, logical(1)))]]
  scope <- new.env(parent = baseenv())
  eval(assertion, scope)
  verify <- function(packages) {
    scope$review_requirements <- dependency_requirements(paste(packages, collapse = ", "))
    eval(target[[1L]], scope)
  }
  verify(declared)
  for (tool in declared) {
    for (packages in list(setdiff(declared, tool),
                         replace(declared, match(tool, declared), "incorrectDependency"))) {
      failure <- tryCatch({ verify(packages); NULL }, error = identity)
      if (!inherits(failure, "error") ||
          !grepl("the review dependency set", conditionMessage(failure), fixed = TRUE)) {
        stop("Missing or incorrect dependencies escaped the strict assertion.", call. = FALSE)
      }
    }
  }
  original <- setdiff(declared, c("devtools", "roxygen2"))
  scope$review_requirements <- data.frame(package = declared)
  broken <- target[[1L]]
  broken[[3L]] <- as.call(c(list(as.name("c")), as.list(original)))
  failure <- tryCatch({ eval(broken, scope); NULL }, error = identity)
  if (!inherits(failure, "error")) stop("The #1107 mismatch was not reproduced.", call. = FALSE)
})
cat("Independent review dependency contract passed; original mismatch and negative cases rejected.\n")
