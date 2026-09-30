# Reproduce calibration observations with ordinary dtplyr controls.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
.libPaths(c(file.path(root, "library"), .Library))
setwd(file.path(root, "cwd"))
data <- data.frame(g = c("a", "a", "b"), v = c(1, 3, 6))
consumer <- loadNamespace("mpconsumerqualifieddtplyr")
ordinary <- function(x) sum(x) + 10000
namespace_offset <- 1000
x <- dtplyr::lazy_dt(data)
outside_margin <- dplyr::collect(consumer$lexical(x, 7))
outside_plain <- dplyr::collect(consumer$lexical_baseline(x, 7))
inside_margin <- dplyr::collect(consumer$lexical(data, 7, inside = TRUE))
inside_plain <- dplyr::collect(consumer$lexical_baseline(data, 7, inside = TRUE))
e <- new.env(parent = globalenv())
e$counted <- function(v) sum(v)
q <- rlang::new_quosure(quote(counted(v)), e)
attempt <- function(call) tryCatch(call, error = function(cnd)
  list(class = class(cnd), message = conditionMessage(cnd)))
missing_margin <- attempt(dplyr::collect(consumer$splice(x, list(total = q), consumer$make_spec())))
missing_plain <- attempt(dplyr::collect(dplyr::summarise(dplyr::group_by(x, g), total = !!q, .groups = "drop")))
stopifnot(identical(outside_margin$total[1:2], outside_plain$total),
          identical(inside_margin$total[1:2], inside_plain$total),
          identical(missing_margin$message, missing_plain$message))
dt <- asNamespace("dtplyr")
dput(list(outside_margin = outside_margin, outside_plain = outside_plain,
  inside_margin = inside_margin, inside_plain = inside_plain,
  missing_margin = missing_margin, missing_plain = missing_plain,
  dtplyr_version = as.character(utils::packageVersion("dtplyr")),
  lazy_dt = deparse(dt$lazy_dt), dt_eval = deparse(dt$dt_eval),
  search = search()), file.path(root, "calibration-controls.R"))
cat("Ordinary dtplyr controls reproduce root-environment lookup and missing function.\n")
