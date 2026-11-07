args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[[1L]] else "observation"
stopifnot(mode %in% c("observation", "contract", "controls"))
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)

input <- tibble::tibble(a = c("A", "B"), b = 1L, value = 1:2)
peek <- tidyselect::peek_data
make_case <- function(operator, wrapped = FALSE) {
  env <- new.env(parent = globalenv())
  env$types <- character()
  selector <- function(...) {
    env$types <- c(env$types, typeof(peek()$a))
    if (is.character(peek()$a)) "a" else "b"
  }
  env[[operator]] <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (pkg == "base" && name == "~") return(selector)
    if (operator == "::") getExportedValue(pkg, name) else getFromNamespace(name, pkg)
  }
  head <- as.call(list(as.name(operator), as.name("base"), as.name("~")))
  if (wrapped) head <- as.call(list(as.name("("), head))
  list(env = env, quo = rlang::new_quosure(as.call(list(head, quote(a))), env))
}

read_names <- function(result, kind) {
  if (kind == "by") result$fixed[[1L]] else result$included
}
attempt <- function(input, case, kind, consumer) {
  q <- case$quo
  if (consumer == "inspect") {
    if (kind == "by") return(inspect_grouping(input, .by = !!q, .grouping = grouping_set(), .format = "list"))
    return(inspect_grouping(input, .grouping = grouping_sets(!!q, b), .format = "list"))
  }
  if (kind == "by") return(summarize_with_margins(input, count = dplyr::n(), .by = !!q, .grouping = grouping_set()))
  summarize_with_margins(input, count = dplyr::n(), .grouping = grouping_sets(!!q, b))
}
inputs <- list(tibble = input, data.frame = as.data.frame(input), data.table = data.table::as.data.table(input), dtplyr = dtplyr::lazy_dt(input))
rows <- list()
for (operator in c("::", ":::")) {
  direct <- make_case(operator)
  stopifnot(identical(names(tidyselect::eval_select(direct$quo, input)), "a"))
  for (input_name in names(inputs)) for (kind in c("by", "union")) for (consumer in c("inspect", "summarize")) {
    case <- make_case(operator, wrapped = mode == "controls")
    before <- serialize(inputs[[input_name]], NULL)
    actual <- tryCatch(attempt(inputs[[input_name]], case, kind, consumer), error = identity)
    unchanged <- identical(before, serialize(inputs[[input_name]], NULL))
    if (consumer == "inspect") {
      good <- !inherits(actual, "error") && identical(read_names(actual,kind), if (kind == "by") "a" else list("a", "b"))
      observed <- if (kind == "by") !inherits(actual,"error") && identical(read_names(actual,kind),"b") else inherits(actual,"marginplyr_error") && grepl("duplicate",conditionMessage(actual))
    } else {
      if (!inherits(actual, "error")) actual <- tibble::as_tibble(actual)
      good <- !inherits(actual,"error") && "a" %in% names(actual) && if (kind == "by") identical(actual$count,c(1L,1L)) else identical(sort(actual$count),c(1L,1L,2L))
      observed <- if (kind == "by") !inherits(actual,"error") && identical(names(actual),c("b","count")) && identical(actual$count,2L) else inherits(actual,"marginplyr_error") && grepl("duplicate",conditionMessage(actual))
    }
    rows[[length(rows)+1L]] <- data.frame(operator,input_name,kind,consumer,good,observed,types=paste(case$env$types,collapse=","),unchanged)
    stopifnot(unchanged)
    if (mode == "observation" && !if (operator == "::") observed else good) {
      print(tail(rows,1L)); print(actual); stop("Unexpected observation")
    }
    if (mode == "controls") stopifnot(good, all(case$env$types == "character"))
  }
}
results <- do.call(rbind,rows)
print(results,row.names=FALSE)
cat("MODE",mode,"rows",nrow(results),"correct",sum(results$good),"dated failures",sum(results$observed),"\n")
if (mode == "contract" && !all(results$good)) quit(status=1L)

# Ordinary bare and genuinely qualified formula controls remain caller errors.
if (mode == "controls") {
  for (expr in list(quote(~a), quote(base::`~`(a)), quote(base:::`~`(a)))) {
    actual <- tryCatch(inspect_grouping(input, .grouping=grouping_set(!!expr)),error=identity)
    stopifnot(inherits(actual,"error"))
  }
  cat("PASS 3 genuine/bare formula refusal controls.\n")
}
