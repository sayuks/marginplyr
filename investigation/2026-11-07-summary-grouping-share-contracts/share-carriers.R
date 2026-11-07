pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[[1L]] %in% c("observe", "contract"))

source_data <- tibble::tibble(
  g = c("x", "y"), x = c(1, 3), prior = c(10, 20)
)
source_bytes <- serialize(source_data, NULL)
cases <- list(
  flow = list(
    block = c("h <- {list(f)}", "h[[1L]]()"),
    block_rebind = c("h <- { local_f <- f; list(local_f) }", "h[[1L]]()"),
    branch = c("h <- if (TRUE) list(f, safe) else list(safe, f)", "h[[1L]]()"),
    switch = c("h <- switch('yes', yes = list(f), list(safe))", "h[[1L]]()")
  ),
  transform = list(
    rev = c("h <- rev(list(safe, f))", "h[[1L]]()"),
    unname = c("h <- unname(list(a = f, b = safe))", "h[[1L]]()"),
    rep = c("h <- rep(list(safe, f), 2L)", "h[[4L]]()"),
    as_list = c("h <- as.list(list(f, safe))", "h[[1L]]()"),
    structure = c("h <- structure(list(f, safe), names = c('a', 'b'))", "h$a()"),
    set_names = c("h <- stats::setNames(list(f, safe), c('a', 'b'))", "h$a()"),
    tail = c("h <- utils::tail(list(safe, f), 1L)", "h[[1L]]()")
  ),
  extract = list(
    get_element = c("h <- list(a = f, b = safe)", "getElement(h, 'a')()"),
    subset2 = c("h <- list(a = f, b = safe)", ".subset2(h, 'a')()")
  ),
  factory = list(
    dots = c("h <- (function(...) list(...))(f)", "h[[1L]]()"),
    default = c("h <- (function(callback = f) list(callback))()", "h[[1L]]()"),
    actual = c("h <- (function(callback) list(callback))(f)", "h[[1L]]()")
  ),
  invoke = list(
    do_call = c("h <- list(a = f)", "do.call(h$a, list())")
  ),
  loop = list(
    singleton = c("h <- list(a = safe); for (i in 1L) h$a <- f", "h$a()")
  ),
  environment = list(
    list2env = c("h <- list2env(list(a = f))", "h$a()"),
    new_env = c("h <- new.env(); h$a <- f", "h$a()")
  ),
  adjacent = list(
    dynamic = c("h <- list(a = f)", "h[[paste0('a')]]()"),
    sequence = c("h <- list(f)[seq_len(1L)]", "h[[1L]]()"),
    local_write = c("h <- list(a = safe); local(h$a <<- f)", "h$a()")
  ),
  repaired = list(
    direct = c("h <- list(f)", "h[[1L]]()"),
    identity = c("h <- identity(list(f))", "h[[1L]]()"),
    subset = c("h <- list(f)[c(TRUE, FALSE)]", "h[[1L]]()"),
    factory_list = c("h <- (function(value) value)(list(f))", "h[[1L]]()"),
    factory_new = c("h <- (function() list(f))()", "h[[1L]]()"),
    exact_member = c("h <- list(a = f, b = safe)", "h$a()"),
    flat_write = c("h <- list(a = safe); h$a <- f", "h$a()"),
    empty_loop = c("h <- list(a = safe); for (i in NULL) h$a <- f", "h$a()"),
    env_replacement = c("h <- list2env(list(a = f)); h$a <- safe", "h$a()"),
    factory_literal = c(
      "h <- (function(callback = function() 7) list(callback))()",
      "h[[1L]]()"
    ),
    branch_write = c("h <- list(a = safe); if (TRUE) h$a <- f", "h$a()"),
    nested = c("h <- list(list(f))", "h[[c(1L, 1L)]]()")
  )
)
family <- args[[2L]]
stopifnot(family %in% names(cases))

make_summary <- function(parts, binding = c("removed", "retained", "independent")) {
  binding <- match.arg(binding)
  callback <- if (binding == "independent") "function() 11" else "function() sum(prior)"
  remove <- if (binding == "retained") "" else "rm(prior);"
  rlang::parse_expr(sprintf(
    "(function(prior) { safe <- function() 13; f <- %s; %s; %s %s })(11)",
    callback, parts[[1L]], remove, parts[[2L]]
  ))
}

run <- function(expr, share, direction) {
  spec <- marginplyr::rollup(g)
  share_call <- rlang::call2(
    share, quote(measure), .ns = "marginplyr"
  )
  tryCatch(
    if (direction == "source") {
      marginplyr::summarize_with_margins(
        source_data, prior = sum(x), measure = !!expr, ratio = !!share_call,
        .grouping = spec, .sort = "last"
      )
    } else {
      marginplyr::summarize_with_margins(
        source_data, measure = sum(x), prior = !!share_call, ratio = !!expr,
        .grouping = spec, .sort = "last"
      )
    },
    error = identity
  )
}

refused <- function(result, direction) {
  fragment <- if (direction == "source") {
    "depends on earlier summary alias `prior`"
  } else {
    "Ordinary summaries cannot use an earlier .* share"
  }
  inherits(result, "marginplyr_error") &&
    grepl(fragment, conditionMessage(result))
}

matches <- logical()
for (share in c("share_of_total", "share_of_parent")) {
  for (direction in c("source", "later")) {
    field <- if (direction == "source") "measure" else "ratio"
    stopifnot(refused(run(make_summary(cases$repaired$direct), share, direction),
                     direction))
    for (name in names(cases[[family]])) {
      parts <- cases[[family]][[name]]
      legal <- name %in% c("factory_literal", "empty_loop", "env_replacement")
      legal_value <- if (name == "factory_literal") 7 else 13
      for (binding in c("retained", "independent")) {
        control <- run(make_summary(parts, binding), share, direction)
        expected <- if (legal) legal_value else 11
        stopifnot(identical(control[[field]], rep(expected, 3L)))
      }
      actual <- run(make_summary(parts), share, direction)
      blocked <- refused(actual, direction)
      values <- if (blocked) {
        "refused"
      } else {
        if (inherits(actual, "error")) stop(actual)
        expected <- if (legal) {
          rep(legal_value, 3L)
        } else if (direction == "source") {
          c(1, 3, 4)
        } else {
          c(10, 20, 30)
        }
        stopifnot(identical(actual[[field]], expected))
        if (direction == "source" && !legal) {
          stopifnot(identical(actual$ratio, c(0.25, 0.75, 1)))
        }
        paste(actual[[field]], collapse = ",")
      }
      cat(name, share, direction, values, "\n")
      matches <- c(matches, blocked == !legal)
    }
  }
}
stopifnot(identical(serialize(source_data, NULL), source_bytes))
if (args[[1L]] == "contract" || family == "repaired") {
  stopifnot(all(matches))
} else {
  stopifnot(!any(matches))
}
