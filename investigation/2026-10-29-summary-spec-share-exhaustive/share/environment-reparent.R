# Visible environment reparenting invalidates share lookup proofs, 2026-10-29.
# --assert requires the documented dependency refusals, with no implementation edit.
args <- commandArgs(TRUE)
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
d <- tibble(g = c('a', 'b'), x = c(1, 3), prior = c(100, 300))
reads <- list(
  get = quote(get('prior', envir = e)),
  get0 = quote(base::get0('prior', envir = e)),
  mget = quote(mget('prior', envir = e, inherits = TRUE)[[1L]]),
  exists = quote(as.integer(exists('prior', envir = e)))
)
failures <- list()
checks <- 0L
for (kind in c('parent', 'total')) for (direction in c('source', 'derived')) {
  helper <- rlang::call2(paste0('share_of_', kind), quote(value))
  for (placement in c('same', 'prior', 'separate')) for (name in names(reads)) {
    e <- new.env(parent = baseenv())
    value <- if (direction == 'source') reads[[name]] else rlang::expr(sum(!!reads[[name]]))
    expression <- if (placement == 'same') rlang::expr({
      parent.env(e) <- environment(); !!value
    }) else value
    ordinary <- if (placement == 'prior') quote({
      parent.env(e) <- environment(); sum(x)
    }) else quote(sum(x))
    mark <- if (placement == 'separate') list(mark = quote({
      parent.env(e) <- environment(); 0
    })) else list()
    answer <- tryCatch(if (direction == 'source') {
      summarize_with_margins(d, !!!mark, prior = !!ordinary,
        value = !!expression, fraction = !!helper, .grouping = rollup(g))
    } else {
      summarize_with_margins(d, !!!mark, value = !!ordinary,
        prior = !!helper, derived = !!expression, .grouping = rollup(g))
    }, error = identity)
    checks <- checks + 1L
    correct <- inherits(answer, 'marginplyr_error')
    cat(kind, direction, placement, name, if (correct) 'REFUSED' else 'BYPASS', '\n')
    if (!correct) {
      failures[[paste(kind, direction, placement, name)]] <- answer
      if (inherits(answer, 'error')) {
        cat(class(answer)[[1L]], conditionMessage(answer), '\n')
      } else print(answer)
    }
  }
}
controls <- 0L
for (kind in c('parent', 'total')) {
  helper <- rlang::call2(paste0('share_of_', kind), quote(value))
  # A local assignment to the environment name is already conservatively read.
  for (expression in list(
    quote({ e <- environment(); get('prior', envir = e) }),
    quote({ parent.env(e) <- environment(); get('prior', envir = parent.env(e)) })
  )) {
    e <- new.env(parent = baseenv())
    answer <- tryCatch(summarize_with_margins(d, prior = sum(x),
      value = !!expression, fraction = !!helper, .grouping = rollup(g)), error = identity)
    stopifnot(inherits(answer, 'marginplyr_error'))
    controls <- controls + 1L
  }
  # A stable external binding with this name never reaches the mask.
  e <- new.env(parent = baseenv()); e$prior <- 7
  stable <- summarize_with_margins(d, prior = sum(x),
    value = get('prior', envir = e), fraction = !!helper, .grouping = rollup(g))
  stopifnot(identical(stable$value, c(7, 7, 7)), identical(stable$fraction, c(1, 1, 1)))
  controls <- controls + 1L
  # Reparenting explicitly to baseenv() leaves a genuine external binding external.
  e <- new.env(parent = emptyenv()); e$prior <- 7
  stable <- summarize_with_margins(d, prior = sum(x),
    value = { parent.env(e) <- baseenv(); get('prior', envir = e) },
    fraction = !!helper, .grouping = rollup(g))
  stopifnot(identical(stable$value, c(7, 7, 7)), identical(stable$fraction, c(1, 1, 1)))
  controls <- controls + 1L
  # A non-inheriting lookup cannot read a parent binding after reparenting.
  e <- new.env(parent = baseenv()); e$prior <- 7
  stable <- summarize_with_margins(d, prior = sum(x),
    value = { parent.env(e) <- environment(); get('prior', envir = e, inherits = FALSE) },
    fraction = !!helper, .grouping = rollup(g))
  stopifnot(identical(stable$value, c(7, 7, 7)), identical(stable$fraction, c(1, 1, 1)))
  controls <- controls + 1L
}
e <- new.env(parent = baseenv())
plain <- summarize_with_margins(d, prior = sum(x),
  value = { parent.env(e) <- environment(); get('prior', envir = e) },
  .grouping = rollup(g))
stopifnot(identical(plain$value, c(1, 3, 4)))
controls <- controls + 1L
cat('Dependency checks:', checks, 'missing refusals:', length(failures),
    'controls passed:', controls, '\n')
if ('--assert' %in% args && length(failures)) {
  stop(length(failures), ' reparented-environment dependency refusals were bypassed', call. = FALSE)
}
