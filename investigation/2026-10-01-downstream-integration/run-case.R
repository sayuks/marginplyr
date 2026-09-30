args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
mode <- args[[2]]
backend <- args[[3]]
phase <- args[[4]]
order <- if (length(args) >= 5L) args[[5]] else "consumer-first"
artifact <- normalizePath(args[[6]])
.libPaths(c(file.path(root, "library"), .Library))
setwd(file.path(root, "cwd"))
tag <- paste(phase, mode, backend, order, sep = "-")
suffix <- Sys.getenv("DOWNSTREAM_SUFFIX")
if (nzchar(suffix)) tag <- paste(tag, suffix, sep = "-")
outdir <- file.path(root, "results", tag)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
rows <- list()
state <- function(stage) {
  ns <- loadedNamespaces()
  info <- lapply(ns, function(p) list(package = p,
    version = as.character(utils::packageVersion(p)),
    path = if (p == "base") file.path(.Library, "base") else getNamespaceInfo(asNamespace(p), "path")))
  dput(list(stage = stage, search = search(), namespaces = ns,
    libpaths = .libPaths(), versions = info, R = R.version.string),
    file.path(outdir, paste0(stage, "-state.txt")))
  stopifnot(!any(c("package:marginplyr", "package:dplyr") %in% search()),
            !"pkgload" %in% loadedNamespaces(), !"testthat" %in% loadedNamespaces())
  paths <- vapply(info, function(x) normalizePath(x$path), character(1))
  stopifnot(all(startsWith(paths, file.path(root, "library")) |
                startsWith(paths, normalizePath(.Library))))
}
state("initial")
if (order == "backend-first") {
  loadNamespace("dplyr"); loadNamespace("dbplyr")
  if (backend != "local") loadNamespace(switch(backend,
    dtplyr = "dtplyr", sqlite = "RSQLite", duckdb = "duckdb", arrow = "arrow"))
}
template <- readLines(file.path(artifact, "consumer-template.R.in"))
direct <- new.env(parent = globalenv())
text <- paste(template, collapse = "\n")
for (pair in list(c("@MP@", "marginplyr::"), c("@DP@", "dplyr::"),
                  c("@DB@", "dbplyr::"))) text <- gsub(pair[1], pair[2], text, fixed = TRUE)
eval(parse(text = text), direct)
package <- paste0("mpconsumer", mode, backend)
if (mode == "direct") {
  consumer <- direct
} else {
  consumer <- loadNamespace(package)
  stopifnot(identical(environment(getExportedValue(package, "report")), consumer),
            identical(getNamespaceInfo(consumer, "path"), file.path(root, "library", package)))
  other <- paste0("mpconsumer", if (mode == "imports") "qualified" else "imports")
  stopifnot(!any(startsWith(loadedNamespaces(), other)))
  if (mode == "imports") {
    stopifnot(identical(get("collect", parent.env(consumer)), dplyr::collect),
              identical(get("rollup", parent.env(consumer)), marginplyr::rollup))
  }
}
state("consumer-loaded")
getfun <- function(name) get(name, consumer, inherits = FALSE)
got <- function(x) if (is.data.frame(x)) x else dplyr::collect(x)
canonical <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  if (!nrow(x)) return(x)
  keys <- lapply(x, function(v) if (is.factor(v)) as.character(v) else v)
  if (any(vapply(keys, is.list, logical(1)))) return(x)
  x <- x[do.call(base::order, c(keys, list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
same <- function(x, y, sorted = FALSE) {
  x <- as.data.frame(x); y <- as.data.frame(y)
  if (!sorted) { x <- canonical(x); y <- canonical(y) }
  rownames(x) <- rownames(y) <- NULL
  stopifnot(identical(names(x), names(y)), identical(nrow(x), nrow(y)))
  for (name in names(x)) {
    if (!identical(typeof(x[[name]]), typeof(y[[name]])))
      stop(sprintf("column %s: type %s versus %s", name, typeof(x[[name]]), typeof(y[[name]])))
    stopifnot(identical(class(x[[name]]), class(y[[name]])))
    if (is.factor(x[[name]])) stopifnot(identical(levels(x[[name]]), levels(y[[name]])))
  }
  if (!isTRUE(all.equal(x, y, tolerance = 1e-10)))
    stop(paste(all.equal(x, y, tolerance = 1e-10), collapse = "; "))
  invisible(TRUE)
}
record <- function(id, code, contract) {
  selected <- Sys.getenv("DOWNSTREAM_CASES")
  if (nzchar(selected) && !id %in% strsplit(selected, ",", fixed = TRUE)[[1]]) return(invisible(NULL))
  observation <- NULL
  error <- NULL
  warnings <- character()
  ok <- tryCatch(withCallingHandlers({ observation <- force(code); TRUE },
    warning = function(cnd) { warnings <<- c(warnings, conditionMessage(cnd)); invokeRestart("muffleWarning") }),
    error = function(cnd) { error <<- list(class = class(cnd), message = conditionMessage(cnd)); FALSE })
  dput(list(id = id, observation = observation, error = error, warnings = warnings),
       file.path(outdir, paste0(id, ".txt")))
  rows[[length(rows) + 1L]] <<- data.frame(id = id, mode = mode, backend = backend,
    phase = phase, order = order, status = if (ok) "NO VIOLATION FOUND" else "UNCLASSIFIED",
    contract = contract, detail = if (ok) "" else error$message)
  cat(id, if (ok) "PASS" else paste("OBSERVATION", error$message), "\n")
  invisible(ok)
}
data <- data.frame(g = c("a", "a", "b"), v = c(1, 3, 6))
connections <- list()
input <- function(data) {
  switch(backend,
    local = data,
    dtplyr = dtplyr::lazy_dt(data),
    sqlite = {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      connections[[length(connections) + 1L]] <<- con
      dplyr::copy_to(con, data, paste0("input_", length(connections)), temporary = TRUE)
    },
    duckdb = {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      DBI::dbExecute(con, "SET threads = 1")
      connections[[length(connections) + 1L]] <<- con
      dplyr::copy_to(con, data, paste0("input_", length(connections)), temporary = TRUE)
    },
    arrow = arrow::Table$create(data)
  )
}
x <- input(data)
check_report <- function(result, totals = c(4, 6, 10), label = NA_character_) {
  stopifnot(identical(names(result), c("g", "sid", "total", "rows")),
            nrow(result) == 3L,
            identical(result$g, c("a", "b", label)),
            isTRUE(all.equal(as.numeric(result$total), totals)),
            identical(as.integer(result$rows), c(2L, 1L, 3L)),
            identical(as.integer(result$sid), c(1L, 1L, 2L)),
            identical(dplyr::group_vars(result), character()))
  invisible(TRUE)
}
record("C01-comparator", {
  expected <- data.frame(g = c("a", "b"), v = c(4, 6))
  bad <- list(value = transform(expected, v = v + 1),
    type = transform(expected, v = as.integer(v)), missing = expected[1, ],
    duplicate = rbind(expected, expected[1, ]), swap = transform(expected, v = rev(v)))
  detected <- vapply(bad, function(z) inherits(try(same(z, expected), silent = TRUE), "try-error"), logical(1))
  stopifnot(all(detected)); detected
}, "Independent comparison mutation controls")
record("C02-installed-basic", {
  q <- getfun("report")(x, sort = "last")
  state("constructed")
  r <- got(q)
  same(r, got(direct$report(x, sort = "last")), TRUE)
  check_report(r)
  baseline <- got(getfun("baseline")(x))
  stopifnot(identical(baseline$g, c("a", "b")),
            identical(as.numeric(baseline$total), c(4, 6)))
  list(class = class(q), result = r, baseline = baseline)
}, "summarize_with_margins: value; ADR 0016/0018")
if (backend %in% c("local", "dtplyr")) {
  record("C03-lexical-delayed", {
    ordinary <- function(x) sum(x) + 10000
    namespace_offset <- 1000
    a <- getfun("lexical")(x, 7)
    b <- getfun("lexical")(x, 17)
    ra <- got(a); rb <- got(b)
    control <- got(getfun("lexical_baseline")(x, 7))
    same(ra[c("g", "total")][1:2, ], control, TRUE)
    expected_a <- if (backend == "dtplyr") c(10011, 10013, 10017) else c(111, 113, 117)
    expected_b <- if (backend == "dtplyr") c(10021, 10023, 10027) else c(121, 123, 127)
    stopifnot(identical(as.numeric(ra$total), expected_a),
              identical(as.numeric(rb$total), expected_b))
    same(rb, got(getfun("lexical")(x, 17)), TRUE)
    same(ra, got(getfun("lexical")(x, 7)), TRUE)
    # Build again, then explicitly reverse the retrieval order.
    a2 <- getfun("lexical")(x, 7); b2 <- getfun("lexical")(x, 17)
    rb2 <- got(b2); ra2 <- got(a2)
    same(rb, rb2, TRUE); same(ra, ra2, TRUE)
    list(alpha = ra, beta = rb, dplyr_control = control, wrapper_returned = TRUE)
  }, "ADR 0007; ordinary lexical/data-mask semantics")
  if (backend == "dtplyr" && mode != "direct") record("C04-input-inside", {
    q <- getfun("lexical")(data, 7, inside = TRUE)
    r <- got(q)
    stopifnot(identical(as.numeric(r$total), c(111, 113, 117)))
    list(class = class(q), result = r)
  }, "dtplyr lazy_dt caller environment; ADR 0007")
  record("C05-no-early-summary", {
    e <- new.env(parent = globalenv()); e$calls <- 0L
    e$counted <- function(v) { e$calls <- e$calls + 1L; sum(v) }
    e$data <- data
    z <- if (backend == "dtplyr") eval(quote(dtplyr::lazy_dt(data)), e) else x
    q <- rlang::new_quosure(quote(counted(v)), e)
    result <- getfun("splice")(z, list(total = q), getfun("make_spec")())
    before <- e$calls
    r <- got(result)
    after <- e$calls
    if (backend == "dtplyr") stopifnot(before == 0L, after > before)
    stopifnot(after == 3L)
    plain_q <- dplyr::summarise(dplyr::group_by(z, g), total = !!q, .groups = "drop")
    got(plain_q)
    stopifnot(identical(as.numeric(r$total), c(4, 6, 10)))
    list(before = before, after = after, result = r)
  }, "ADR 0020; ordinary dplyr evaluation per group/set")
}
if (backend == "sqlite") record("C06-public-generics", {
  q <- getfun("report")(x, sort = "last")
  stopifnot(inherits(q, "marginplyr_sqlite_typed_result"))
  sql <- getfun("finish")(q, "render")
  outside <- getfun("finish")(q)
  inside <- getfun("inside_report")(x)
  materialized <- getfun("finish")(q, "compute", "calibrated_result")
  same(outside, inside, TRUE); same(outside, materialized, TRUE)
  check_report(outside)
  list(sql = as.character(sql), class = class(q), result = outside)
}, "ADR 0031 B-direct amendment; NAMESPACE public registrations")

if (phase == "main") {
  record("W3-query-pair-orders", {
    orders <- list(c("beta", "alpha"), c("alpha", "beta"))
    answers <- list()
    for (iteration in seq_along(orders)) {
      pair <- list(alpha = getfun("report")(x, label = "Alpha", sort = "last"),
                   beta = getfun("report")(x, label = "Beta", sort = "first"))
      for (which in orders[[iteration]]) {
        r <- got(pair[[which]])
        if (which == "alpha") check_report(r, label = "Alpha") else {
          stopifnot(identical(r$g, c("Beta", "a", "b")),
                    identical(as.numeric(r$total), c(10, 4, 6)),
                    identical(as.integer(r$sid), c(2L, 1L, 1L)))
        }
        reference <- got(direct$report(x,
          label = if (which == "alpha") "Alpha" else "Beta",
          sort = if (which == "alpha") "last" else "first"))
        same(r, reference, TRUE)
        answers[[paste(iteration, which)]] <- r
      }
    }
    answers
  }, "ADR 0007/0018; independent same-process delayed query settings")
  if (backend %in% c("dtplyr", "duckdb", "sqlite")) record("W6-inside-outside-compute", {
    r <- getfun("finish")(getfun("report")(x, sort = "last"), "compute", "outside_generic")
    inside <- getfun("inside_report")(x, "compute", "inside_generic")
    same(r, inside, TRUE); check_report(r)
    list(outside = r, inside = inside)
  }, "Public dplyr compute dispatch within installed consumer and after return")
  if (backend %in% c("local", "dtplyr", "arrow")) record("W10-result-print", {
    q <- getfun("report")(x, sort = "last")
    before <- got(q)
    display <- getfun("finish")(q, "print")
    same(before, got(q), TRUE)
    stopifnot(length(display) > 0L)
    list(class = class(q), display = display, result = before)
  }, "Underlying result print dispatch; explicit display after laziness observation")
  for (kind in c("set", "rollup", "cube", "sets", "product")) {
    record(paste0("W2-spec-", kind), {
      s <- getfun("make_spec")("g", kind)
      other <- getfun("make_spec")("v", kind)
      pb <- getfun("inspect")(x, other)
      p <- getfun("inspect")(x, s)
      same(p, direct$inspect(x, direct$make_spec("g", kind)), TRUE)
      expected <- if (kind == "set") list("g") else list("g", character())
      stopifnot(identical(p$included, expected),
                identical(as.integer(p$set_id), seq_along(expected)))
      expected_b <- if (kind == "set") list("v") else list("v", character())
      stopifnot(identical(pb$included, expected_b))
      same(p, getfun("inspect")(x, s), TRUE)
      same(pb, getfun("inspect")(x, other), TRUE)
      list(alpha = p, beta = pb, specification_class = class(s))
    }, "grouping_set: How an argument is read; ADR 0026")
  }
  record("W1-forward", {
    r <- got(getfun("forward")(x, v, g, rows = dplyr::n()))
    same(r, got(direct$forward(x, v, g, rows = dplyr::n())), TRUE)
    stopifnot(identical(as.numeric(r$total), c(4, 6, 10)))
    r
  }, "ADR 0007; dplyr data masking and tidyselect")
  record("W1-splice-quosure", {
    q <- rlang::quo(sum(v))
    r <- got(getfun("splice")(x, list(total = q), getfun("make_spec")()))
    same(r, got(direct$splice(x, list(total = q), direct$make_spec())), TRUE)
    stopifnot(identical(as.numeric(r$total), c(4, 6, 10)))
    r
  }, "ADR 0007; quosure expression/environment")
  record("W1-default-null", {
    a <- getfun("report")(x)
    b <- getfun("report")(x, sort = "none", label = NULL)
    same(got(a), got(b)); got(a)
  }, "summarize_with_margins: Option arguments; .margin_label")
  record("W4-contextual", {
    check <- backend != "sqlite"
    shares <- backend != "arrow"
    r <- got(getfun("contextual")(x, check = check, shares = shares))
    same(r, got(direct$contextual(x, check = check, shares = shares)), TRUE)
    stopifnot(identical(as.numeric(r$total), c(4, 6, 10)),
              identical(as.integer(r$bit), c(0L, 0L, 1L)),
              identical(as.integer(r$gid), c(0L, 0L, 1L)))
    if (shares) stopifnot(isTRUE(all.equal(as.numeric(r$share), c(0.4, 0.6, 1))),
              isTRUE(all.equal(as.numeric(r$parent), c(0.4, 0.6, 1))))
    r
  }, "ADR 0019; grouping_bit/share_of_parent help")
  record("W4-shadow-contextual", {
    r <- got(getfun("shadow_contextual")(x))
    same(r, got(direct$shadow_contextual(x)), TRUE)
    stopifnot(identical(as.numeric(r$total), c(4, 6, 10)),
              identical(as.integer(r$bit), c(0L, 0L, 1L)))
    r
  }, "ADR 0019: caller binding never changes a recognized spelling")
  record("W1-fixed-keys", {
    d <- data; d$partition <- c("p", "p", "q")
    z <- input(d)
    r <- got(getfun("fixed")(z))
    same(r, got(direct$fixed(z)), TRUE)
    stopifnot(identical(r$partition, c("p", "p", "q", "q")),
              identical(r$g, c("a", NA_character_, "b", NA_character_)),
              identical(as.numeric(r$total), c(4, 4, 6, 6)))
    r
  }, "summarize_with_margins .by fixed keys and Margin order")
  record("W4-selection", {
    r <- got(getfun("selections")(x))
    same(r, got(direct$selections(x)), TRUE)
    stopifnot(identical(as.numeric(r$v_total), c(4, 6, 10)))
    r
  }, "summarize_with_margins: Relationship to dplyr summaries")
  for (kind in if (backend %in% c("local", "dtplyr")) c("summary", "expand", "nest", "nest_by") else c("summary", "expand")) {
    record(paste0("W5-verb-", kind), {
      q <- getfun("verb")(x, kind, NULL)
      r <- got(q)
      reference <- got(direct$verb(x, kind, NULL))
      same(r, reference, TRUE)
      if (kind == "summary") stopifnot(nrow(r) == 3L, identical(as.integer(r$rows), c(2L, 1L, 3L)))
      if (kind == "expand") stopifnot(nrow(r) == 6L, identical(sort(r$v), c(1, 1, 3, 3, 6, 6)))
      if (kind %in% c("nest", "nest_by")) {
        stopifnot(nrow(r) == 3L, identical(vapply(r$data, nrow, integer(1)), c(2L, 1L, 3L)),
                  all(vapply(r$data, is.data.frame, logical(1))),
                  identical(lapply(r$data, function(y) sort(y$v)), list(c(1, 3), 6, c(1, 3, 6))))
        if (kind == "nest_by") stopifnot(inherits(r, "rowwise_df"))
      }
      r
    }, "Margin verb value sections; ADR 0016/0018")
  }
  if (backend %in% c("local", "dtplyr")) {
    for (ordered in c(FALSE, TRUE)) for (kind in c("summary", "expand", "nest", "nest_by")) {
      record(paste0("W5-factor-", ordered, "-", kind), {
        f <- data; f$g <- factor(f$g, levels = c("a", "b"), ordered = ordered)
        z <- input(f)
        r <- got(getfun("verb")(z, kind))
        same(r, got(direct$verb(z, kind)), TRUE)
        stopifnot(is.factor(r$g), identical(is.ordered(r$g), ordered),
                  identical(levels(r$g), c("a", "b", "Total")))
        r
      }, "ADR 0012/0016; #491 factor restoration regression")
    }
    record("W3-quosure-caller", {
      multiplier <- 10
      r <- got(getfun("quosure_report")(x, sum(v) * multiplier, 7))
      same(r, got(direct$quosure_report(x, sum(v) * multiplier, 7)), TRUE)
      stopifnot(identical(as.numeric(r$total), c(47, 67, 107)))
      r
    }, "ADR 0007; nested quosure environments")
  }
  if (backend %in% c("sqlite", "duckdb")) {
    record("W7-native-portable", {
      native <- getfun("report")(x, sort = "last", duplicates = "drop")
      portable <- getfun("report")(x, sort = "last", duplicates = "keep")
      rn <- got(native); rp <- got(portable)
      same(rn, rp, TRUE); check_report(rn)
      sn <- as.character(getfun("finish")(native, "render"))
      sp <- as.character(getfun("finish")(portable, "render"))
      if (backend == "duckdb") stopifnot(grepl("GROUPING SETS", sn, fixed = TRUE))
      stopifnot(grepl("UNION ALL", sp, fixed = TRUE))
      same(rn, getfun("finish")(native, "compute", "native_result"), TRUE)
      same(rp, getfun("finish")(portable, "compute", "portable_result"), TRUE)
      list(native = sn, portable = sp, result = rn)
    }, "summarize_with_margins native/fallback; .duplicates and .id")
    record("W6-finite-typed", {
      typed <- data.frame(g = c(1L, 1L, 2L), v = c(1, 3, 6))
      z <- input(typed)
      q <- getfun("report")(z, sort = "last")
      r <- got(q)
      stopifnot(identical(r$g, c(1L, 2L, NA_integer_)))
      same(r, getfun("finish")(q, "compute", "typed_result"), TRUE)
      prefixes <- list()
      for (n in c(0L, 1L, 2L)) {
        prefix <- getfun("finish")(q, n = n)
        reference <- dplyr::collect(direct$report(z, sort = "last"), n = n)
        same(prefix, reference, TRUE)
        stopifnot(nrow(prefix) == n, identical(prefix$g, r$g[seq_len(n)]),
                  identical(prefix$sid, r$sid[seq_len(n)]))
        if (n > 0L) same(r[seq_len(n), , drop = FALSE], prefix, TRUE)
        prefixes[[as.character(n)]] <- prefix
      }
      plain_zero <- dplyr::collect(dplyr::summarise(dplyr::group_by(z, g), total = sum(v), .groups = "drop"), n = 0L)
      list(class = class(q), result = r, prefixes = prefixes, ordinary_zero = plain_zero)
    }, "ADR 0031 direct typed collection/materialization")
    record("W10-audit-print", {
      options(marginplyr.audit_sql = TRUE)
      a <- getfun("report")(x, sort = "last")
      ar <- getfun("audit")()
      b <- getfun("report")(x, label = "All", sort = "last")
      br <- getfun("audit")()
      got(a)
      same(br, getfun("audit")(), TRUE)
      before <- got(b)
      printed <- getfun("finish")(b, "print")
      same(before, got(b), TRUE)
      stopifnot("result" %in% br$purpose)
      list(first = ar, last = br, printed = printed)
    }, "last_sent_queries: What the record promises; result public projection")
  }
  if (backend == "duckdb") record("W8-duckdb-env-compute", {
    z <- input(data.frame(g = data$g, .env = data$v, check.names = FALSE))
    q <- getfun("verb")(z, "expand", NULL)
    stopifnot(inherits(q, "marginplyr_duckdb_env"))
    r <- got(q)
    same(r, getfun("finish")(q, "compute", "env_result"), TRUE)
    stopifnot(identical(names(r), c("g", "sid", ".env")), nrow(r) == 6L,
              identical(sort(r[[".env"]]), c(1, 1, 3, 3, 6, 6)))
    list(class = class(q), result = r)
  }, "SQL expansion .env boundary; public compute registration")
  if (backend == "arrow") record("W9-arrow-dataset", {
    path <- file.path(root, "cwd", paste0(tag, "-dataset"))
    dir.create(path, showWarnings = FALSE)
    arrow::write_dataset(data, path)
    z <- arrow::open_dataset(path)
    q <- getfun("report")(z, sort = "last")
    r <- got(q); check_report(r)
    same(r, got(direct$report(z, sort = "last")), TRUE)
    list(class = class(q), result = r)
  }, "Supported Arrow Dataset; translated expressions; same-process lifetime")
  record("W10-spec-print", {
    s <- getfun("make_spec")()
    p <- getfun("inspect")(x, s)
    display <- capture.output(print(s))
    same(p, getfun("inspect")(x, s), TRUE)
    stopifnot(length(display) > 0L)
    list(display = display, plan = p)
  }, "margin_grouping_spec print registration; inspect_grouping public tibble")
}
state("finished")
for (con in connections) if (DBI::dbIsValid(con)) {
  if (backend == "duckdb") DBI::dbDisconnect(con, shutdown = TRUE) else DBI::dbDisconnect(con)
}
utils::write.csv(do.call(rbind, rows), file.path(outdir, "cases.csv"), row.names = FALSE)
