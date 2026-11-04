pkgload::load_all(quiet = TRUE)

dot_dimensions <- function() {
  constructors <- c("grouping_set", "grouping_sets", "grouping_spec",
                    "rollup", "cube")
  aliases <- c("..01", "..001", "..+1", ".. 1", "..\t1", ".. +02")
  count <- c(refusal = 0L, explicit = 0L, forwarded = 0L)
  for (name in aliases) {
    for (constructor in constructors) {
      for (depth in c(0L, 1L, 3L)) {
        argument <- as.name(name)
        for (i in seq_len(depth)) argument <- call("(", argument)
        build <- get(constructor)
        spec <- build(!!argument)
        explicit <- eval(call(constructor, call("all_of", name)))
        for (input_class in c("data.frame", "tibble")) {
          data <- data.frame(a = c("x", "y"), value = 1:2)
          data[[name]] <- c("p", "p")
          if (input_class == "tibble") data <- tibble::as_tibble(data)
          for (verb in c("inspect", "summary")) {
            consume <- switch(verb,
              inspect = function(grouping) {
                inspect_grouping(data, .grouping = grouping,
                                 .format = "list")$included
              },
              summary = function(grouping) {
                summarize_with_margins(
                  data, n = dplyr::n(), .grouping = grouping,
                  .id = "set", .margin_label = NULL
                )
              }
            )
            result <- tryCatch(consume(spec), error = identity)
            stopifnot(inherits(result, "rlang_error"),
                      grepl("used in an incorrect context",
                            conditionMessage(result), fixed = TRUE))
            count[["refusal"]] <- count[["refusal"]] + 1L
            control <- consume(explicit)
            if (verb == "inspect") {
              family <- if (constructor %in% c("cube", "rollup")) {
                list(name, character())
              } else {
                list(name)
              }
              stopifnot(identical(control, family))
            } else {
              stopifnot(name %in% names(control), all(control$n == 2L))
            }
            count[["explicit"]] <- count[["explicit"]] + 1L
          }
        }
      }
    }
  }

  for (name in aliases) {
    data <- data.frame(a = c("x", "y"))
    data[[name]] <- c("p", "p")
    for (depth in c(1L, 3L)) {
      argument <- as.name(name)
      for (i in seq_len(depth)) argument <- call("(", argument)
      body <- rlang::expr(inspect_grouping(
        data, .grouping = grouping_sets(!!argument), .format = "list"
      ))
      wrapper <- rlang::new_function(alist(... = ), body, environment())
      child <- grouping_set(a)
      supplied <- if (grepl("2$", name)) list(NULL, child) else list(child)
      stopifnot(identical(do.call(wrapper, supplied)$included, list("a")))
      count[["forwarded"]] <- count[["forwarded"]] + 1L
    }
  }
  print(count)
}

duplicate_prefixes <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = 1L, b = "x")
  remote <- dplyr::copy_to(con, data, "duplicate_matrix")
  derived <- dtplyr::lazy_dt(data) |> dplyr::mutate(b = toupper(b))
  counter <- new.env(parent = emptyenv())
  counter$queries <- 0L
  signature <- c("duckdb_connection", "character")
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    print = FALSE, tracer = function() {
      counter$queries <- counter$queries + 1L
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE)

  calls <- 0L
  predicate <- function(x) {
    calls <<- calls + 1L
    is.character(x)
  }
  specifications <- list(
    direct = list(grouping_sets(a, a, b),
                  grouping_sets(a, a, where(predicate))),
    nested = list(grouping_sets(grouping_set(a), grouping_set(a), b),
                  grouping_sets(grouping_set(a), grouping_set(a), where(predicate))),
    nonadjacent = list(grouping_sets(rollup(a), a, b),
                       grouping_sets(rollup(a), a, where(predicate))),
    product = list(
      grouping_sets(grouping_spec(grouping_set(a)), grouping_set(a), b),
      grouping_sets(grouping_spec(grouping_set(a)), grouping_set(a), where(predicate))
    )
  )
  verbs <- list(
    inspect = function(input, grouping, policy) {
      inspect_grouping(input, .grouping = grouping, .duplicates = policy,
                       .format = "list")
    },
    summary = function(input, grouping, policy) {
      summarize_with_margins(
        input, n = dplyr::n(), .grouping = grouping, .duplicates = policy,
        .margin_label = NULL, .id = "set"
      )
    }
  )
  count <- c(metadata_query = 0L, replaced_error = 0L,
             names_controls = 0L, policy_controls = 0L)
  for (forms in specifications) {
    for (verb in verbs) {
      for (input_name in c("data", "remote", "derived")) {
        input <- get(input_name)
        calls <- 0L
        before <- counter$queries
        control <- tryCatch(verb(input, forms[[1L]], "error"), error = identity)
        stopifnot(inherits(control, "marginplyr_error"), calls == 0L,
                  counter$queries == before,
                  grepl("Duplicate grouping sets", conditionMessage(control),
                        fixed = TRUE))
        count[["names_controls"]] <- count[["names_controls"]] + 1L
        result <- tryCatch(verb(input, forms[[2L]], "error"), error = identity)
        stopifnot(inherits(result, "marginplyr_error"))
        if (input_name == "derived") {
          stopifnot(calls == 0L, counter$queries == before,
                    grepl("Can't safely determine column metadata",
                          conditionMessage(result), fixed = TRUE))
          count[["replaced_error"]] <- count[["replaced_error"]] + 1L
        } else {
          expected_queries <- before + as.integer(input_name == "remote")
          stopifnot(calls == 2L, counter$queries == expected_queries,
                    identical(conditionMessage(result), conditionMessage(control)))
          if (input_name == "remote") {
            count[["metadata_query"]] <- count[["metadata_query"]] + 1L
          }
          for (policy in c("drop", "keep")) {
            calls <- 0L
            result <- verb(input, forms[[2L]], policy)
            stopifnot(calls == 2L, !is.null(result))
            count[["policy_controls"]] <- count[["policy_controls"]] + 1L
          }
        }
      }
    }
  }
  print(count)
}

dot_dimensions()
duplicate_prefixes()
