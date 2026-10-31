# Dated investigation evidence; run from the repository root.
devtools::load_all(quiet = TRUE)

options(width = 120)

builders <- list(plain = function() 1:3, complex = function() c(1 + (0+1i), NA_complex_,
    2 + (0+0i)), posixlt = function() as.POSIXlt(c("2020-01-01", NA, "2021-01-01"),
    tz = "UTC"), duration = function() lubridate::dseconds(c(1, NA, 2)), period = function() lubridate::seconds(c(1,
    NA, 2)), interval = function() lubridate::interval(as.POSIXct("2020-01-01",
    tz = "UTC"), as.POSIXct("2020-01-01", tz = "UTC") + c(1, NA, 2)), num_version = function() numeric_version(c("1.1",
    "2.1", "3.1")), factor_na = function() structure(c(1L, 2L, NA_integer_), levels = c("a",
    NA_character_, "b"), class = "factor"), wk_wkt = function() wk::wkt(c("POINT (0 0)",
    NA, "POINT (1 1)")), wk_wkb = function() wk::as_wkb(wk::wkt(c("POINT (0 0)",
    NA, "POINT (1 1)"))), wk_xy = function() wk::xy(c(1, NA, 2), c(1, NA, 2)),
    wk_xyz = function() wk::xyz(c(1, NA, 2), c(1, NA, 2), c(1, NA, 2)), list_of_table = function() vctrs::list_of(table(c("a")),
        table(c("a", "a")), table(c("b"))), list_of_factor = function() vctrs::list_of(factor("a",
        levels = c("a", NA)), factor(NA, levels = c("a", NA)), factor("a", levels = c("a",
        NA))), nested_packed = function() tibble::tibble(x = tibble::tibble(a = factor(c("b",
        NA, "a"), levels = c("a", "b", NA)), b = as.Date(c("2020-01-01", NA, "2021-01-01")))),
    array_factor = function() structure(c(1L, NA_integer_, 2L, 2L, 1L, NA_integer_),
        dim = c(3L, 2L), levels = c("a", "b", NA), class = "factor"), matrix_date = function() structure(c(1,
        NA, 2, 3, 4, NA), dim = c(3L, 2L), class = "Date"), table_1d = function() table(c("a",
        "b", "b", "c", "c", "c")), empty_packed = function() vctrs::new_data_frame(list(),
        n = 3L), matrix_0 = function() matrix(integer(), nrow = 3L, ncol = 0L),
    array_0 = function() array(integer(), dim = c(3L, 0L, 2L)), array_4d = function() array(c(1,
        NA, 3, 4, 5, 6), dim = c(3L, 1L, 1L, 2L)), list_nested = function() list(list(NULL),
        NULL, list(list(NA))), asis_list = function() I(list(list(1), NULL, list(NA))),
    distribution = function() distributional::dist_normal(c(1, NA, 2), 1), difftime = function() as.difftime(c(1,
        NA, 2), units = "hours"))

output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("extended-types-"))

dir.create(output, recursive = TRUE, showWarnings = FALSE)

rows <- list()

index <- 0L

for (family in names(builders)) {
    key <- tryCatch(builders[[family]](), error = identity)
    if (inherits(key, "error")) {
        cat("BUILD", family, conditionMessage(key), "\n")
        next
    }
    for (n in c(0L, 1L, 3L)) for (packed in c(FALSE, TRUE)) {
        val <- tryCatch(vctrs::vec_slice(key, seq_len(n)), error = identity)
        if (inherits(val, "error")) {
            cat("SLICE", family, conditionMessage(val), "\n")
            next
        }
        if (packed)
            val <- tibble::tibble(inner = val)
        input <- tryCatch(tibble::tibble(g = val, h = rep(c("b", "a", "b"), length.out = n),
            f = rep(c("z", "z", NA), length.out = n), rid = seq_len(n)), error = identity)
        if (inherits(input, "error")) {
            cat("INPUT", family, conditionMessage(input), "\n")
            next
        }
        for (role in c("dimension", "fixed", "payload")) for (sort in c("none",
            "last", "first")) {
            spec <- if (role == "dimension")
                grouping_sets(grouping_set(g, h), grouping_set(h), grouping_set(g),
                  grouping_set())
            else rollup(h)
            fixed <- if (role == "fixed")
                "g"
            else "f"
            sets <- if (role == "dimension")
                list(c("g", "h"), "h", "g", character())
            else list("h", character())
            expected <- tryCatch({
                branches <- lapply(seq_along(sets), function(j) {
                  out <- input
                  for (nm in setdiff(if (role == "dimension")
                    c("g", "h")
                  else "h", sets[[j]])) out[[nm]] <- vctrs::vec_init(input[[nm]],
                    n)
                  out$sid <- rep(j, n)
                  out
                })
                tmp <- do.call(dplyr::union_all, branches[1:2])
                if (length(branches) > 2L) {
                  for (j in 3:length(branches)) {
                    tmp <- dplyr::union_all(tmp, branches[[j]])
                  }
                }
                tmp <- dplyr::select(tmp, dplyr::all_of(unique(c(fixed, if (role ==
                  "dimension") c("g", "h") else "h", "sid", names(input)))))
                tmp
            }, error = identity)
            actual <- tryCatch(expand_with_margins(input, .by = dplyr::all_of(fixed),
                .grouping = spec, .margin_label = NULL, .duplicates = "keep", .id = "sid",
                .sort = sort), error = identity)
            if (!inherits(actual, "error") && !inherits(expected, "error")) {
                aa <- dplyr::arrange(actual, .data$sid, .data$rid)
                ee <- dplyr::arrange(expected, .data$sid, .data$rid)
                status <- if (isTRUE(all.equal(aa, ee)))
                  "match"
                else "difference"
                detail <- if (status == "difference")
                  paste(all.equal(aa, ee), collapse = "; ")
                else ""
            }
            else {
                status <- if (inherits(actual, "error"))
                  if (inherits(expected, "error"))
                    "shared_error"
                  else "package_error"
                else "control_error"
                detail <- if (inherits(actual, "error"))
                  conditionMessage(actual)
                else conditionMessage(expected)
            }
            index <- index + 1L
            rows[[index]] <- data.frame(family, n, packed, role, sort, status,
                detail)
            if (status %in% c("difference", "package_error", "control_error"))
                cat(index, family, n, packed, role, sort, status, substr(detail,
                  1, 180), "\n")
        }
    }
}

out <- do.call(rbind, rows)

write.csv(out, file.path(output, "extended-types-results.csv"), row.names = FALSE)

print(with(out, table(status)))

print(with(out, table(family, status)))
