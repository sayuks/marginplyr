pkgload::load_all(".", quiet = TRUE)

suppressPackageStartupMessages(library(dplyr))

set.seed(20261025)

count <- 0L

failures <- 0L

for (iter in 1:100L) {
    n <- sample(0:8, 1)
    d <- tibble(f = sample(c("z", "a", NA), n, TRUE), a = sample(c("b", "a", NA),
        n, TRUE), b = sample(c("Y", "X", NA), n, TRUE), c = sample(c("2", "1", NA),
        n, TRUE), rid = seq_len(n))
    k <- sample(1:8, 1)
    sets <- lapply(seq_len(k), function(i) sample(c("a", "b", "c"), sample(0:3, 1)))
    dims <- unique(unlist(sets))
    spec <- rlang::inject(grouping_sets(!!!lapply(sets, function(s) grouping_set(all_of(s)))))
    for (dup in c("keep", "drop")) for (sort in c("none", "first", "last")) {
        ss <- sets
        if (dup == "drop")
            ss <- ss[!duplicated(vapply(ss, function(s) paste(sort(s), collapse = "|"),
                character(1)))]
        expected <- bind_rows(lapply(seq_along(ss), function(i) {
            b <- d
            for (c in setdiff(dims, ss[[i]])) b[[c]] <- rep(NA_character_, n)
            b$set <- rep(as.integer(i), n)
            for (c in dims) b[[paste0("bit_", c)]] <- rep(as.integer(!c %in% ss[[i]]),
                n)
            b
        }))
        if (sort != "none") {
            terms <- list(rlang::expr(is.na(f)), rlang::sym("f"))
            for (c in dims) {
                bit <- rlang::sym(paste0("bit_", c))
                if (sort == "first")
                  bit <- rlang::expr(desc(!!bit))
                terms <- c(terms, list(bit, rlang::expr(is.na(!!rlang::sym(c))),
                  rlang::sym(c)))
            }
            expected <- arrange(expected, !!!terms, set)
        }
        expected <- select(expected, all_of(c("f", dims, "set", setdiff(names(d),
            c("f", dims)))))
        run <- function(x) expand_with_margins(x, .by = f, .grouping = spec, .margin_label = NULL,
            .duplicates = dup, .sort = sort, .id = "set")
        for (kind in c("local", "dtplyr", "data.table")) {
            x <- switch(kind, local = d, dtplyr = dtplyr::lazy_dt(d), data.table = data.table::as.data.table(d))
            actual <- tryCatch({
                z <- run(x)
                if (kind == "dtplyr")
                  z <- collect(z)
                as_tibble(z)
            }, error = identity)
            count <- count + 1L
            if (inherits(actual, "error")) {
                failures <- failures + 1L
                cat("ERROR", iter, kind, dup, sort, conditionMessage(actual), "\n")
                next
            }
            if (!identical(actual, expected)) {
                failures <- failures + 1L
                cat("BAD", iter, kind, dup, sort, "\n")
                dput(sets)
                print(actual)
                print(expected)
                print(all.equal(actual, expected))
            }
        }
    }
}

cat("CASES", count, "\n")

stopifnot(count == 1800L, failures == 0L)
