# Run from the repository root. Evidence gathered on 2026-10-25.
pkgload::load_all(".", quiet = TRUE)

set.seed(20261025)

data <- tibble::tibble(k = "f", a = "A", b = 1, c = TRUE, d = as.Date("2020-01-01"))

cols <- letters[1:4]

leaf <- function() {
    chosen <- sample(cols, sample(0:4, 1))
    spell <- sample(c("all_of", "c", "positions"), 1)
    ast <- switch(spell, all_of = rlang::call2("all_of", chosen, .ns = "tidyselect"),
        c = rlang::call2("c", !!!rlang::syms(chosen)), positions = match(chosen,
            names(data)))
    list(ast = ast, columns = chosen)
}

combine <- function(left, right) unlist(lapply(left, function(x) lapply(right, function(y) unique(c(x,
    y)))), recursive = FALSE)

make <- function(depth = 0) {
    kind <- sample(if (depth >= 3)
        "grouping_set"
    else c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"), 1)
    n <- if (kind %in% c("rollup", "cube"))
        sample(1:3, 1)
    else if (kind == "grouping_sets")
        sample(1:3, 1)
    else sample(0:3, 1)
    args <- vector("list", n)
    families <- vector("list", n)
    units <- list()
    for (i in seq_len(n)) {
        if (kind %in% c("grouping_sets", "grouping_spec") && runif(1) < 0.7) {
            x <- make(depth + 1)
            args[[i]] <- x$ast
            families[[i]] <- x$sets
        }
        else {
            x <- leaf()
            if (kind %in% c("rollup", "cube") && length(x$columns) == 0)
                x <- list(ast = quote(a), columns = "a")
            if (kind %in% c("rollup", "cube") && runif(1) < 0.5) {
                args[[i]] <- rlang::call2("grouping_set", x$ast)
                units <- c(units, list(x$columns))
            }
            else {
                args[[i]] <- x$ast
                if (kind %in% c("rollup", "cube"))
                  units <- c(units, as.list(x$columns))
            }
            families[[i]] <- list(x$columns)
        }
    }
    sets <- switch(kind, grouping_set = list(unique(unlist(lapply(families, `[[`,
        1), use.names = FALSE))), grouping_sets = unlist(families, recursive = FALSE),
        grouping_spec = Reduce(combine, families, init = list(character())), rollup = lapply(rev(seq.int(0,
            length(units))), function(n) unique(unlist(units[seq_len(n)], use.names = FALSE))),
        cube = unlist(lapply(rev(seq.int(0, length(units))), function(n) lapply(utils::combn(seq_along(units),
            n, simplify = FALSE), function(ix) unique(unlist(units[ix], use.names = FALSE)))),
            recursive = FALSE))
    sets <- lapply(sets, function(x) if (is.null(x))
        character()
    else x)
    list(ast = rlang::call2(kind, !!!args), sets = sets)
}

for (i in 1:1000) {
    x <- make()
    dimensions <- unique(unlist(x$sets, use.names = FALSE))
    expected <- lapply(x$sets, function(x) dimensions[dimensions %in% x])
    for (policy in c("keep", "drop", "error")) {
        expected2 <- if (policy == "drop")
            expected[!duplicated(expected)]
        else expected
        actual <- tryCatch(inspect_grouping(data, .by = k, .grouping = !!x$ast, .duplicates = policy,
            .format = "list"), error = identity)
        if (policy == "error" && anyDuplicated(expected)) {
            stopifnot(inherits(actual, "marginplyr_error"), grepl("Duplicate grouping sets",
                conditionMessage(actual)))
            next
        }
        if (inherits(actual, "error") || !identical(actual$included, expected2)) {
            print(x$ast)
            print(x$sets)
            print(actual)
            stop("oracle mismatch")
        }
        stopifnot(identical(actual$set_id, seq_along(expected2)))
        stopifnot(all(vapply(actual$fixed, identical, logical(1), "k")))
        expected_ids <- vapply(expected2, function(s) as.integer(sum((!dimensions %in%
            s) * 2^rev(seq_along(dimensions) - 1))), integer(1))
        stopifnot(identical(actual$grouping_id, expected_ids))
    }
}

cat("PASS 1000 trees x 3 duplicate policies; fixed keys, membership, order, set_id, grouping_id\n")
