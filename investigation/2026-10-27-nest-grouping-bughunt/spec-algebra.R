pkgload::load_all(".", quiet = TRUE)

set.seed(94813)

d <- tibble::tibble(a = 1, b = 2, c = 3, d = 4, e = 5)

leaf <- function() sample(names(d), sample(0:3, 1), FALSE)

make <- function(depth = 0) {
    kind <- sample(if (depth > 2)
        c("grouping_set")
    else c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"), 1)
    n <- sample(if (kind == "grouping_sets")
        1:3
    else if (kind %in% c("rollup", "cube"))
        1:3
    else 0:3, 1)
    args <- replicate(n, if (kind == "grouping_set")
        leaf()
    else if (kind %in% c("rollup", "cube")) {
        if (runif(1) < 0.4)
            list(k = "grouping_set", a = list(sample(names(d), sample(1:3, 1))))
        else sample(names(d), sample(1:3, 1))
    }
    else if (runif(1) < 0.6)
        make(depth + 1)
    else leaf(), simplify = FALSE)
    list(k = kind, a = args)
}

expr <- function(x) {
    if (is.character(x))
        return(rlang::expr(tidyselect::all_of(!!x)))
    rlang::call2(x$k, !!!lapply(x$a, expr))
}

model <- function(x) {
    if (is.character(x))
        return(list(x))
    families <- lapply(x$a, model)
    switch(x$k, grouping_set = list(unique(unlist(families, use.names = FALSE))), grouping_sets = unlist(families,
        recursive = FALSE), rollup = {
        units <- unlist(lapply(x$a, function(arg) if (is.character(arg)) lapply(arg, identity) else model(arg)),
            recursive = FALSE)
        lapply(length(units):0, function(i) unique(unlist(units[seq_len(i)], use.names = FALSE)))
    }, cube = {
        units <- unlist(lapply(x$a, function(arg) if (is.character(arg)) lapply(arg, identity) else model(arg)),
            recursive = FALSE)
        unlist(lapply(length(units):0, function(size) lapply(utils::combn(seq_along(units),
            size, simplify = FALSE), function(ix) unique(unlist(units[ix], use.names = FALSE)))),
            recursive = FALSE)
    }, grouping_spec = Reduce(function(x, y) unlist(lapply(x, function(a) lapply(y, function(b) unique(c(a,
        b)))), recursive = FALSE), families, init = list(character())))
}

for (i in 1:3000) {
    ast <- make()
    ex <- expr(ast)
    spec <- eval(ex)
    expected <- model(ast)
    dims <- as.character(unique(unlist(expected, use.names = FALSE)))
    expected <- lapply(expected, function(s) dims[dims %in% s])
    plan <- inspect_grouping(d, .grouping = spec, .duplicates = "keep", .format = "list")
    if (!identical(plan$included, expected)) {
        cat("FAIL", i, rlang::expr_deparse(ex), "\n")
        dput(expected)
        dput(plan$included)
        quit(status = 1)
    }
    dropped <- inspect_grouping(d, .grouping = spec, .duplicates = "drop", .format = "list")
    stopifnot(identical(dropped$included, unique(expected)))
    if (i%%500 == 0)
        cat("PASS", i, "random specifications\n")
}
