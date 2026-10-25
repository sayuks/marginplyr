pkgload::load_all(".", quiet = TRUE)

library(dplyr)

nms <- c(".data", ".env", ".N", ".I", ".BY", ".SD", ".GRP", ".NGRP", ".EACHI", "..1",
    "..2", "...", "x...1", "a b", "a`b", "a\nb", "a\tb", "..marginplyr_sort_1", "..marginplyr_set_id_1",
    "new_levels", "col", "result", "source", "set_id", "n", "TRUE", "NA", "if", "_",
    "é", "😺", "")

for (nm in nms) for (role in c("dimension", "fixed", "payload", "id")) for (sort in c("none",
    "first", "last")) {
    d <- tibble(g = c("b", "a"), f = c("x", "y"), v = 1:2)
    if (role != "id")
        names(d)[switch(role, dimension = 1L, fixed = 2L, payload = 3L)] <- nm
    run <- function(x) expand_with_margins(x, .by = all_of(if (role == "fixed")
        nm
    else "f"), .grouping = rollup(all_of(if (role == "dimension")
        nm
    else "g")), .id = if (role == "id")
        nm
    else "set", .sort = sort)
    a <- tryCatch(run(d), error = identity)
    b <- tryCatch(collect(run(dtplyr::lazy_dt(d))), error = identity)
    if (inherits(a, "error") || inherits(b, "error")) {
        if (inherits(a, "error") != inherits(b, "error"))
            cat("ERROR", encodeString(nm), role, sort, "LOCAL", if (inherits(a, "error"))
                conditionMessage(a)
            else "ok", "DT", if (inherits(b, "error"))
                conditionMessage(b)
            else "ok", "\n")
    }
    else if (!isTRUE(all.equal(a, b))) {
        cat("VALUE", encodeString(nm), role, sort, "\n")
        print(a)
        print(b)
        print(all.equal(a, b))
    }
}

cat("DONE\n")
