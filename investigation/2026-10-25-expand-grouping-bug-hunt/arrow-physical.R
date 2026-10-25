suppressPackageStartupMessages({
    library(pkgload)
    load_all(".", quiet = TRUE)
    library(dplyr)
    library(dbplyr)
    library(arrow)
})

specs <- list(null = list(c(NA, NA), null()), int8 = list(c(1L, 2L), int8()), uint64 = list(c(1,
    2), uint64()), float32 = list(c(1.25, 2.5), float32()), decimal = list(c(1.25,
    2.5), decimal128(10L, 2L)), date32 = list(as.Date(c("2000-01-01", "2000-01-02")),
    date32()), date64 = list(as.Date(c("2000-01-01", "2000-01-02")), date64()), time32 = list(hms::hms(seconds = c(3600,
    7200)), time32("s")), time64 = list(hms::hms(seconds = c(3600, 7200)), time64("us")),
    duration = list(as.difftime(c(3600, 7200), units = "secs"), duration("us")),
    timestamp = list(as.POSIXct(c("2000-01-01", "2000-01-02"), tz = "Asia/Tokyo"),
        timestamp("us", "Asia/Tokyo")), binary = list(list(charToRaw("a"), charToRaw("bb")),
        binary()), dictionary = list(factor(c("x", "y"), levels = c("y", "x")), dictionary(index_type = int8())),
    struct = list(data.frame(a = 1:2, b = c("x", "y")), struct(a = int32(), b = utf8())),
    list = list(list(1:2, 3:4), list_of(int32())))

problems <- list()

n <- 0L

for (nm in names(specs)) {
    a <- tryCatch(Array$create(specs[[nm]][[1]], type = specs[[nm]][[2]]), error = identity)
    if (inherits(a, "error")) {
        cat("ARRAY", nm, conditionMessage(a), "\n")
        next
    }
    source <- Table$create(g = a, v = 1:2)
    for (shape in c("table", "empty", "query", "dataset")) {
        src <- switch(shape, table = source, empty = head(source, 0L), query = mutate(source,
            v = v * 2L), dataset = InMemoryDataset$create(source))
        for (label in list(NULL, "Total")) for (sort in c("none", "last")) {
            key <- paste(nm, shape, if (is.null(label))
                "NULL"
            else label, sort, sep = "/")
            n <- n + 1L
            got <- tryCatch(expand_with_margins(src, .grouping = rollup(g), .margin_label = label,
                .id = "set", .sort = sort), error = identity)
            if (!inherits(got, "error"))
                got <- tryCatch(collect(got), error = identity)
            if (inherits(got, "error")) {
                cat("ERROR", key, conditionMessage(got), "\n")
                problems[[key]] <- got
            }
            else {
                stopifnot(nrow(got) == if (shape == "empty")
                  0L
                else 4L)
                cat("OK", key, "R", paste(class(got$g), collapse = ","), "\n")
            }
        }
    }
}

cat("TOTAL", n, "FAILURES", length(problems), "\n")

saveRDS(problems, "/tmp/margin-lazy-arrow-physical.rds")
