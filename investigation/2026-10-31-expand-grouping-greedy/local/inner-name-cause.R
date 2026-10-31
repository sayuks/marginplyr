# Dated investigation evidence; run from the repository root.
devtools::load_all(quiet = TRUE)

x <- tibble::tibble(key = summary(1)[1L], row = 1L)

y <- dplyr::mutate(x, key = vctrs::vec_init(key, 1L))

cat("Input names:", names(x), "\n")

cat("Prototype class:", class(y$key), "missing:", is.na(y$key), "\n")

print(dplyr::union_all(x, y))

seen <- list()

r <- tryCatch(vctrs::vec_rbind(x, y, .name_repair = function(nms) {
    seen[[length(seen) + 1L]] <<- nms
    if (anyDuplicated(nms) || !all(nzchar(nms)))
        vctrs::vec_as_names(nms, repair = "check_unique")
    nms
}), error = identity)

cat("Callback observations:\n")

dput(seen)

if (inherits(r, "error")) {
    cat(class(r)[1L], "\n")
    cat(conditionMessage(r), "\n")
}

cat("Default vec_rbind:\n")

print(vctrs::vec_rbind(x, y))
