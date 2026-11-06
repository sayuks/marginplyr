# Finite dated investigation. No package implementation or tests are changed.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(list(value = force(expr)), error = function(e) list(error = class(e)[[1L]], message = conditionMessage(e)))
fixtures <- list(
 ts = function(n) ts(seq_len(n), start = c(2000, 1), frequency = 12),
 zoo = function(n) zoo::zoo(seq_len(n), order.by = as.Date("2000-01-01") + seq_len(n)),
 yearmon = function(n) zoo::as.yearmon(rep(c("2000-02", NA, "2000-01"), length.out = n)),
 yearqtr = function(n) zoo::as.yearqtr(rep(c("2000 Q2", NA, "2000 Q1"), length.out = n)),
 period = function(n) lubridate::period(rep(c(2, NA, 1), length.out = n)),
 duration = function(n) lubridate::dseconds(rep(c(2, NA, 1), length.out = n)),
 interval = function(n) lubridate::interval(as.POSIXct("2000-01-01", tz = "UTC") + rep(c(2, NA, 1), length.out = n), as.POSIXct("2000-01-03", tz = "UTC") + rep(c(2, NA, 1), length.out = n)),
 AsIs_complex = function(n) I(rep(c(2+1i, NA_complex_, 1+2i), length.out = n)),
 AsIs_raw = function(n) I(as.raw(seq_len(n))),
 expression = function(n) rep(expression(a, NULL, NA), length.out = n),
 call_list = function(n) rep(list(quote(x + 1), NULL, quote(f(a=2))), length.out = n),
 pairlist_list = function(n) rep(list(pairlist(a=1), NULL, pairlist(b=2)), length.out = n),
 blob = function(n) blob::blob(!!!rep(list(as.raw(1:2), NULL, as.raw(3:4)), length.out = n)),
 s2 = function(n) s2::as_s2_geography(rep(c("POINT (1 2)", NA, "POINT (3 4)"), length.out = n)),
 wkt = function(n) wk::as_wkt(rep(c("POINT (1 2)", NA, "POINT (3 4)"), length.out = n))
)
results <- list(); i <- 0L
for (family in names(fixtures)) for (n in c(0L, 1L, 3L)) for (shape in c("direct", "packed")) for (role in c("dimension", "fixed", "payload")) for (label in c("null", "missing", "text")) for (sort in c("none", "first", "last")) {
 i <- i + 1L
 key <- paste(family, n, shape, role, label, sort, sep="/")
 constructor <- capture(fixtures[[family]](n))
 if (!is.null(constructor$error)) { results[[i]] <- data.frame(key, verdict="fixture_error", detail=constructor$message); next }
 value <- constructor$value
 if (shape == "packed") value <- tibble::tibble(a = value, b = seq_len(n))
 input <- tibble::tibble(k=rep(c("b",NA,"a"), length.out=n), g=rep(c("y",NA,"x"), length.out=n), row=seq_len(n), payload=seq_len(n))
 target <- switch(role, dimension="g", fixed="k", payload="payload")
 source <- capture({input[[target]] <- value; input})
 if (!is.null(source$error)) { results[[i]] <- data.frame(key, verdict="fixture_error", detail=source$message); next }
 input <- source$value
 missing <- capture(vctrs::vec_init(input$g, n))
 lab <- switch(label, null=NULL, missing=NA_character_, text="All")
 ordinary <- capture({
   a <- input; b <- input
   if (label=="text") { a$g <- as.character(a$g); b$g <- rep("All", n) } else { if (!is.null(missing$error)) stop(missing$message); b$g <- missing$value }
   a$sid <- rep(1L,n); b$sid <- rep(2L,n)
   expected <- dplyr::union_all(dplyr::relocate(a,k,g), dplyr::relocate(b,k,g))
   if(sort!="none") {
     miss <- function(x) if(is.data.frame(x)) Reduce(`&`,lapply(x,vctrs::vec_detect_missing)) else vctrs::vec_detect_missing(x)
     bit <- expected$sid == 2L
     expected <- dplyr::arrange(expected, !!!list(miss(expected$k),expected$k,if(sort=="first") dplyr::desc(bit) else bit,miss(expected$g),expected$g,expected$sid))
   }
   expected
 })
 actual <- capture(expand_with_margins(input,.by=k,.grouping=rollup(g),.margin_label=lab,.id="sid",.sort=sort))
 verdict <- if (!is.null(actual$error) && grepl("packed grouping|matrix or array|cannot be represented",actual$message)) { "expected_refusal" } else if (!is.null(ordinary$error)) {
   if (!is.null(actual$error)) "ordinary_error" else "package_succeeds"
 } else if (!is.null(actual$error)) {
   if (grepl("packed grouping|matrix or array|cannot be represented",actual$message)) "expected_refusal" else "package_error"
 } else {
   want <- ordinary$value; got <- actual$value; if(sort=="none") {want<-dplyr::arrange(want,sid,row);got<-dplyr::arrange(got,sid,row)}; got <- got[names(want)]
   if (isTRUE(all.equal(got,want,check.attributes=TRUE))) "match" else "difference"
 }
 detail <- if (verdict=="difference") paste(all.equal(got,want),collapse="; ") else if (!is.null(actual$error)) actual$message else ""
 results[[i]] <- data.frame(key,verdict,detail)
}
results <- do.call(rbind,results)
write.csv(results, file.path(output,"temporal-types.csv"), row.names=FALSE)
print(table(results$verdict)); print(unique(results[results$verdict %in% c("package_error","difference"), c("key","verdict")]),row.names=FALSE)
print(sessionInfo())
