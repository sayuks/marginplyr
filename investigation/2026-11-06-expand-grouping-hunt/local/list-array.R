# Dated local typed-missing list-array investigation; no implementation changes.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(trailingOnly = TRUE)
observe <- !"--assert" %in% args
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(list(value = force(expr)), error = function(e) list(error = class(e)[[1L]], message = conditionMessage(e)))

# One row and one matrix cell suffice; payload is only a witness for copying.
key <- matrix(list(7L), nrow = 1L)
input <- tibble::tibble(g = key, payload = 19L)
prototype <- vctrs::vec_init(key, 1L)
stopifnot(identical(prototype, matrix(list(NULL), nrow = 1L)),
          identical(vctrs::vec_detect_missing(prototype), TRUE),
          identical(unname(is.na(prototype)), matrix(FALSE, nrow = 1L)))
control <- dplyr::union_all(input, dplyr::mutate(input, g = prototype))
stopifnot(nrow(control) == 2L, identical(dim(control$g), c(2L, 1L)),
          identical(control$g[[1L]], 7L), identical(control$g[[2L]], NULL),
          identical(control$payload, c(19L, 19L)))
actual <- capture(expand_with_margins(input, .grouping = rollup(g), .margin_label = NULL))
cat("Minimal ordinary control: PASS; rows=2, list-matrix shape=2x1, cells=7L/NULL\n")
if (!is.null(actual$error)) cat("Minimal expansion:", actual$error, actual$message, "\n")
if (!observe) {
  if (!is.null(actual$error)) stop("Expected list-matrix expansion to equal the successful ordinary control; got ",actual$error)
  stopifnot(identical(actual$value, control))
  quit(status = 0L)
}

make_key <- function(n, width, rank, classed) {
  shape <- if (rank == 2L) c(n, width) else c(n, width, 2L)
  cells <- rep(list(2L, NULL, NA_integer_, 1L), length.out = prod(shape))
  if (classed) cells <- vctrs::list_of(!!!cells, .ptype = integer())
  dim(cells) <- shape
  cells
}
missing_rows <- function(x) {
  if (is.data.frame(x)) {
    if (ncol(x) == 0L) return(rep(TRUE, nrow(x)))
    Reduce(`&`, lapply(x, missing_rows))
  } else vctrs::vec_detect_missing(x)
}
results <- list(); i <- 0L
for (n in c(0L,1L,3L)) for (width in c(0L,1L,2L)) for (rank in c(2L,3L)) for (classed in c(FALSE,TRUE)) for (shape in c("direct","packed","nested")) for (role in c("dimension","fixed","payload")) for (label in c("null","missing")) for (sort in c("none","first","last")) {
  i <- i + 1L
  id <- paste(n,width,rank,classed,shape,role,label,sort,sep="/")
  fixture <- capture({
    g <- make_key(n,width,rank,classed)
    if (shape=="packed") g <- tibble::tibble(a=g,b=seq_len(n))
    if (shape=="nested") g <- tibble::tibble(inner=tibble::tibble(a=g),b=seq_len(n))
    d <- tibble::tibble(k=rep(c("b",NA,"a"),length.out=n),g=seq_len(n),h=rep(c("q",NA,"p"),length.out=n),row=seq_len(n),payload=seq_len(n))
    target <- switch(role,dimension="g",fixed="k",payload="payload")
    d[[target]] <- g
    d
  })
  if (!is.null(fixture$error)) { results[[i]]<-data.frame(id,status="fixture_error",detail=fixture$message); next }
  d <- fixture$value
  dimensions <- if(role=="dimension") c("g","h") else "h"
  sets <- if(role=="dimension") list(c("g","h"),"g",character()) else list("h",character())
  spec <- if(role=="dimension") rollup(g,h) else rollup(h)
  before <- serialize(d,NULL)
  ordinary <- capture({
    branches <- lapply(seq_along(sets),function(sid) {
      b <- d
      for(col in setdiff(dimensions,sets[[sid]])) b[[col]] <- vctrs::vec_init(d[[col]],n)
      b$sid <- rep.int(sid,n)
      b
    })
    expected <- Reduce(dplyr::union_all,branches)
    expected <- dplyr::relocate(expected, dplyr::all_of(c("k",dimensions,"sid")))
    if(sort!="none") {
      keys <- list(missing_rows(expected$k),expected$k)
      for(col in dimensions) {
        bit <- vapply(expected$sid,function(sid)!col %in% sets[[sid]],logical(1))
        keys <- c(keys,list(if(sort=="first") dplyr::desc(bit) else bit,missing_rows(expected[[col]]),expected[[col]]))
      }
      expected <- dplyr::arrange(expected, !!!c(keys,list(expected$sid)))
    }
    expected
  })
  actual <- capture(expand_with_margins(d,.by=k,.grouping=spec,.margin_label=if(label=="null") NULL else NA_character_,.id="sid",.sort=sort))
  stopifnot(identical(before,serialize(d,NULL)))
  status <- if(!is.null(ordinary$error)) "ordinary_error" else if(!is.null(actual$error)) "package_error" else if(isTRUE(all.equal(actual$value,ordinary$value,check.attributes=TRUE))) "match" else "difference"
  detail <- if(!is.null(actual$error)) actual$message else if(status=="difference") paste(all.equal(actual$value,ordinary$value),collapse="; ") else ""
  results[[i]] <- data.frame(id,status,detail)
}
results<-do.call(rbind,results)
write.csv(results,file.path(output,"list-array.csv"),row.names=FALSE)
print(table(results$status))
print(unique(results[results$status=="package_error","detail"]))
print(results[results$status=="difference",c("id","detail")],row.names=FALSE)
print(sessionInfo())
