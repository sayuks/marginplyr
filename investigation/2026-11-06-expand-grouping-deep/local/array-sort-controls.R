Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
factories <- list(
  date = function(n) as.Date(rep(c(1,NA,2),length.out=n),origin="1970-01-01"),
  datetime = function(n) as.POSIXct(rep(c(1,NA,2),length.out=n),origin="1970-01-01",tz="UTC"),
  factor = function(n) factor(rep(c("a",NA,"b"),length.out=n)),
  ordered = function(n) ordered(rep(c("a",NA,"b"),length.out=n))
)
records <- list(); index <- 0L
for (family in names(factories)) for(width in 1:2) for(packed in c(FALSE,TRUE)) {
  g <- factories[[family]](3L*width); dim(g) <- c(3L,width)
  if(packed) g <- tibble(inner=g)
  x <- tibble(g=g,rid=1:3)
  ordinary <- union_all(x,mutate(x,g=!!vctrs::vec_init(g,3L)))
  unsorted <- expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL)
  stopifnot(identical(ordinary,unsorted))
  a <- tryCatch(arrange(ordinary,g),error=identity)
  b <- tryCatch(expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL,.sort="last"),error=identity)
  stopifnot(inherits(a,"expressionStackOverflowError"), inherits(b,"expressionStackOverflowError"))
  index <- index + 1L
  records[[index]] <- data.frame(family,width,packed,ordinary=class(a)[1L],package=class(b)[1L])
}
print(bind_rows(records)); cat("Sixteen ordinary-composition matches and sixteen shared value-ordering errors confirmed.\n")
