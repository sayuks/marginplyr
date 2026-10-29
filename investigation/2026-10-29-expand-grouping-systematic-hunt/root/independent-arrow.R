pkgload::load_all(quiet=TRUE)
root <- tempfile('directory-')
dir.create(file.path(root,'7'),recursive=TRUE)
arrow::write_parquet(data.frame(g=9L,rid=42L),file.path(root,'7','facts.parquet'))
src <- arrow::open_dataset(root,partitioning=arrow::DirectoryPartitioning$create(arrow::schema(sid=arrow::int32())),schema=arrow::schema(g=arrow::int32(),rid=arrow::int32()))
actual <- dplyr::collect(expand_with_margins(src,.grouping=rollup(g),.margin_label=NULL,.id='sid'))
expected <- tibble::tibble(g=c(9L,NA_integer_),sid=1:2,rid=c(42L,42L))
control <- dplyr::collect(dplyr::union_all(dplyr::mutate(src,sid=1L),dplyr::mutate(src,g=NA_integer_,sid=2L)))
canon <- function(z) {z <- z[c('g','sid','rid')]; z[order(z$sid),]}
expected <- canon(expected);control <- canon(control); actual <- actual[order(is.na(actual$g)),];rownames(actual) <- NULL
print(actual);print(control);stopifnot(identical(control,expected))
cat('independent_directory_identifier_match:',identical(actual,expected),'\n')
unlink(root,recursive=TRUE)
if('--assert' %in% commandArgs(TRUE)) stopifnot(identical(actual,expected))
