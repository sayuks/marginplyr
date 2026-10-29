# Upstream control: ordinary dtplyr/data.table selection changes `.BY` to `BY`.
pkgload::load_all('.', quiet=TRUE)
source <- data.table::data.table(special=c('b','a','b'),value=1:3,discard=TRUE)
data.table::setnames(source,'special','.BY')
before <- data.table::copy(source)
root <- dtplyr::lazy_dt(source)
step <- dplyr::select(root,dplyr::all_of(c('.BY','value')))
materialized <- dplyr::collect(step)
cat('Ordinary dtplyr query:\n')
dplyr::show_query(step)
cat('Declared dtplyr columns:',paste(dplyr::tbl_vars(step),collapse=', '),'\n')
cat('Collected columns:',paste(names(materialized),collapse=', '),'\n')
# No Margin operation was needed for the discrepancy above.
stopifnot(identical(names(materialized),c('BY','value')),
 identical(as.character(dplyr::tbl_vars(step)),c('.BY','value')))
raw <- source[, .(.BY,value)]
cat('Direct data.table selected columns:',paste(names(raw),collapse=', '),'\n')
stopifnot(identical(names(raw),c('BY','value')))
cat('Inspection of derived step:\n')
print(inspect_grouping(step,.grouping=grouping_set(where(is.character))))
cat('Inspection after explicit materialization:\n')
print(inspect_grouping(materialized,.grouping=grouping_set(where(is.character))))
stopifnot(identical(source,before))
cat('Source unchanged; discrepancy exists before marginplyr.\n')
