suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.', quiet = TRUE)
fixture <- tibble(p = c('x', 'x', 'y', 'y', NA), g = c('a', NA, 'b', 'a', 'b'), h = c('j','k','j','j','k'), v = c(2,-1,3,4,2))
weird <- c('NA', 'NULL', 'Inf', 'NaN', '*', '"', '`', '\n', ' ', 'g\rkey', 'a.b', 'a:b', '...','..1', '.N', '.SD', 'T', 'F', 'n()', '..marginplyr_share_partition_1', '..marginplyr_parent_key_1', 'é', 'e\u0301')
call <- function(src, ns, sort) summarize_with_margins(src,
  !!ns[5] := sum(.data[[ns[4]]], na.rm = TRUE),
  !!ns[6] := share_of_parent(!!sym(ns[5])),
  !!ns[7] := share_of_total(!!sym(ns[5])),
  .by = all_of(ns[1]), .grouping = rollup(all_of(ns[2]), all_of(ns[3])),
  .margin_label = NULL, .id = ns[8], .sort = sort, .check_share_source = FALSE)
canon <- function(x) { x <- as.data.frame(x); names(x) <- paste0('c',seq_along(x)); x[] <- lapply(x, function(v) if (is.numeric(v)) as.double(v) else v); x <- x[do.call(order,x[c(1,2,3,7)]),]; rownames(x) <- NULL; x }
results <- list()
for (backend in c('sqlite','duckdb')) {
 con <- if (backend == 'sqlite') DBI::dbConnect(RSQLite::SQLite()) else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
 for (pos in c(1L,2L,3L,4L,5L,6L,7L,8L)) for (nm in weird) {
  ns <- c('p','g','h','v','value','parent','total','set'); ns[pos] <- nm
  input <- setNames(fixture,ns[1:4]); src <- copy_to(con,input,'source',temporary=TRUE,overwrite=TRUE)
  for (sort in c('none','last')) {
   want <- tryCatch(call(input,ns,sort),error=identity)
   got <- tryCatch(collect(call(src,ns,sort)),error=identity)
   verdict <- if(inherits(want,'error')) 'local-error' else if(inherits(got,'error')) 'backend-error' else if (isTRUE(all.equal(canon(want),canon(got)))) 'match' else 'mismatch'
   if (verdict != 'match') cat(backend,pos,encodeString(nm),sort,verdict,if(inherits(got,'error')) gsub('\n',' ',conditionMessage(got)) else '', '\n')
   results[[length(results)+1L]] <- data.frame(backend,pos,name=nm,sort,verdict)
  }
 }
 DBI::dbDisconnect(con)
}
results <- bind_rows(results)
write.csv(results,'investigation/2026-11-05-summary-grouping-share-greedy/backend/name-results.csv',row.names=FALSE)
print(count(results,backend,verdict))
