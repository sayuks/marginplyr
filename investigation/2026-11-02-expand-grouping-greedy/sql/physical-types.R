# Dated SQL physical-type differential; no package implementation changes.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cases <- list(
  enum = "'z'::ENUM('z','a')", uuid = "'123e4567-e89b-12d3-a456-426614174000'::UUID",
  bit = "'10101'::BIT", bigint = "9223372036854775806::BIGINT", ubigint = "18446744073709551614::UBIGINT",
  hugeint = "170141183460469231731687303715884105726::HUGEINT", decimal = "12.345::DECIMAL(12,3)",
  time = "TIME '12:34:56'", timetz = "TIMETZ '12:34:56+05:30'", timestamp_ns = "'2026-01-01 12:34:56.123456789'::TIMESTAMP_NS",
  timestamptz = "TIMESTAMPTZ '2026-01-01 12:34:56+05:30'", interval = "INTERVAL '2 months 3 days 4 seconds'",
  blob = "'abc'::BLOB", json = "'{\"x\":1}'::JSON", list = "[1,NULL,3]", array = "[1,NULL,3]::INTEGER[3]",
  struct = "{'x': 1, 'y': 'a'}", map = "MAP {'x': 1, 'y': 2}", union = "union_value(num := 42)",
  all_null_character = "NULL::VARCHAR"
)
rows <- list()
observe <- function(expr) tryCatch(list(ok=TRUE,value=force(expr)), error=function(e) list(ok=FALSE,error=conditionMessage(e),class=class(e)[1]))
for (nm in names(cases)) {
  setup <- observe(DBI::dbExecute(con, paste0('CREATE OR REPLACE TABLE src AS SELECT ',cases[[nm]],' AS k, 7::INTEGER AS rid')))
  if (!setup$ok) { rows[[length(rows)+1]] <- data.frame(type=nm,role='setup',sort='',label='',status='source_error',detail=setup$error); next }
  source <- observe(tbl(con,'src'))
  if (!source$ok) { rows[[length(rows)+1]] <- data.frame(type=nm,role='source',sort='',label='',status='source_error',detail=source$error); next }
  src <- source$value
  typ <- DBI::dbGetQuery(con, 'SELECT typeof(k) AS t FROM src')$t[[1]]
  for (role in c('dimension','fixed','payload')) for (sort in c('none','first','last')) for (label in c('null','text')) {
    input <- if(role=='dimension') src else mutate(src,g='a')
    expected_sql <- if(role=='dimension') paste0('SELECT k,rid,1::INTEGER AS sid FROM src UNION ALL SELECT ',if(label=='null') paste0('NULL::',typ) else "'Total'::VARCHAR",',rid,2::INTEGER AS sid FROM src') else 'SELECT k,rid,\'a\' AS g,1::INTEGER AS sid FROM src UNION ALL SELECT k,rid,\'Total\' AS g,2::INTEGER AS sid FROM src'
    if(role!='dimension' && label=='null') expected_sql <- sub("'Total' AS g", 'NULL::VARCHAR AS g', expected_sql, fixed=TRUE)
    if(role=='dimension' && label=='text') expected_sql <- sub('SELECT k,rid', 'SELECT CAST(k AS VARCHAR) AS k,rid', expected_sql, fixed=TRUE)
    expected <- observe(DBI::dbGetQuery(con,expected_sql))
    if(nm=='enum' && role=='dimension' && expected$ok) expected$value$k <- factor(expected$value$k, levels=c('z','a',if(label=='text') 'Total'))
    result <- observe(if(role=='dimension') expand_with_margins(input,.grouping=rollup(k),.margin_label=if(label=='null') NULL else 'Total',.id='sid',.sort=sort) else if(role=='fixed') expand_with_margins(input,.by=k,.grouping=rollup(g),.margin_label=if(label=='null') NULL else 'Total',.id='sid',.sort=sort) else expand_with_margins(input,.grouping=rollup(g),.margin_label=if(label=='null') NULL else 'Total',.id='sid',.sort=sort))
    if(result$ok) result <- observe(collect(result$value))
    # Compare per occurrence; value ordering is immaterial for one input row.
    eq <- if(expected$ok && result$ok) isTRUE(all.equal(as.data.frame(result$value[order(result$value$sid),names(expected$value)]),as.data.frame(expected$value),check.attributes=TRUE)) else FALSE
    status <- if(!expected$ok) 'control_error' else if(!result$ok) 'package_error' else if(eq) 'match' else 'difference'
    detail <- if(!result$ok) result$error else if(!expected$ok) expected$error else if(!eq) paste(all.equal(result$value[order(result$value$sid),names(expected$value)],tibble::as_tibble(expected$value)),collapse='; ') else ''
    rows[[length(rows)+1]] <- data.frame(type=nm,role=role,sort=sort,label=label,status=status,detail=detail)
  }
}
out <- do.call(rbind,rows)
write.csv(out,'investigation/2026-11-02-expand-grouping-greedy/sql/physical-types.csv',row.names=FALSE)
print(with(out,table(status)))
print(unique(out[out$status!='match',c('type','role','status','detail')]),row.names=FALSE)
DBI::dbDisconnect(con,shutdown=TRUE)
