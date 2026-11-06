Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con))
  exprs <- c(json_number = "'2'::JSON", json_string = "'\"2\"'::JSON", uuid="'00000000-0000-0000-0000-000000000002'::UUID",
    enum = "'2'::ENUM ('1', '2')", bit = "'10'::BIT", boolean="TRUE", date="DATE '2020-01-01'",
    timestamp="TIMESTAMP '2020-01-01'", interval="INTERVAL '1 day'", blob="'2'::BLOB",
    numeric="2::DECIMAL(38,0)")
  for (kind in names(exprs)) {
    x <- tryCatch(tbl(con, dbplyr::sql(paste0("SELECT 'a' AS g, ",exprs[[kind]]," AS v"))),error=identity)
    if(inherits(x,"error")) { cat(kind,"SETUP ERROR",conditionMessage(x),"\n"); next }
    ordinary <- tryCatch(collect(summarize_with_margins(x, value=max(v))),error=identity)
    cat(kind,"ORDINARY",if(inherits(ordinary,"error")) paste("ERROR",conditionMessage(ordinary)) else paste("SUCCESS",paste(class(ordinary$value),collapse="/")),"\n")
    for (grouped in c(FALSE, TRUE)) {
      value <- tryCatch(collect(summarize_with_margins(x, value=max(v), total=share_of_total(value),
        .grouping=if(grouped) rollup(g) else NULL)),error=identity)
      cat(kind, grouped, if(inherits(value,"error")) paste("ERROR",conditionMessage(value)) else paste("SUCCESS",paste(class(value$value),collapse="/"),paste(value$total,collapse=",")),"\n")
    }
  }
}
main()
