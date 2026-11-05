con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
values <- c(varchar="'2'::VARCHAR", logical="TRUE", interval="INTERVAL 2 DAY", list="[1,2]", bit="'10'::BIT", uuid="'80000000-0000-0000-0000-000000000000'::UUID", integer="2::INTEGER", decimal="2::DECIMAL(38,0)")
for(nm in names(values)) {
 q <- sprintf('SELECT typeof(%s) AS input, typeof((%s) * 1 - 0) AS product',values[[nm]],values[[nm]])
 result <- tryCatch(DBI::dbGetQuery(con,q),error=function(e) class(e)[1])
 cat(nm, if(is.data.frame(result)) paste(result$input,result$product) else result, '\n')
}
DBI::dbDisconnect(con)
