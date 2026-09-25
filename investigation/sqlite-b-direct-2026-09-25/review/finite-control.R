library(dplyr)
c<-DBI::dbConnect(RSQLite::SQLite(),':memory:'); x<-copy_to(c,tibble(g=1:2),'s'); print(tryCatch(collect(x,n=1.9),error=function(e)conditionMessage(e)));DBI::dbDisconnect(c)
