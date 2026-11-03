pkgload::load_all(quiet=TRUE)
library(dplyr)
data <- data.frame(x=1:3,y=5:7)
forms <- c('{.col}_{seq_along(.col)}', '{.col}_{length(.col)}', '{.col}_{rev(.col)}', '{.col}_{match(.col, .col)}')
for(s in forms) {
 cat("\nTEMPLATE:",s,"\n")
 print(summarise(data,across(c(x,y),sum,.names=s)))
 print(tryCatch(summarize_with_margins(data,across(c(x,y),sum,.names=s),p=share_of_total(y_2)),error=function(cnd) conditionMessage(cnd)))
 print(tryCatch(summarize_with_margins(dtplyr::lazy_dt(data),across(c(x,y),sum,.names=s)) %>% collect(),error=function(cnd) conditionMessage(cnd)))
}
