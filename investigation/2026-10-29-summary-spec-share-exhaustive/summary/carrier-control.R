# This control uses only dplyr/vctrs and excludes private grouping keys.
data <- data.frame(g=c('a','b','b'),x=1:3)
value <- function(x) if(length(x)==1L) NA else data.frame(z=sum(x))
carrier <- function(value) {
  if(is.data.frame(value)) return(value)
  vctrs::new_data_frame(list('value(x)'=value),n=vctrs::vec_size(value))
}
original <- dplyr::summarise(data,value(x),.by=g)
wrapped <- dplyr::summarise(data,carrier(value(x)),.by=g)
ptype <- vctrs::vec_ptype2(NA,data.frame(z=integer()))
stopifnot(identical(names(original),c('g','z')))
stopifnot(identical(names(wrapped),c('g','value(x)','z')))
stopifnot(identical(names(ptype),'z'))
print(original)
print(wrapped)
cat('Common prototype names:',names(ptype),'\n')
