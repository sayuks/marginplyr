set.seed(29102911)
make_factor <- function(n) {
  x <- factor(rep(c("b", "a", NA_character_, "..marginplyr_missing_factor_code", "b", "a"), length.out = n),
    levels = c("b", NA_character_, "..marginplyr_missing_factor_code", "a", "unused"), exclude = NULL)
  if (n > 2L) x[2L] <- NA_integer_
  x
}
make_value <- function(kind, n) {
  ix <- rep(seq_len(6), length.out=n)
  value <- switch(kind,
    integer=c(NA_integer_, 1L, 2L, 1L, 3L, NA_integer_)[ix],
    logical=c(NA, TRUE, FALSE, TRUE, FALSE, NA)[ix],
    double=c(NA_real_, NaN, 1, 1+1e-14, Inf, -Inf)[ix],
    signed_zero=c(0, -0, 0, -0, NA_real_, NaN)[ix],
    character=c(NA_character_, "", "b", "a", "Total", "b")[ix],
    factor=make_factor(n),
    ordered=ordered(c(NA, "a", "b", "a", "b", NA)[ix], levels=c("b","a","unused")),
    complex=c(NA_complex_, 1i, 1+2i, NaN+0i, 1i, 1+2i)[ix],
    date=as.Date(c(NA_real_,0,1,0,2,1)[ix],origin="1970-01-01"),
    timestamp=as.POSIXct(c(NA_real_,0,1,0,2,1)[ix],origin="1970-01-01",tz="Asia/Kathmandu"),
    posixlt=as.POSIXlt(as.POSIXct(c(NA_real_,0,1,0,2,1)[ix],origin="1970-01-01",tz="America/New_York")),
    duration=as.difftime(c(NA_real_,0,1,0,2,1)[ix],units="hours"),
    list=list(NULL,NA,1L,list(1L),quote(a+b),1L)[ix],
    named_list=list(list(a=1L),list(b=1L),list(a=1L),list(a=NULL),NULL,list(a=NA))[ix],
    expression_list=list(quote(a+b),quote(a+b),quote(b+a),expression(a),NULL,quote(NULL))[ix],
    matrix=matrix(c(NA_real_,1,2,1,3,NA)[rep(ix,2)],nrow=n,ncol=2),
    matrix0=matrix(integer(),nrow=n,ncol=0),
    array=array(c(NA_real_,1,2,1,3,NA)[rep(ix,4)],dim=c(n,2,2)),
    packed=tibble::tibble(x=c(NA_real_,NaN,1,NA,1,1)[ix], f=make_factor(n)),
    packed0=tibble::new_tibble(list(),nrow=n),
    nested=tibble::tibble(x=tibble::tibble(f=make_factor(n),y=ix),
      z=matrix(rep(ix,2),nrow=n,ncol=2)),
    integer64=bit64::as.integer64(c(NA_character_,"9007199254740993","9007199254740994","9007199254740993","0","-9007199254740993")[ix]),
    hms=hms::hms(c(NA_real_,0,1,0,2,1)[ix]),
    units=units::set_units(c(NA_real_,0,1,0,2,1)[ix],"m"),
    record=vctrs::new_rcrd(list(x=c(NA_real_,0,1,0,2,1)[ix], y=c("a","a","b","a","c",NA_character_)[ix]),class="hunt_record"),
    AsIs=I(c(NA_real_,NaN,1,2,1,NA)[ix]),
    list_of=vctrs::list_of(1L,2L,NA_integer_,1L,2L,integer())[ix])
  value
}
format.hunt_record <- function(x, ...) paste(vctrs::field(x,"x"),vctrs::field(x,"y"),sep=":")
vec_ptype_abbr.hunt_record <- function(x,...) "rec"
kinds <- c("integer","logical","double","signed_zero","character","factor","ordered","complex","date","timestamp","posixlt","duration","list","named_list","expression_list","matrix","matrix0","array","packed","packed0","nested","integer64","hms","units","record","AsIs","list_of")
plans <- list(list(c("a","b"),"a",character()),list("b",character(),"a"),list(c("a","b")),list(character()))
expr_sets <- function(sets) rlang::call2("grouping_sets",!!!lapply(sets,function(s)rlang::call2("grouping_set",!!!rlang::syms(s))))
