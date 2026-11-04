suppressPackageStartupMessages({library(dplyr); library(tidyselect)})
pkgload::load_all(".", quiet = TRUE)
d <- tibble(g = c("e", "e", "w", "w"), amt01 = c(1L,4L,2L,8L), amt02 = c(0,3,1,2),
            tax01 = c(3L,4L,6L,8L), net = c(3,0,1,9), tax02 = c(1,1,4,0), fee = c(3L,1L,0L,2L))
ext <- c("net", "amt02")
missing_ext <- c("not_here", "tax02")
helpers <- c('starts_with("amt")', 'ends_with("01")', 'contains("t0")', 'matches("^(tax|fee)")',
             'num_range("amt", 1:2, width=2)', 'one_of(ext)', 'everything()', 'last_col()',
             'all_of(ext)', 'any_of(missing_ext)', 'where(is.integer)')
stopifnot(identical(names(tidyselect::vars_select_helpers), sub("\\(.*", "", helpers)))
combine <- function(a,b) c(sprintf("c(%s,%s)",a,b), sprintf("%s | %s",a,b),
                          sprintf("%s & %s",a,b), sprintf("c(%s,-%s)",a,b),
                          sprintf("%s & !%s",a,b), sprintf("%s / %s",a,b),
                          sprintf("c(-%s,%s)",a,b), sprintf("c(!%s,-%s)",a,b),
                          sprintf("!%s & !%s",a,b), sprintf("-c(%s,%s)",a,b))
pairs <- expand.grid(a=helpers,b=helpers, stringsAsFactors=FALSE)
solo <- unique(c(helpers,paste0("-",helpers),paste0("!",helpers),
                 sub("^","tidyselect::",helpers), sub("^","dplyr::",helpers),
                 c("amt01:tax02", "-(amt01:tax02)", "!(tax02:amt01)", "c(-amt01,-amt02,amt01)",
                   "c(amt01, -amt01, amt02)", "c()", "NULL", "integer()", "character()",
                   "c(-g,amt01)", "everything() & !g", "-any_of('g')", "c(alias = amt01)",
                   'c(z = all_of(ext), -net)', "1:3", "c(3L,1L,3L)",
                   "-c(1L,3L)", "c(.data$net, .env$ext)", "all_of(.env$ext)",
                   "{ if (TRUE) c('amt01', 'fee') }", "function(x) is.integer(x)")))
pair_case <- unique(unlist(Map(combine,pairs$a,pairs$b)))
capture <- function(x) {
  w <- character()
  x <- tryCatch(withCallingHandlers(x, warning=function(e) {
    w <<- c(w,conditionMessage(e)); invokeRestart("muffleWarning")
  }), error=identity)
  list(ok=!inherits(x,"error"), result=x, warnings=w)
}
parse_one <- function(x) parse(text=x, keep.source=FALSE)[[1]]
run <- function(inp, kind, sel, margin) {
  expr <- switch(kind, across=sprintf("across(%s, sum)",sel),
                 pick=sprintf("z = sum(rowSums(pick(%s)))",sel),
                 if_any=sprintf("z = sum(if_any(%s, ~ .x > 2))",sel),
                 if_all=sprintf("z = sum(if_all(%s, ~ .x > 2))",sel),
                 later=sprintf('across(%s, ~ sum(.x), .names = "out_{.col}")',sel),
                 named=sprintf("packed = across(%s, sum)",sel))
  extras <- if (kind == "later") 'amt01 = "changed", amt03 = sum(net),' else ""
  code <- if (margin) sprintf("summarize_with_margins(inp, n=n(), %s %s, .by=g)",extras,expr) else
          sprintf("summarise(group_by(inp,g), n=n(), %s %s, .groups='drop')",extras,expr)
  value <- eval(parse_one(code))
  if (!is.data.frame(value)) value <- collect(value)
  arrange(value,g)
}
report <- function(cases, inp, name, kinds) {
  out <- vector("list",length(cases)*length(kinds))
  i <- 0L
  for (kind in kinds) for (sel in cases) {
    i <- i+1L
    o <- capture(run(inp,kind,sel,FALSE))
    a <- capture(run(inp,kind,sel,TRUE))
    equal <- if (o$ok && a$ok) identical(names(o$result),names(a$result)) && isTRUE(all.equal(o$result,a$result,check.attributes=FALSE)) else o$ok == a$ok
    explain <- function(x) if (x$ok) paste(names(x$result),collapse=",") else conditionMessage(x$result)
    out[[i]] <- data.frame(backend=name,kind=kind,sel=sel,oracle_ok=o$ok,margin_ok=a$ok,equal=equal,
                           oracle=explain(o),margin=explain(a),warnings_oracle=paste(o$warnings,collapse="\n"),
                           warnings_margin=paste(a$warnings,collapse="\n"))
  }
  tab <- do.call(rbind,out)
  saveRDS(tab, file.path(out_dir, paste0("summary-matrix-", name, ".rds")))
  print(aggregate(rep(1L,nrow(tab)),tab[c("kind","oracle_ok","margin_ok","equal")],sum))
}
args <- commandArgs(trailingOnly = TRUE)
job <- match.arg(args[[1L]], c("local", "scope", "backend", "deep", "by", "grouping"))
out_dir <- if (length(args) > 1L) args[[2L]] else tempdir()
stopifnot(dir.exists(out_dir))
if (job=="local") report(c(solo,pair_case),d,"local",c("across","pick","if_any","if_all"))
if (job=="scope") report(c(solo, pair_case),d,"scope",c("later","named"))
if (job=="backend") {
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  sqlite <- copy_to(con,d,overwrite=TRUE)
  targets <- list(dtplyr=dtplyr::lazy_dt(d), sqlite=sqlite, arrow=arrow::Table$create(d))
  cases <- unique(c(solo, unlist(Map(combine,helpers,rev(helpers)))))
  for (target in names(targets)) report(cases,targets[[target]],target,c("across","pick","if_any","if_all"))
  DBI::dbDisconnect(con)
}
if (job == "deep") {
  helper_variants <- c(
    'starts_with(c("Amt", "tax"), ignore.case = FALSE)',
    'starts_with(c("Amt", "tax"), ignore.case = TRUE)',
    'ends_with(c("01", "02"))', 'contains(c("amt", "net"))',
    'matches(c("^amt", "[12]$"))', 'matches("(?<=tax)0", perl = TRUE)',
    'matches("(?<=tax)0", perl = FALSE)', 'matches("[")',
    'num_range("amt", c(2,1), width = 2)', 'num_range("amt", c(2,1))',
    'num_range("amt", integer())', 'last_col(offset=2)',
    'last_col(offset=9)', 'last_col(offset=-1)',
    'all_of(c("absent","net"))', 'all_of(c(Z="net",Y="amt01"))',
    'all_of(c(Z=2L,Y=1L))', 'all_of(c(-2L,-1L))',
    'all_of(list("amt01","net"))', 'any_of(c(NA_character_, "net"))',
    'any_of(c(Z="missing",Y="tax02"))', 'any_of(c("missing","missing"))',
    'any_of(2:4)', 'any_of(c(-2L,-1L))',
    'one_of(c("absent","net"))', 'one_of("amt01", c("tax01","fee"))',
    'where(~is.numeric(.x) && any(.x > 5))', 'where(~is.character(.x))',
    'where(~NA)', 'where(~1)', 'where(~c(TRUE, FALSE))',
    'where(function(x) { if (length(x)) is.double(x) else FALSE })',
    'c(NULL, ,amt01, ,!last_col(),-1L)', 'c(-c(1:2), !last_col(),-amt02)'
  )
  set.seed(408042L)
  make <- function() {
    a <- sample(c(helpers,helper_variants),4L,replace=TRUE)
    a <- sample(combine(a[1],a[2]),1L)
    b <- sample(c(helpers,helper_variants),2L,replace=TRUE)
    b <- sample(combine(b[1],b[2]),1L)
    sample(combine(sprintf("(%s)",a),sprintf("(%s)",b)),1L)
  }
  report(unique(c(helper_variants,replicate(250L,make()))),d,"deep",c("across","pick"))
}
if (job == "by") {
  cases <- unique(c(solo, unlist(Map(combine,helpers,rev(helpers)))))
  run_by <- function(s, margin) {
    expr <- sprintf("(%s) & !g",s)
    if (margin) {
      return(eval(parse_one(sprintf(
        "summarise_with_margins(d, total=sum(net), .by=%s, .grouping=rollup(g), .id='set', .margin_label=NULL)",expr))))
    }
    by <- names(tidyselect::eval_select(parse_one(expr),d,allow_rename=FALSE))
    expect_detail <- summarise(d,total=sum(net),.by=all_of(c(by,"g")))
    expect_total <- mutate(summarise(d,total=sum(net),.by=all_of(by)),g=NA_character_)
    bind_rows(mutate(expect_detail,set=1L,.before=total),mutate(expect_total,set=2L,.before=total))
  }
  compared <- vapply(cases,function(s) {
    a <- capture(run_by(s,FALSE)); b <- capture(run_by(s,TRUE))
    ok <- if (!a$ok || !b$ok) a$ok == b$ok else
      identical(names(a$result),names(b$result)) &&
        isTRUE(all.equal(arrange(a$result,across(everything())),arrange(b$result,across(everything())),check.attributes=FALSE))
    if (!ok) {cat(s, "\n"); print(a$result); print(b$result)}
    c(oracle=a$ok,margin=b$ok,equal=ok)
  },logical(3L))
  cat("cases =", length(cases), "\n"); print(rowSums(compared))
}

if (job == "grouping") {
  data <- mutate(d, margin_key = c("m", "n", "m", "n"))
  cases <- unique(c(solo, unlist(Map(combine, helpers, rev(helpers))),
                    "margin_key", "everything() & !margin_key",
                    "c(-all_of('margin_key'), starts_with('amt'))"))
  expressions <- list(
    across = "across(%s, sum)",
    pick = "out = sum(rowSums(pick(%s)))",
    if_any = "out = sum(if_any(%s, ~ .x > 2))",
    if_all = "out = sum(if_all(%s, ~ .x > 2))"
  )
  direct <- function(expr) {
    detail <- eval(parse_one(sprintf(
      "summarise(group_by(data, g, margin_key), n = n(), %s, .groups = 'drop')",
      expr
    )))
    total <- eval(parse_one(sprintf(
      "summarise(group_by(select(data, -margin_key), g), n = n(), %s, .groups = 'drop')",
      expr
    )))
    bind_rows(
      mutate(detail, sid = 1L, .before = n),
      mutate(total, margin_key = NA_character_, sid = 2L, .before = n)
    )
  }
  margin <- function(expr) {
    code <- sprintf(paste0(
      "summarize_with_margins(data, n = n(), %s, .by = g, ",
      ".grouping = rollup(margin_key), .id = 'sid', .margin_label = NULL)"
    ), expr)
    eval(parse_one(code))
  }
  counts <- c(equal = 0L, both_rejected = 0L, mismatch = 0L)
  for (selector in cases) {
    for (consumer in names(expressions)) {
      expr <- sprintf(expressions[[consumer]], selector)
      expected <- capture(direct(expr))
      observed <- capture(margin(expr))
      equal <- if (expected$ok && observed$ok) {
        a <- arrange(expected$result, g, sid, margin_key)
        b <- arrange(observed$result, g, sid, margin_key)
        identical(names(a), names(b)) &&
          isTRUE(all.equal(a, b, check.attributes = FALSE))
      } else {
        FALSE
      }
      status <- if (equal) "equal" else if (!expected$ok && !observed$ok) {
        "both_rejected"
      } else {
        "mismatch"
      }
      counts[[status]] <- counts[[status]] + 1L
      if (status == "mismatch") {
        print(list(consumer = consumer, selector = selector,
                   expected = expected$result, observed = observed$result))
      }
    }
  }
  print(counts)
  stopifnot(counts[["mismatch"]] == 0L)
}
