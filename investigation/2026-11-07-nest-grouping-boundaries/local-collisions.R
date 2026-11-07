pkgload::load_all(quiet=TRUE)
codes <- c(NA_integer_,2L,1L,3L)
h <- structure(codes, levels=c("declared",NA_character_,"used"),
               class=c("ordered","factor"))
data <- tibble::tibble(g=c("Total","a","Total",NA_character_), h=h, i=1:4)
labels <- list(
  both_clear = list(g="All",h="SUB"),
  observed = list(g="Total",h="SUB"),
  used_declared = list(g="All",h="declared"),
  unused_declared = list(g="All",h="unused"),
  missing_mix = list(g=NA_character_,h="SUB"),
  null_mix = list(g="All",h=NULL),
  null_observed = list(g="Total",h=NULL),
  declared_na = list(g=NULL,h=NA_character_)
)
level_variants <- c("declared", NA_character_, "used", "unused")
attr(data$h, "levels") <- level_variants
for (nm in names(labels)) for (size in c(0L,4L)) for (check in c(FALSE,TRUE)) {
  fixture <- data[seq_len(size),,drop=FALSE]
  original <- serialize(fixture,NULL)
  for (position in c("first","last")) for (sort in c("none","first","last")) for (rowwise in c(FALSE,TRUE)) {
    v <- if (rowwise) nest_by_with_margins else nest_with_margins
    call <- function(label) tryCatch(v(fixture,.grouping=rollup(g,h),.id="set",.keep=TRUE,
      .margin_label=label,.check_margin_label=check,.margin_label_position=position,
      .sort=sort),error=identity)
    b <- labels[[nm]]
    expected <- if (nm %in% c("used_declared","unused_declared")) "factor level"
      else if (nm == "declared_na") "already a factor level"
      else if (nm %in% c("observed","null_observed") && check && size > 0L) "already" else "ok"
    a <- call(b)
    reorder <- call(rev(b))
    outcome <- "ok"
    if (!identical(original,serialize(fixture,NULL))) outcome <- "mutated input"
    else if (inherits(a,"error")) {
      if (expected == "ok" || !inherits(a,"marginplyr_error") ||
          !grepl(expected,conditionMessage(a),fixed=TRUE) ||
          !inherits(reorder,"marginplyr_error") ||
          !identical(conditionMessage(a),conditionMessage(reorder))) outcome <- paste("error", conditionMessage(a))
      else outcome <- "expected refusal"
    } else if (expected != "ok") outcome <- "missing refusal"
    else if (!identical(a,reorder)) outcome <- "label order changed result"
    else {
      expect_levels <- if (is.null(b$h)) level_variants
        else if (position == "first") c("SUB",level_variants) else c(level_variants,"SUB")
      if (!identical(levels(a$h),expect_levels)) outcome <- "wrong levels"
      if (!identical(as.integer(a$h[a$set==1L]),vapply(a$data[a$set==1L],function(cell) {
         code <- as.integer(cell$h)[1L]
         if (!is.null(b$h) && position == "first") code + 1L else code
      },integer(1)))) outcome <- "wrong detail codes"
      if (!all(vapply(a$data,function(cell) identical(cell$h, fixture$h[cell$i]), logical(1)))) {
         outcome <- "wrong kept values"
      }
      if (size > 0L) {
        total <- a[a$set==3L,]
        if (nrow(total) != 1L || !identical(total$data[[1L]]$i, 1:4)) outcome <- "wrong Grand total"
      }
    }
    cat(nm,size,check,position,sort,rowwise,gsub("\n"," ",outcome),sep="\t"); cat("\n")
  }
}
