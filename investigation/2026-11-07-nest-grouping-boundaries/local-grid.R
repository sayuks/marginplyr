pkgload::load_all(quiet = TRUE)
tib <- tibble::tibble
factor_na <- structure(c(NA_integer_, 2L, 1L, 2L, 3L, NA_integer_),
  levels = c("a", NA_character_, "z"), class = c("ordered", "factor"))
factor_plain <- factor(c("x",NA,"a","x","z","a"),levels=c("a","x","z","unseen"))
number <- c(NA_real_, NaN, 0, -0, Inf, -Inf)
dim_sources <- list(
  string = c("Total", "", NA_character_, "a", "Total", "a"),
  ordered_na = factor_na, factor = factor_plain, numeric = number,
  date = as.Date("2020-03-01") + c(0,NA,-1,5,0,3),
  raw = as.raw(c(0,255,12,0,12,4)),
  list = list(NULL,NA_integer_,numeric(),NULL,as.raw(0),NA_integer_),
  matrix = matrix(c(number, 7:12), 6L),
  packed = tib(z = factor_na, a = c("",NA,"a","","z","a")),
  zero_width = tib(.rows = 6)
)
plans <- list(
  single = list(spec = grouping_set(g,h), sets = list(c("g","h"))),
  omitted = list(spec = grouping_sets(grouping_set(g,h), grouping_set(h)),
                 sets = list(c("g","h"), "h")),
  rollup = list(spec = rollup(g,h), sets = list(c("g","h"), "g", character())),
  cube = list(spec = cube(g,h), sets = list(c("g","h"), "g", "h", character())),
  duplicates = list(
    spec = grouping_sets(grouping_set(g,h), rollup(g,h), cube(g,h)),
    sets = list(c("g","h"), "g", character(), "h"), duplicates = "drop"
  )
)
encode_factor_display <- function(source, label, position, omitted, n) {
  levels <- levels(source)
  codes <- as.integer(source)
  if (!is.null(label)) {
    if (identical(position, "first")) {
      levels <- c(label, levels)
      codes <- codes + 1L
      codes[omitted] <- 1L
    } else {
      levels <- c(levels, label)
      codes[omitted] <- length(levels)
    }
  } else {
    codes[omitted] <- NA_integer_
  }
  structure(codes, levels = levels, class = class(source))
}
same_values <- function(x,y) {
  if (is.data.frame(x) && is.data.frame(y)) {
    return(nrow(x) == nrow(y) && identical(names(x), names(y)) &&
      all(mapply(same_values, as.list(x), as.list(y))))
  }
  identical(x, y, num.eq = FALSE, single.NA = FALSE)
}
oracle <- function(source, fixed, sets, keep, label, position, sort, rowwise) {
  keys <- c(fixed,"g","h")
  payload <- if (keep) c(keys, setdiff(names(source), keys)) else setdiff(names(source), keys)
  out <- list()
  for (j in seq_along(sets)) {
    k <- c(fixed,sets[[j]])
    loc <- if (!nrow(source)) list() else if (!length(k)) list(seq_len(nrow(source))) else {
      vctrs::vec_group_loc(source[k])$loc
    }
    for (i in loc) {
      elem <- list(rows = i, set = j, omitted = !c("g","h") %in% sets[[j]])
      out <- c(out,list(elem))
    }
  }
  list(rows = out, payload = payload)
}
check <- function(result, source, fixed, sets, keep, label, position, sort, rowwise, key) {
  expect <- oracle(source, fixed, sets, keep, label, position, sort, rowwise)
  exp <- expect$rows
  key_names <- c(fixed,"g","h")
  if (!identical(names(result), c(key_names,"set",key)) ||
      nrow(result) != length(exp) ||
      !identical(dplyr::group_vars(result), if (rowwise) c(key_names,"set") else character())) {
    return(paste("shape", nrow(result), length(exp), paste(names(result),collapse=":")))
  }
  if (!length(exp)) return("ok")
  cells <- result[[key]]
  index <- vapply(seq_along(cells), function(j) {
    hit <- which(vapply(exp,function(e) {
      identical(result$set[[j]], e$set) && identical(cells[[j]]$.source_id, e$rows)
    }, logical(1)))
    if (length(hit) == 1L) hit else NA_integer_
  },integer(1))
  if (!identical(sort(index), seq_along(exp))) return("membership")
  for (j in seq_along(cells)) {
    expected <- vctrs::vec_slice(source[expect$payload], exp[[index[[j]]]]$rows)
    if (!same_values(cells[[j]],expected)) return(paste("cell",j))
  }
  expected_rows <- exp[index]
  for (col in key_names) {
    omitted <- vapply(expected_rows, function(x) col %in% c("g","h")[x$omitted], logical(1))
    representative <- vapply(expected_rows, function(x) x$rows[[1L]],integer(1))
    src <- source[[col]]
    ord <- vctrs::vec_slice(src, representative)
    if (is.factor(src)) {
      labelled <- !is.null(label) && col %in% c("g","h")
      if (!labelled) {
        ord <- encode_factor_display(ord, NULL, position, omitted, length(ord))
      } else {
        ord <- encode_factor_display(ord, label, position, omitted, length(ord))
        if (position == "first") {
          codes <- as.integer(ord)
          codes[!omitted] <- as.integer(src)[representative[!omitted]] + 1L
          ord <- structure(codes,levels=levels(ord),class=class(ord))
        }
      }
    } else if (is.null(label) || !col %in% c("g","h")) {
      ord <- vctrs::vec_assign(ord, which(omitted), vctrs::vec_init(src, sum(omitted)))
    } else {
      ord <- as.character(ord)
      ord[omitted] <- label
    }
    if (!same_values(result[[col]],ord)) return(paste("outer key",col))
  }
  if (sort != "none") {
    expected_idx <- seq_len(nrow(result))
    order_frame <- tib(.index = expected_idx)
    for (col in key_names) {
      if (col %in% c("g","h")) {
        omit <- vapply(expected_rows, function(x) col %in% c("g","h")[x$omitted],logical(1))
        order_frame[[paste0(col,"_omit")]] <- if (sort=="last") omit else !omit
      }
      order_frame[[paste0(col,"_missing")]] <- vctrs::vec_detect_missing(result[[col]])
      order_frame[[col]] <- result[[col]]
    }
    order_frame$set <- result$set
    res <- dplyr::arrange(order_frame, dplyr::across(!.index))
    if (!identical(res$.index,expected_idx)) return("order")
  }
  "ok"
}
runs <- 0L
outcomes <- list()
for (type in names(dim_sources)) for (size in c(0L, 6L)) for (source_class in c("tibble","frame","grouped","rowwise")) {
  original <- tib(fixed = c(1L, NA_integer_, 1L, 2L, 2L, NA_integer_), g = dim_sources[[type]],
                  h = factor_na, .source_id = 1:6, p = list(3:7, NULL, NA, integer(), "s", 1L),
                  packed = tib(a=factor_na, b=number), matrix = matrix(1:12,6L))
  original <- vctrs::vec_slice(original, seq_len(size))
  source <- switch(source_class, tibble=original, frame=as.data.frame(original),
                   grouped=dplyr::group_by(original,fixed), rowwise=dplyr::rowwise(original,fixed))
  fixed <- "fixed"
  for (plan_name in names(plans)) {
    plan <- plans[[plan_name]]
    for (label_type in c("missing","display")) for (keep in c(FALSE,TRUE)) for (sort in c("none","last","first")) for (rowwise in c(FALSE,TRUE)) {
      label <- if(label_type == "display") "Total" else NULL
      position <- if (identical(sort,"first")) "first" else "last"
      key <- if (rowwise) ".rows" else "..1"
      by <- if (source_class %in% c("grouped","rowwise")) NULL else quote(fixed)
      nest <- if(rowwise) nest_by_with_margins else nest_with_margins
      out <- tryCatch(rlang::inject(nest(source,.by = !!by, .grouping = plan$spec,
             .margin_label = label, .margin_label_position = position,
             .check_margin_label = FALSE, .duplicates = if (is.null(plan$duplicates)) "error" else plan$duplicates,
             .keep = keep, .sort = sort, .key = key, .id = "set")), error=identity)
      if (inherits(out,"error")) {
        verdict <- paste(class(out)[[1L]], strsplit(conditionMessage(out),"\n")[[1L]][[1L]])
      } else {
        verdict <- tryCatch(check(out, original, fixed, plan$sets, keep, label, position, sort, rowwise, key),
                            error=function(e) paste("oracle error", conditionMessage(e)))
      }
      runs <- runs + 1L
      cat(type,size,source_class,plan_name,label_type,keep,sort,rowwise,verdict,sep="	"); cat("
")
    }
  }
}
