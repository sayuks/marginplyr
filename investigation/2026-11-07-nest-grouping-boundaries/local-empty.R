pkgload::load_all(quiet = TRUE)
tib <- tibble::tibble
same_columns <- function(x,y) {
  if (!identical(names(x),names(y)) || nrow(x) != nrow(y)) return(FALSE)
  all(mapply(identical, as.list(x), as.list(y)))
}
source_specs <- list(
  no_columns = function(n) tib(.rows = n),
  keys_only = function(n) tib(g = rep_len(c("a","a","b"),n), h = rep_len(c(NA,TRUE,TRUE),n)),
  wide_payload = function(n) tib(
    g = rep_len(c("a","a","b"), n), h = rep_len(c(NA,TRUE,TRUE),n),
    payload = seq_len(n), object = rep_len(list(NULL, numeric(), 2L),n),
    matrix_0 = matrix(integer(), n, 0L), matrix_2 = matrix(seq_len(n*2),n,2L),
    packed_0 = tib(.rows=n), packed_2 = tib(integer=seq_len(n), str=rep_len(letters,n))
  )
)
set_cases <- list(
  none = list(plan=NULL, sets=list(character()), dimensions=character()),
  grand = list(plan=grouping_set(), sets=list(character()), dimensions=character()),
  union_grand = list(plan=grouping_sets(grouping_set(), grouping_set()),
                    sets=list(character()), dimensions=character()),
  only_g = list(plan=grouping_set(g), sets=list("g"), dimensions="g"),
  roll = list(plan=rollup(g,h), sets=list(c("g","h"),"g",character()),dimensions=c("g","h"))
)
for (size in c(0L,1L,3L)) for (type in names(source_specs)) {
  data <- source_specs[[type]](size)
  for (case in names(set_cases)) {
    p <- set_cases[[case]]
    if (!all(p$dimensions %in% names(data))) next
    fixed_cases <- list(none=character(),available=setdiff(intersect(c("g","h"),names(data)),p$dimensions))
    fixed_cases <- unique(fixed_cases)
    for (fixed in fixed_cases) for (rowwise in c(FALSE,TRUE)) for (keep in c(FALSE,TRUE)) for (sort in c("none","last","first")) for (id in list(NULL,"...")) {
      key <- if (type == "wide_payload") "payload" else ".rows"
      verb <- if (rowwise) nest_by_with_margins else nest_with_margins
      expected_keys <- c(fixed,p$dimensions)
      raw_groups <- lapply(p$sets,function(s) if (length(c(fixed,s))) {
        vctrs::vec_group_loc(data[c(fixed,s)])$loc
      } else if (size > 0L || (rowwise && !length(expected_keys))) list(seq_len(size)) else list())
      want <- unlist(raw_groups,recursive=FALSE)
      wanted_id <- rep(seq_along(raw_groups), lengths(raw_groups))
      want_payload <- if (keep) c(expected_keys, setdiff(names(data), expected_keys)) else setdiff(names(data), expected_keys)
      result <- tryCatch(verb(data,.by=dplyr::all_of(fixed),.grouping=p$plan,.id=id,
                             .keep=keep,.sort=sort,.key=key,.duplicates="drop",.margin_label=NULL),
                        error=identity)
      verdict <- if (inherits(result,"error")) conditionMessage(result) else if (
        nrow(result) != length(want) ||
        !identical(names(result),c(expected_keys,id,key)) ||
        !identical(dplyr::group_vars(result), if(rowwise) c(expected_keys,id) else character())) {
        paste("shape",nrow(result),length(want),paste(names(result),collapse=":"), paste(dplyr::group_vars(result), collapse=":"))
      } else {
        matched <- logical(length(want))
        for (j in seq_len(nrow(result))) {
          eligible <- if (is.null(id)) which(!matched) else which(!matched & wanted_id == result[[id]][[j]])
          match <- eligible[vapply(eligible, function(i) {
            candidate <- vctrs::vec_slice(data[want_payload], want[[i]])
            same_columns(result[[key]][[j]],candidate)
          },logical(1))]
          if (length(match)) matched[match[1L]] <- TRUE
        }
        if (all(matched)) "ok" else "membership"
      }
      cat(type,size,case,paste(fixed,collapse=","),rowwise,keep,sort,if(is.null(id)) "no" else id,verdict,sep="\t",fill=TRUE)
    }
  }
}
