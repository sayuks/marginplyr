# Dated Arrow nested-type/source-graph differential, no package mutations.
Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "investigation/2026-11-02-expand-grouping-greedy/lazy")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(expr, error = identity)
fixture <- list(
  nested_list = function() arrow::Array$create(list(list(c(2L,3L), NULL), list(1L), NULL), type = arrow::list_of(arrow::list_of(arrow::int16()))),
  list_struct = function() arrow::Array$create(list(data.frame(x = 1:2, y = c("b", "a")), data.frame(x = 3L, y = "c"), NULL), type = arrow::list_of(arrow::struct(x = arrow::int32(), y = arrow::utf8()))),
  struct_nested = function() arrow::Array$create(data.frame(x = I(list(c(2L,3L), 1L, NULL))), type = arrow::struct(x = arrow::large_list_of(arrow::int32()))),
  list_map = function() arrow::Array$create(list(list(data.frame(key = c("b","a"), value = 1:2)), list(data.frame(key = "c", value = 3L)), NULL), type = arrow::list_of(arrow::map_of(arrow::utf8(), arrow::int32()))),
  extension = function() arrow::vctrs_extension_array(vctrs::new_vctr(c(2L,1L,NA), class = "hunt_extension")),
  struct_extension = function() arrow::StructArray$create(x = arrow::vctrs_extension_array(vctrs::new_vctr(c(2L,1L,NA), class = "hunt_extension")), y = c("b", "a", NA_character_)),
  chunked_dictionary = function() arrow::ChunkedArray$create(arrow::Array$create(factor(c("b","a"), levels = c("a","b"))), arrow::Array$create(factor(NA_character_, levels = c("b","a")))),
  chunked_struct = function() arrow::ChunkedArray$create(arrow::Array$create(data.frame(x = 1:2, y = c("b", "a"))), arrow::Array$create(data.frame(x = NA_integer_, y = NA_character_)))
)
canon <- function(x) {
  x <- as.data.frame(x)
  if (nrow(x)) x <- x[order(x$sid, x$rid), , drop = FALSE]
  rownames(x) <- NULL
  x
}
records <- list(); k <- 0L
for (name in names(fixture)) {
  arr <- capture(fixture[[name]]())
  if (inherits(arr, "error")) { cat("FIXTURE", name, conditionMessage(arr), "\n"); next }
  for (empty in c(FALSE, TRUE)) {
    a <- if (empty) arr$Slice(0L, 0L) else arr
    tbl <- capture(arrow::Table$create(g = a, rid = if(empty) integer() else 1:3, b = if(empty) character() else c("x","x","y")))
    if (inherits(tbl, "error")) { cat("TABLE", name, conditionMessage(tbl), "\n"); next }
    scratch <- tempfile("margin-nested-arrow-"); dir.create(scratch)
    ipc <- file.path(scratch, "facts.arrow")
    written <- capture(arrow::write_feather(tbl, ipc))
    ds <- if (inherits(written, "error")) written else capture(arrow::open_dataset(ipc, format = "ipc"))
    shapes <- list(table = tbl, batch = capture(arrow::as_record_batch(tbl)), table_union = capture(union_all(tbl, filter(tbl, rid == 2L))), table_join = capture(left_join(tbl, select(tbl, rid), by = "rid")))
    if (!inherits(ds,"error")) {
      shapes$dataset <- ds
      shapes$dataset_union <- capture(union_all(ds, filter(tbl, rid == 2L)))
      shapes$dataset_join <- capture(left_join(tbl, select(ds, rid), by = "rid"))
    }
    shape_filter <- Sys.getenv("MARGINPLYR_HUNT_SHAPES")
    if (nzchar(shape_filter)) shapes <- shapes[strsplit(shape_filter, ",", fixed = TRUE)[[1L]]]
    for (shape in names(shapes)) for (role in c("dimension", "payload", "fixed")) {
      src <- shapes[[shape]]
      source <- if(inherits(src,"error")) src else capture(collect(src))
      typ <- if(inherits(source,"error")) NULL else arrow::schema(src)$GetFieldByName("g")$type
      for (sort in c("none", "last")) {
        ordinary <- if(inherits(source,"error")) source else capture({
          inc <- mutate(src, sid = 1L)
          om <- if(role == "dimension") {
            nul <- arrow::Scalar$create(NA,type=arrow::null())$cast(typ)
            mutate(src, g = nul, sid = 2L)
          } else mutate(src, b = NA_character_, sid = 2L)
          q <- union_all(inc,om)
          if(sort == "last") {
            if(role == "fixed") q <- arrange(q,is.na(g),g,sid,is.na(b),b) else if(role == "dimension") q <- arrange(q,is.na(b),b,sid,is.na(g),g) else q <- arrange(q,sid,is.na(b),b)
          }
          list(data=collect(q), schema=arrow::schema(q))
        })
        actual <- if(inherits(source,"error")) source else capture({
          q <- if(role == "dimension") expand_with_margins(src,.by=b,.grouping=rollup(g),.margin_label=NULL,.id="sid",.sort=sort) else if(role == "fixed") expand_with_margins(src,.by=g,.grouping=rollup(b),.margin_label=NULL,.id="sid",.sort=sort) else expand_with_margins(src,.grouping=rollup(b),.margin_label=NULL,.id="sid",.sort=sort)
          list(data=collect(q),schema=arrow::schema(q))
        })
        status <- if(inherits(source,"error")) "source_error" else if(inherits(ordinary,"error")) {if(inherits(actual,"error")) "both_error" else "control_only_error"} else if(inherits(actual,"error")) "package_error" else if(!isTRUE(all.equal(canon(actual$data)[names(ordinary$data)],canon(ordinary$data)))) {
          a <- canon(actual$data)[names(ordinary$data)]; b <- canon(ordinary$data)
          if(is.factor(a$g) && is.factor(b$g)) {
            a$g <- as.character(a$g); b$g <- as.character(b$g)
            if(isTRUE(all.equal(a,b))) "factor_level_order_only" else "value_diff"
          } else "value_diff"
        } else if(!actual$schema$GetFieldByName("g")$type$Equals(ordinary$schema$GetFieldByName("g")$type)) "type_diff" else "pass"
        k <- k+1L
        records[[k]] <- data.frame(name=name,empty=empty,shape=shape,role=role,sort=sort,status=status,actual_error=if(inherits(actual,"error"))conditionMessage(actual)else"",control_error=if(inherits(ordinary,"error"))conditionMessage(ordinary)else"")
        if(status %in% c("package_error","value_diff","type_diff")) cat("LEAD",name,empty,shape,role,sort,status,records[[k]]$actual_error,"\n")
      }
    }
    unlink(scratch,recursive=TRUE)
  }
}
res <- bind_rows(records); write.csv(res,file.path(out,if(nzchar(Sys.getenv("MARGINPLYR_HUNT_SHAPES"))) "nested-arrow-batch-results.csv" else "nested-arrow-results.csv"),row.names=FALSE)
print(table(res$status)); print(table(res$name,res$status))
