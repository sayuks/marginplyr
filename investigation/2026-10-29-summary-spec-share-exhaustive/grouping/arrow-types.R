suppressPackageStartupMessages(pkgload::load_all(quiet=TRUE))
observe <- function(expr) tryCatch(force(expr), error=identity)
vals <- list(
 bool=arrow::Array$create(c(TRUE,FALSE)),
 i8=arrow::Array$create(c(1,2),type=arrow::int8()),
 i64=arrow::Array$create(c(1,2),type=arrow::int64()),
 u64=arrow::Array$create(c(1,2),type=arrow::uint64()),
 f32=arrow::Array$create(c(1,2),type=arrow::float32()),
 f64=arrow::Array$create(c(1,2),type=arrow::float64()),
 string=arrow::Array$create(c('a','b')),
 large_string=arrow::Array$create(c('a','b'),type=arrow::large_utf8()),
 decimal=arrow::Array$create(c(1,2),type=arrow::decimal128(6,2)),
 date32=arrow::Array$create(as.Date(c('2020-01-01','2020-01-02'))),
 date64=arrow::Array$create(as.Date(c('2020-01-01','2020-01-02')),type=arrow::date64()),
 timestamp=arrow::Array$create(as.POSIXct(c('2020-01-01','2020-01-02'),tz='UTC')),
 duration=arrow::Array$create(as.difftime(c(1,2),units='hours'),type=arrow::duration('s')),
 time32=arrow::Array$create(hms::as_hms(c(1,2)),type=arrow::time32('s')),
 time64=arrow::Array$create(hms::as_hms(c(1,2)),type=arrow::time64('us')),
 binary=arrow::Array$create(list(as.raw(1:2),as.raw(3:4)),type=arrow::binary()),
 large_binary=arrow::Array$create(list(as.raw(1:2),as.raw(3:4)),type=arrow::large_binary()),
 list_int=arrow::Array$create(list(1:2,3:4)),
 fixed_list=arrow::Array$create(list(1:2,3:4),type=arrow::fixed_size_list_of(arrow::int32(),2)),
 struct=arrow::Array$create(data.frame(x=1:2,y=c('a','b'))),
 dict_string=arrow::Array$create(factor(c('a','b'))),
 null=arrow::Array$create(c(NA,NA),type=arrow::null())
)
select_names <- function(data, pred) names(tidyselect::eval_select(rlang::new_quosure(rlang::expr(dplyr::where(!!pred)),environment()),data))
preds<-list(is.numeric,is.integer,is.double,is.character,is.factor,is.logical,is.list,is.data.frame,is.object,
 function(x)inherits(x,'Date'),function(x)inherits(x,'POSIXct'),function(x)inherits(x,'difftime'),
 function(x)inherits(x,'integer64'),function(x)inherits(x,'hms'),function(x)inherits(x,'arrow_binary'),function(x)is.null(x))
names(preds)<-c('numeric','integer','double','character','factor','logical','list','frame','object','Date','POSIXct','difftime','integer64','hms','arrow_binary','null')
checks <- 0L
failures <- list()
record <- function(label, truth, detail = NULL) {
 checks <<- checks + 1L
 if (!isTRUE(truth)) failures[[label]] <<- detail
}
for (downcast in c(TRUE, FALSE)) {
 old_options <- options(arrow.int64_downcast = downcast)
 tab <- do.call(arrow::Table$create, vals)
 batch <- do.call(arrow::RecordBatch$create, vals)
 reader <- arrow::RecordBatchReader$create(batch)
 inputs <- list(table = tab, batch = batch, dataset = arrow::InMemoryDataset$create(tab),
                query = dplyr::select(tab, dplyr::everything()), reader = reader)
 proxy <- as.data.frame(arrow::schema(tab))
 actual <- as.data.frame(tab)
 for (nm in names(vals)) {
  record(paste(downcast, nm, 'class'), identical(class(proxy[[nm]]), class(actual[[nm]])),
         list(metadata = class(proxy[[nm]]), materialized = class(actual[[nm]])))
 }
 for (input_name in names(inputs)) for (p in names(preds)) for (role in c('by', 'set')) {
  ref <- select_names(actual, preds[[p]])
  out <- observe(if (role == 'set') rlang::inject(inspect_grouping(
   inputs[[input_name]], .grouping = grouping_set(dplyr::where(!!preds[[p]])), .format = 'list'
  )) else rlang::inject(inspect_grouping(inputs[[input_name]], .by = dplyr::where(!!preds[[p]]), .format = 'list')))
  got <- if (inherits(out, 'error')) conditionMessage(out) else if (role == 'by') out$fixed[[1L]] else out$included[[1L]]
  record(paste(downcast, input_name, p, role), identical(ref, got), list(expected = ref, actual = got))
 }
 retained <- as.data.frame(reader)
 record(paste(downcast, 'reader-not-consumed'), identical(retained, actual), retained)
 options(old_options)
}
cat('Arrow physical fields:', length(vals), 'predicate comparisons:', length(inputs) * length(preds) * 2L * 2L,
    'assertions:', checks, 'failures:', length(failures), '\n')
if (length(failures)) { print(failures); quit(status = 1L) }
