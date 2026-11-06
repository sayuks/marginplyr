Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all('.', quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
output <- dirname(normalizePath(sub('^--file=', '', commandArgs()[grepl('^--file=', commandArgs())])))
fixtures <- list(
  text = Array$create(c('z', 'a', NA, 'b')),
  integer = Array$create(c(9L, -2L, NA, 3L)),
  double = Array$create(c(9.5, -2.5, NA, 3.5)),
  date = Array$create(as.Date(c('2026-01-09', '2026-01-01', NA, '2026-01-03'))),
  timestamp = Array$create(as.POSIXct(c('2026-01-09', '2026-01-01', NA, '2026-01-03'), tz = 'UTC')),
  int64 = Array$create(c('9007199254740995', '-9007199254740995', NA, '9007199254740994'))$cast(int64()),
  decimal = Array$create(c('99999999999999.12', '-99999999999999.12', NA, '99999999999999.11'))$cast(decimal128(22, 2)),
  dictionary = DictionaryArray$create(Array$create(c(0L, 1L, NA, 2L)), Array$create(c('z', 'a', 'b', 'unused')))
)
families <- list(
  rollup = list(spec = rollup(g, h), sets = list(c('g', 'h'), 'g', character())),
  composite = list(spec = rollup(grouping_set(g, h)), sets = list(c('g', 'h'), character())),
  cube = list(spec = cube(g, h), sets = list(c('g', 'h'), 'g', 'h', character())),
  repeated = list(spec = grouping_sets(grouping_set(), grouping_set(g), grouping_set(), grouping_set(g, h)), sets = list(character(), 'g', character(), c('g', 'h')))
)
capture <- function(x) tryCatch(force(x), error = function(e) e)
results <- list()
k <- 0L
tmp <- tempfile('arrow-exact-supplement-')
dir.create(tmp)
on.exit <- NULL
for (type in names(fixtures)) {
  source_table <- Table$create(g = fixtures[[type]], h = c('b', 'a', NA, 'c'), f = c(NA, 'x', 'x', 'a'), rid = 1:4)
  path <- file.path(tmp, paste0(type, '.arrow'))
  write_ipc_file(source_table, path)
  dataset <- open_dataset(path, format = 'ipc')
  sources <- list(table = source_table, batch = as_record_batch(source_table), dataset = dataset, uniondataset = open_dataset(list(dataset, dataset)))
  for (shape in names(sources)) for (family in names(families)) for (sort in c('first', 'last')) {
    sets <- families[[family]]$sets
    copies <- if (shape == 'uniondataset') 2L else 1L
    q <- capture(expand_with_margins(sources[[shape]], .by = f, .grouping = families[[family]]$spec, .duplicates = 'keep', .id = 'sid', .margin_label = NULL, .sort = sort))
    actual <- capture(if (inherits(q, 'error')) q else as_arrow_table(q))
    status <- 'error'
    error <- ''
    rows_ok <- FALSE
    types_ok <- FALSE
    values_ok <- FALSE
    names_ok <- FALSE
    if (inherits(actual, 'error')) {
      error <- conditionMessage(actual)
    } else {
      # Only small integer identifiers cross into R; physical dimension values
      # remain Arrow arrays throughout the expected-value construction.
      rid <- as.vector(actual$GetColumnByName('rid'))
      sid <- as.vector(actual$GetColumnByName('sid'))
      rows_ok <- identical(sort(unique(sid)), seq_along(sets)) &&
        all(vapply(seq_along(sets), function(i) identical(sort(rid[sid == i]), rep(1:4, each = copies)), logical(1)))
      if (rows_ok) {
        source_indices <- rid - 1L
        g_indices <- source_indices
        h_indices <- source_indices
        g_omitted <- vapply(sid, function(i) !'g' %in% sets[[i]], logical(1))
        h_omitted <- vapply(sid, function(i) !'h' %in% sets[[i]], logical(1))
        g_indices[g_omitted] <- NA_integer_
        h_indices[h_omitted] <- NA_integer_
        expected <- list(
          f = source_table$GetColumnByName('f')$Take(Array$create(source_indices)),
          g = source_table$GetColumnByName('g')$Take(Array$create(g_indices)),
          h = source_table$GetColumnByName('h')$Take(Array$create(h_indices)),
          sid = ChunkedArray$create(Array$create(as.integer(sid))),
          rid = source_table$GetColumnByName('rid')$Take(Array$create(source_indices))
        )
        names_ok <- identical(names(actual), names(expected))
        types_ok <- all(vapply(names(expected), function(nm) actual$GetColumnByName(nm)$type$Equals(expected[[nm]]$type), logical(1)))
        values_ok <- all(vapply(names(expected), function(nm) actual$GetColumnByName(nm)$Equals(expected[[nm]]), logical(1)))
      }
      status <- if (rows_ok && names_ok && types_ok && values_ok) 'match' else 'mismatch'
    }
    k <- k + 1L
    results[[k]] <- data.frame(type, shape, family, sort, status, rows_ok, names_ok, types_ok, values_ok, error)
  }
}
r <- bind_rows(results)
write.csv(r, file.path(output, 'arrow-exact-supplement.csv'), row.names = FALSE)
print(count(r, status))
print(filter(r, status != 'match'), row.names = FALSE)
unlink(tmp, recursive = TRUE)
stopifnot(nrow(r) == 256L, all(r$status == 'match'))
