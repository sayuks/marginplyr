pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
  cons <- list(sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'),
    duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ':memory:'))
  on.exit(lapply(cons, DBI::dbDisconnect), add = TRUE)
  original <- tibble(f = c('u','u','v','v'), g = c('a',NA,'b','b'),
    h = c(2L,1L,NA,3L), rid = 1:4, value = c(10,20,30,40))
  derive <- list(root = identity, empty = function(x) filter(x, FALSE),
    filter = function(x) filter(x, rid > 1L),
    distinct = function(x) distinct(x, f, g, .keep_all = TRUE),
    limit = function(x) head(x, 2), ordered_limit = function(x) head(arrange(x, desc(rid)), 2),
    window = function(x) mutate(arrange(x, rid), value = cumsum(value)),
    grouped_window = function(x) ungroup(mutate(group_by(arrange(x, rid), f), value = cumsum(value))),
    distinct_window = function(x) distinct(mutate(x, value = row_number()), f, g, h, rid, value),
    union = function(x) union_all(filter(x, rid < 3L), filter(x, rid >= 3L)),
    intersect = function(x) intersect(x, filter(x, rid < 3L)),
    except = function(x) setdiff(x, filter(x, rid < 3L)),
    join = function(x) inner_join(x, select(x, rid), by = 'rid'),
    semi = function(x) semi_join(x, filter(select(x, rid), rid > 1L), by = 'rid'),
    collapse = function(x) collapse(head(arrange(x, desc(rid)), 3)),
    computed = function(x) compute(filter(x, rid > 1L)))
  plans <- list(only = grouping_set(g,h), omit = grouping_sets(grouping_set(), grouping_set(g,h)),
    rollup = rollup(g,h), cube = cube(g,h), repeated = grouping_sets(rollup(g,h), grouping_set(g)))
  canonical <- function(x) {
    x <- as.data.frame(x)
    x[] <- lapply(x, function(v) ifelse(is.na(v), '<NA>', as.character(v)))
    rows <- apply(x, 1L, paste, collapse = '|')
    sort(rows)
  }
  results <- list()
  for (backend in names(cons)) {
    input <- copy_to(cons[[backend]], original, name = 'fixture', overwrite = TRUE, temporary = TRUE)
    for (shape in names(derive)) {
      source <- tryCatch(derive[[shape]](input), error = identity)
      if (inherits(source, 'error')) next
      source_local <- collect(source)
      for (pn in names(plans)) for (label in list(NULL, 'ALL')) for (sort_mode in c('none','first','last')) {
        expected <- expand_with_margins(source_local, .by = f, .grouping = plans[[pn]],
          .duplicates = 'keep', .margin_label = label, .id = 'sid', .sort = sort_mode)
        actual <- tryCatch(collect(expand_with_margins(source, .by = f, .grouping = plans[[pn]],
          .duplicates = 'keep', .margin_label = label, .id = 'sid', .sort = sort_mode)), error = identity)
        outcome <- if (inherits(actual, 'error')) conditionMessage(actual) else if (!identical(names(actual),names(expected))) 'NAMES' else if (!identical(canonical(actual),canonical(expected))) 'VALUES' else if (sort_mode != 'none' && !identical(as.data.frame(select(actual,f,g,h,sid)),as.data.frame(select(expected,f,g,h,sid)))) 'ORDER' else 'PASS'
        results[[length(results)+1L]] <- data.frame(backend,shape,plan=pn,label=if(is.null(label)) 'NULL' else label,sort=sort_mode,outcome)
      }
    }
  }
  out <- as_tibble(bind_rows(results))
  print(count(out,backend,outcome),n=Inf)
  print(filter(out,outcome != 'PASS'),n=Inf)
  write.csv(out, '/tmp/marginplyr-sql-composition-results.csv', row.names = FALSE)
})
