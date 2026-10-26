# Run from the audited checkout. The DBI connection and translator are ordinary
# public extension points; no marginplyr binding is replaced.
main <- function(output, leaf_type = 's3') {
  stopifnot(leaf_type %in% c('s3', 's4'))
  if (file.exists(output)) stop('Output already exists')
  pkgload::load_all('.', quiet = TRUE)
  old <- options()
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dplyr::copy_to(con, data.frame(g = c('a', 'b'), v = c(1, 2)), 'hunt_source')
  methods::setClass('HuntConditionSQLiteConnection', contains = 'SQLiteConnection')
  on.exit(methods::removeClass('HuntConditionSQLiteConnection'), add = TRUE)
  subclass <- methods::new('HuntConditionSQLiteConnection', con)
  dialect <- dbplyr::sql_dialect(con)
  variant <- dbplyr::sql_translation(con)
  original_sum <- variant$aggregate$sum
  state <- new.env(parent = emptyenv())
  registerS3method('sql_dialect', 'HuntConditionSQLiteConnection', function(con) {
    structure(dialect, class = c('hunt_condition_sqlite', class(dialect)))
  }, envir = asNamespace('dbplyr'))
  registerS3method('sql_translation', 'hunt_condition_sqlite', function(con) {
    dbplyr::sql_variant(variant$scalar, dbplyr::sql_translator(
      .parent = variant$aggregate,
      sum = function(...) {
        if (state$signal == 'ordinary_warning') warning(state$leaf)
        if (state$signal == 'bare_warning') signalCondition(state$leaf)
        if (state$signal == 'ordinary_message') message(state$leaf)
        if (state$signal == 'bare_message') signalCondition(state$leaf)
        original_sum(...)
      }), variant$window)
  }, envir = asNamespace('dbplyr'))
  registerS3method('conditionMessage', 'hunt_condition', function(cnd) {
    state$formats <- state$formats + 1L
    # Restrict the secondary disturbance to diagnostic formatting. Every
    # original signal's own formatting remains identical in the paired runs.
    audit_format <- any(vapply(sys.calls(), function(call) {
      is.call(call) && identical(call[[1L]], as.name('sent_query_condition_message'))
    }, logical(1)))
    if (audit_format) {
      switch(state$format,
        error = stop('secondary formatter error'),
        warning = warning('secondary formatter warning'),
        message = message('secondary formatter message'),
        bare_warning = signalCondition(simpleWarning('secondary formatter warning')),
        bare_message = signalCondition(simpleMessage('secondary formatter message')),
        invalid = return(character()),
        reentry = marginplyr::inspect_grouping(data.frame(z = 1),
                                               .grouping = marginplyr::rollup(z))
      )
    }
    structure(cnd$message, retained = environment())
  }, envir = asNamespace('base'))
  if (leaf_type == 's4') {
    methods::setOldClass(c('hunt_condition', 'warning', 'condition'))
    methods::setOldClass(c('hunt_message', 'message', 'condition'))
    methods::setClass('HuntS4Warning', contains = c('list', 'hunt_condition'))
    methods::setClass('HuntS4Message', contains = c('list', 'hunt_message'))
    registerS3method('conditionMessage', 'hunt_message',
      getS3method('conditionMessage', 'hunt_condition', envir = asNamespace('base')),
      envir = asNamespace('base'))
    on.exit(rm('conditionMessage.hunt_message',
               envir = asNamespace('base')$.__S3MethodsTable__.), add = TRUE)
  }
  on.exit(rm(list = c('sql_dialect.HuntConditionSQLiteConnection',
                     'sql_translation.hunt_condition_sqlite'),
             envir = asNamespace('dbplyr')$.__S3MethodsTable__.), add = TRUE)
  on.exit(rm('conditionMessage.hunt_condition',
             envir = asNamespace('base')$.__S3MethodsTable__.), add = TRUE)
  remote <- dplyr::tbl(subclass, 'hunt_source')
  run <- function(audited, signal, format, warn) {
    state$signal <- signal
    state$format <- format
    state$formats <- 0L
    state$leaf <- structure(list(message = 'primary translator condition', call = NULL),
                           class = c('hunt_condition',
                                     if (grepl('warning', signal)) 'warning' else 'message',
                                     'condition'))
    if (leaf_type == 's4') {
      state$leaf <- methods::new(if (grepl('warning', signal)) {
        'HuntS4Warning'
      } else {
        'HuntS4Message'
      }, list(message = 'primary translator condition', call = NULL))
    }
    options(marginplyr.audit_sql = audited, warn = warn,
            rlib_warning_verbosity = 'default', lifecycle_verbosity = 'warning')
    before <- options(c('warn', 'rlib_warning_verbosity', 'lifecycle_verbosity'))
    delivered <- list()
    observe <- function(cnd) {
      delivered[[length(delivered) + 1L]] <<- cnd
      # Leave warnings unmuffled to exercise warn=2 conversion; messages use a
      # caller restart only when the original signaller supplies one.
      if (inherits(cnd, 'message') && !is.null(findRestart('muffleMessage')))
        invokeRestart('muffleMessage')
    }
    result <- tryCatch(withCallingHandlers(
      marginplyr::summarize_with_margins(remote, total = sum(v, na.rm = TRUE),
        .grouping = marginplyr::rollup(g), .check_margin_label = TRUE),
      warning = observe, message = observe), error = identity)
    rows <- if (audited) marginplyr::last_sent_queries(details = TRUE) else NULL
    if (audited) {
      stopifnot(identical(names(rows), c('purpose', 'sql', 'source', 'status',
                                       'conditions')))
      for (summaries in rows$conditions) {
        stopifnot(identical(names(summaries), c('classes', 'message', 'phase',
                                             'handling')),
                  is.character(summaries$message),
                  is.null(attributes(summaries$message)),
                  all(vapply(summaries$classes, function(classes) {
                    is.character(classes) && is.null(attributes(classes))
                  }, logical(1))))
      }
    }
    stopifnot(identical(before, options(names(before))))
    outcome <- if (inherits(result, 'error')) {
      list(classes = class(result), message = .subset2(result, 'message'))
    } else {
      class(result)
    }
    list(outcome = outcome,
         delivered = lapply(delivered, function(cnd) {
           list(classes = class(cnd), message = .subset2(cnd, 'message'))
         }),
         formats = state$formats, rows = rows)
  }
  configs <- expand.grid(signal = c('ordinary_warning', 'bare_warning',
                                    'ordinary_message', 'bare_message'),
                         format = c('plain', 'error', 'warning', 'message',
                                    'bare_warning', 'bare_message', 'invalid', 'reentry'),
                         warn = c(-1L, 0L, 1L, 2L), stringsAsFactors = FALSE)
  observations <- vector('list', nrow(configs))
  for (i in seq_len(nrow(configs))) {
    cfg <- as.list(configs[i, ])
    off <- do.call(run, c(list(audited = FALSE), cfg))
    on <- do.call(run, c(list(audited = TRUE), cfg))
    observations[[i]] <- list(config = cfg, off = off, on = on,
      outcome_equal = identical(off$outcome, on$outcome),
      delivery_equal = identical(off$delivered, on$delivered))
  }
  saveRDS(observations, output)
  failed <- which(!vapply(observations, function(x) x$outcome_equal && x$delivery_equal,
                         logical(1)))
  cat('Leaf type:', leaf_type, '\nCases:', nrow(configs),
      '\nFailure indices:', paste(failed, collapse = ', '), '\n')
  if (length(failed)) print(configs[failed, ])
  stopifnot(length(failed) == 0L)
}
args <- commandArgs(trailingOnly = TRUE)
main(args[[1L]], if (length(args) >= 2L) args[[2L]] else 's3')
