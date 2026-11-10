# Reproduce the 2026-11-10 native column-vector element-name observations.
# Run from the repository root with Rscript; research packages must be installed.
main <- function() {
  packages <- c('dplyr', 'vctrs', 'tibble', 'data.table', 'dtplyr',
                'arrow', 'dbplyr', 'RSQLite')
  print(R.version.string)
  print(vapply(packages, function(package) {
    as.character(utils::packageVersion(package))
  }, character(1)))

  source <- tibble::tibble(
    g = c(k1 = 'A', k2 = 'A', k3 = 'B'),
    x = c(r1 = 10, r2 = 20, r3 = 30)
  )
  frames <- list(
    tibble = source,
    data.frame = structure(as.list(source), class = 'data.frame',
                           row.names = seq_len(nrow(source)))
  )
  direct <- data.table::as.data.table(source)
  for (column in names(source)) {
    data.table::setattr(direct[[column]], 'names', names(source[[column]]))
  }
  frames$data.table <- direct

  for (kind in names(frames)) {
    input <- frames[[kind]]
    cat('\nInput:', kind, '\n')
    print(lapply(input, names))
    summary <- dplyr::summarise(input, first = x[1], total = sum(x), .by = g)
    cat('Native summary names\n')
    print(lapply(summary, names))
    nested <- tidyr::nest(input, .by = g)
    cat('Native nest outer/first inner names\n')
    print(lapply(nested, names))
    print(lapply(nested$data[[1L]], names))
  }

  cat('\nNative display replacement and ordering\n')
  print(lapply(dplyr::mutate(source, g = 'Total'), names))
  print(lapply(dplyr::mutate(source, g = NA_character_), names))
  print(lapply(dplyr::arrange(source, dplyr::desc(x)), names))
  for (element_names in list(c('same', 'same', 'last'), c('', '', ''),
                            c(NA_character_, 'second', NA_character_))) {
    input <- source
    names(input$g) <- element_names
    names(input$x) <- element_names
    cat('Native summary with element names\n')
    print(element_names)
    print(lapply(dplyr::summarise(input, first = x[1], .by = g), names))
  }

  cat('\nNames at lazy entry/collection\n')
  print(lapply(dtplyr::lazy_dt(source)$parent, names))
  connection <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  remote <- dplyr::copy_to(connection, source, 'element_name_probe')
  print(lapply(dplyr::collect(remote), names))

  table <- arrow::Table$create(source)
  cat('\nArrow source names stored in R metadata\n')
  print(table$schema$metadata$r$columns$x$attributes$names)
  show_arrow <- function(query) {
    result <- dplyr::collect(query)
    print(list(values = unname(result$x), element_names = names(result$x)))
  }
  cat('Arrow native ordering\n')
  show_arrow(dplyr::arrange(table, dplyr::desc(x)))
  cat('Arrow native filtering; any metadata warning is part of the observation\n')
  show_arrow(dplyr::filter(table, x >= 20))
  cat('Arrow native union; branch row order is not asserted\n')
  show_arrow(dplyr::union_all(
    dplyr::mutate(table, g = g), dplyr::mutate(table, g = 'Total')
  ))
}

main()
