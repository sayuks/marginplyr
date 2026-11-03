test_that("SQLite Boolean-word Margin results join from either input", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "a", `TRUE` = 7L, check.names = FALSE), "facts"
  )
  keys <- dplyr::copy_to(con, data.frame(g = c("a", "Total")), "keys")
  query <- expand_with_margins(source, .grouping = rollup(g))
  saved <- dplyr::compute(query, name = "saved", analyze = FALSE)
  expected <- data.frame(g = c("a", "Total"), `TRUE` = c(7L, 7L),
                         check.names = FALSE)
  for (input in list(source, saved, query)) {
    for (right in c(FALSE, TRUE)) {
      joined <- if (right) {
        dplyr::inner_join(keys, input, by = "g")
      } else {
        dplyr::inner_join(input, keys, by = "g")
      }
      actual <- as.data.frame(dplyr::collect(joined))
      target <- if (identical(input, source)) expected[1L, ] else expected
      expect_equal(dplyr::arrange(actual, .data$g),
                   dplyr::arrange(target, .data$g))
    }
  }
})

test_that("SQLite Boolean-word Margin inputs retain bag and set semantics", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "a"), `FALSE` = c(19L, 19L),
                    check.names = FALSE), "bags"
  )
  query <- expand_with_margins(source, .grouping = rollup(g))
  saved <- dplyr::compute(query, name = "bag_saved", analyze = FALSE)
  for (input in list(source, saved, query)) {
    for (right in c(FALSE, TRUE)) {
      x <- if (right) source else input
      y <- if (right) input else source
      different <- !identical(input, source)
      expected <- list(
        c(rep("a", 4L), if (different) rep("Total", 2L)),
        c("a", if (different) "Total"), "a",
        if (different && !right) "Total" else character()
      )
      verbs <- list(dplyr::union_all, dplyr::union,
                    dplyr::intersect, dplyr::setdiff)
      for (i in seq_along(verbs)) {
        actual <- as.data.frame(dplyr::collect(verbs[[i]](x, y)))
        expect_identical(names(actual), c("g", "FALSE"))
        expect_identical(sort(actual$g), sort(expected[[i]]))
        expect_identical(actual[["FALSE"]], rep(19L, nrow(actual)))
      }
    }
  }
})

test_that("SQLite binary inputs cover public column roles and Margin choices", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  canonical <- function(data) {
    data <- as.data.frame(lapply(data, as.character), check.names = FALSE)
    dplyr::arrange(data, dplyr::across(dplyr::everything()))
  }
  verbs <- list(
    inner = function(x, y, by) {
      dplyr::inner_join(
        x, y, by, na_matches = "na", relationship = "many-to-many"
      )
    },
    left = function(x, y, by) {
      dplyr::left_join(
        x, y, by, na_matches = "na", relationship = "many-to-many"
      )
    },
    right = function(x, y, by) {
      dplyr::right_join(
        x, y, by, na_matches = "na", relationship = "many-to-many"
      )
    },
    full = function(x, y, by) {
      dplyr::full_join(
        x, y, by, na_matches = "na", relationship = "many-to-many"
      )
    },
    semi = function(x, y, by) dplyr::semi_join(x, y, by, na_matches = "na"),
    anti = function(x, y, by) dplyr::anti_join(x, y, by, na_matches = "na"),
    cross = function(x, y, by) dplyr::cross_join(x, y),
    union_all = function(x, y, by) dplyr::union_all(x, y),
    union = function(x, y, by) dplyr::union(x, y),
    intersect = function(x, y, by) dplyr::intersect(x, y),
    setdiff = function(x, y, by) dplyr::setdiff(x, y)
  )
  words <- c("TRUE", "FALSE", "true", "false", "TrUe", "FaLsE")
  roles <- c("dimension", "fixed", "payload", "identifier")
  for (i in seq_len(12L)) {
    word <- words[(i - 1L) %% length(words) + 1L]
    role <- roles[(i - 1L) %% length(roles) + 1L]
    fixed <- if (role == "fixed") word else "p"
    dimension <- if (role == "dimension") word else "g"
    payload <- if (role == "payload") word else "v"
    id <- if (role == "identifier") word else "set"
    data <- data.frame(p = "north", g = c("a", "a", "b"),
                       v = c(7L, 7L, 19L),
                       ..MARGINPLYR_BOOLEAN_1 = 99L)
    names(data)[1:3] <- c(fixed, dimension, payload)
    label <- if (i %% 2L == 0L) NULL else "Total"
    sort <- c("none", "first", "last")[(i - 1L) %% 3L + 1L]
    for (empty in c(FALSE, TRUE)) {
      selected <- dplyr::copy_to(
        con, if (empty) data[FALSE, ] else data,
        "role_source", overwrite = TRUE
      )
      query <- expand_with_margins(
        selected, .by = dplyr::all_of(fixed),
        .grouping = rollup(dplyr::all_of(dimension)),
        .margin_label = label, .id = id, .sort = sort
      )
      dplyr::compute(query, "role_saved", overwrite = TRUE, analyze = FALSE)
      # Reopen the physical control without compute's rowid ORDER BY, which
      # ordinary dbplyr cannot nest for Boolean-word names on SQLite.
      saved <- dplyr::tbl(con, "role_saved")
      expected_source <- if (empty) data[FALSE, ] else data
      margin <- data
      margin[[dimension]] <- if (is.null(label)) NA_character_ else label
      expected <- rbind(data, margin)
      expected[[id]] <- rep(1:2, each = 3L)
      expected <- expected[c(fixed, dimension, id, payload,
                             "..MARGINPLYR_BOOLEAN_1")]
      if (empty) expected <- expected[FALSE, ]
      peer <- expected
      if (!empty) peer[[payload]][[1L]] <- 31L
      peer_query <- dplyr::copy_to(con, peer, "role_peer", overwrite = TRUE)
      for (j in seq_len(3L)) {
        input <- list(source = selected, saved = saved, margin = query)[[j]]
        target <- if (j == 1L) expected_source else expected
        other <- dplyr::select(peer_query, dplyr::all_of(names(target)))
        local_other <- peer[names(target)]
        for (right in c(FALSE, TRUE)) {
          x <- if (right) other else input
          y <- if (right) input else other
          local_x <- if (right) local_other else target
          local_y <- if (right) target else local_other
          for (verb in names(verbs)) {
            by <- c(fixed, dimension)
            info <- paste(word, role, empty, sort, j, right, verb)
            actual <- dplyr::collect(verbs[[verb]](x, y, by))
            reference <- verbs[[verb]](local_x, local_y, by)
            expect_identical(names(actual), names(reference), info = info)
            expect_equal(canonical(actual), canonical(reference), info = info)
          }
        }
      }
    }
  }
})
