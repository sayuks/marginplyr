# The limit on a Grouping plan's grouping-set occurrences (ADR 0046, #1745).

# A one-row frame of `n` distinct integer columns, and their symbols.
limit_columns <- function(n) {
  keys <- paste0("x", seq_len(n))
  list(
    data = as.data.frame(stats::setNames(as.list(seq_len(n)), keys)),
    syms = rlang::syms(keys)
  )
}

test_that("inspection accepts 4,096 occurrences and refuses 4,097", {
  withr::local_options(marginplyr.max_grouping_sets = NULL)
  twelve <- limit_columns(12L)
  plan <- inspect_grouping(twelve$data, .grouping = cube(!!!twelve$syms))
  expect_identical(nrow(plan), 4096L)

  thirteen <- limit_columns(13L)
  cnd <- expect_error(
    inspect_grouping(thirteen$data, .grouping = cube(!!!thirteen$syms)),
    class = "marginplyr_error"
  )
  expect_match(conditionMessage(cnd), "8,192", fixed = TRUE)
  expect_match(conditionMessage(cnd), "4,096", fixed = TRUE)
  expect_match(
    conditionMessage(cnd), "marginplyr.max_grouping_sets", fixed = TRUE
  )

  # One occurrence past the limit, from an explicit list.
  sets <- c(list(grouping_set()), rep(list(quote(x1)), 4096L))
  expect_error(
    inspect_grouping(
      twelve$data, .grouping = grouping_sets(!!!sets), .duplicates = "keep"
    ),
    "4,097", class = "marginplyr_error"
  )
})

test_that("a refusal does not build the plan it refuses", {
  withr::local_options(marginplyr.max_grouping_sets = NULL)
  wide <- limit_columns(30L)
  specs <- list(
    cube(!!!wide$syms),
    grouping_spec(cube(!!!wide$syms), grouping_sets(x1, grouping_set())),
    grouping_spec(!!!lapply(wide$syms, function(column) {
      grouping_sets(!!column, grouping_set())
    })),
    grouping_sets(rollup(x1), cube(!!!wide$syms))
  )
  # Building 2^20 occurrences took 39 s and 1.3 GB in the 2026-11-21 note, so
  # a plan of 2^30 that was being built would reach the time limit instead of
  # the refusal, and fail the expectation rather than exhaust memory.
  refuse_within <- function(spec, duplicates, seconds = 10) {
    setTimeLimit(elapsed = seconds, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf))
    inspect_grouping(wide$data, .grouping = spec, .duplicates = duplicates)
  }
  for (spec in specs) {
    for (duplicates in c("error", "keep", "drop")) {
      expect_error(
        refuse_within(spec, duplicates),
        "the limit of 4,096", class = "marginplyr_error"
      )
    }
  }
})

test_that("the option raises, removes, or lowers the limit for every verb", {
  thirteen <- limit_columns(13L)
  spec <- cube(!!!thirteen$syms)
  withr::local_options(marginplyr.max_grouping_sets = 8192)
  expect_identical(nrow(inspect_grouping(thirteen$data, .grouping = spec)),
                   8192L)
  withr::local_options(marginplyr.max_grouping_sets = Inf)
  expect_identical(nrow(inspect_grouping(thirteen$data, .grouping = spec)),
                   8192L)

  # Eight occurrences pass and nine are refused, through each verb.
  withr::local_options(marginplyr.max_grouping_sets = 8L)
  data <- data.frame(a = 1L, b = 2L, c = 3L, d = 4L, value = 1L)
  within <- cube(a, b, c)
  beyond <- grouping_sets(cube(a, b, c), d)
  for (verb in names(forwarded_verbs)) {
    call <- forwarded_verbs[[verb]]
    expect_no_error(call(data, NULL, within))
    expect_error(
      call(data, NULL, beyond), "the limit of 8", class = "marginplyr_error"
    )
  }
})

test_that("an invalid option value is refused naming the option", {
  data <- data.frame(a = 1L)
  for (value in list(0, -1, NA, NA_real_, "many", 2.5, c(8, 9), -Inf, NaN,
                     TRUE)) {
    withr::local_options(marginplyr.max_grouping_sets = value)
    expect_error(
      inspect_grouping(data, .grouping = rollup(a)),
      "marginplyr.max_grouping_sets", class = "marginplyr_error"
    )
  }
  # A refusal the specification earns keeps its precedence over the option's.
  expect_error(
    inspect_grouping(data, .grouping = cube()),
    "requires at least one dimension", class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(data, .grouping = cube(a, a)),
    "more than one argument", class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(data, .grouping = cube(zzz)), "doesn't exist"
  )
  withr::local_options(marginplyr.max_grouping_sets = 1L)
  expect_identical(
    nrow(inspect_grouping(data, .grouping = grouping_set(a))), 1L
  )
})

test_that("a plan dropped back within the limit is not refused", {
  withr::local_options(marginplyr.max_grouping_sets = NULL)
  data <- data.frame(key = 1L)
  # 2^20 written occurrences of two distinct sets (#1245).
  repeated <- cube(!!!rep(list(quote(key)), 20L))
  plan <- inspect_grouping(data, .grouping = repeated, .duplicates = "drop",
                           .format = "list")
  expect_identical(plan$included, list("key", character()))

  family <- grouping_sets(key, grouping_set())
  product <- grouping_spec(!!!rep(list(family), 21L))
  plan <- inspect_grouping(data, .grouping = product, .duplicates = "drop",
                           .format = "list")
  expect_identical(plan$included, list("key", character()))

  # Two copies of a 4,096-occurrence cube are 8,192 written and 4,096 kept.
  twelve <- limit_columns(12L)
  twice <- grouping_sets(cube(!!!twelve$syms), cube(!!!twelve$syms))
  expect_identical(
    nrow(inspect_grouping(twelve$data, .grouping = twice,
                          .duplicates = "drop")),
    4096L
  )
  expect_error(
    inspect_grouping(twelve$data, .grouping = twice, .duplicates = "keep"),
    "8,192", class = "marginplyr_error"
  )
})

test_that("every constructor's occurrences count toward the limit", {
  withr::local_options(marginplyr.max_grouping_sets = 8L)
  data <- data.frame(a = 1L, b = 2L, c = 3L, d = 4L, e = 5L, f = 6L, g = 7L,
                     h = 8L)
  within <- list(
    rollup(a, b, c, d, e, f, g),
    grouping_spec(rollup(a), rollup(b), rollup(c)),
    grouping_sets(a, b, c, d, e, f, g, h),
    grouping_spec(cube(a, b), grouping_sets(c, d))
  )
  beyond <- list(
    rollup(a, b, c, d, e, f, g, h),
    grouping_spec(rollup(a), rollup(b), rollup(c, d)),
    grouping_sets(a, b, c, d, e, f, g, h, grouping_set()),
    grouping_spec(cube(a, b), grouping_sets(c, d, e)),
    grouping_sets(rollup(a, b), rollup(c, d), rollup(e, f))
  )
  for (spec in within) {
    expect_identical(nrow(inspect_grouping(data, .grouping = spec)), 8L)
  }
  for (spec in beyond) {
    expect_error(
      inspect_grouping(data, .grouping = spec, .duplicates = "keep"),
      "the limit of 8", class = "marginplyr_error"
    )
  }
})

test_that("a lazy input is refused before any query is sent", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(a = 1L, b = 2L, c = 3L, d = 4L, value = 1),
    "grouping_set_limit", temporary = TRUE
  )
  withr::local_options(
    marginplyr.max_grouping_sets = 8L, marginplyr.audit_sql = TRUE
  )

  # The same input within the limit records what it sends.
  summarize_with_margins(remote, total = sum(value), .grouping = cube(a, b, c))
  expect_gt(nrow(last_sent_queries()), 0L)

  # A union past the limit stops before its next branch, here a selection
  # whose column types would be queried for.
  awaiting_types <- grouping_sets(
    a, b, c, d, c(a, b), c(a, c), c(a, d), c(b, c), c(b, d), where(is.numeric)
  )
  for (verb in c("summarize_with_margins", "expand_with_margins")) {
    call <- forwarded_verbs[[verb]]
    for (spec in list(cube(a, b, c, d), awaiting_types)) {
      expect_error(
        call(remote, NULL, spec), "the limit of 8",
        class = "marginplyr_error"
      )
      expect_identical(nrow(last_sent_queries()), 0L)
    }
  }
})

test_that("sibling families in a union are bounded together", {
  withr::local_options(marginplyr.max_grouping_sets = NULL)
  twelve <- limit_columns(12L)
  siblings <- grouping_sets(!!!rep(list(cube(!!!twelve$syms)), 100L))
  # Refused partway: a union built to its last branch would state its count,
  # 409,600, rather than that it expands to more than the limit.
  expect_error(
    inspect_grouping(twelve$data, .grouping = siblings, .duplicates = "keep"),
    "`grouping_sets()` in the Grouping specification expands to more",
    fixed = TRUE, class = "marginplyr_error"
  )
  # Under "drop" the repeats add nothing, so three siblings stay within it.
  repeated <- grouping_sets(!!!rep(list(cube(!!!twelve$syms)), 3L))
  dropped <- inspect_grouping(twelve$data, .grouping = repeated,
                              .duplicates = "drop")
  expect_identical(
    dropped,
    inspect_grouping(twelve$data, .grouping = cube(!!!twelve$syms))
  )

  # Passing the limit at the last branch states the union's count.
  last <- grouping_sets(cube(!!!twelve$syms), x1)
  expect_error(
    inspect_grouping(twelve$data, .grouping = last, .duplicates = "keep"),
    "expands to 4,097 grouping sets", class = "marginplyr_error"
  )
})

test_that("a union crossed under \"drop\" is bounded across its branches", {
  withr::local_options(marginplyr.max_grouping_sets = NULL)
  thirteen <- limit_columns(13L)
  twelve <- thirteen$syms[1:12]
  # Repeats of one cube add nothing to the product they are crossed into.
  repeated <- grouping_spec(
    grouping_sets(!!!rep(list(cube(!!!twelve)), 3L)), x13
  )
  expect_identical(
    inspect_grouping(thirteen$data, .grouping = repeated,
                     .duplicates = "drop"),
    inspect_grouping(thirteen$data, .grouping = grouping_spec(cube(!!!twelve),
                                                              x13))
  )
  # Two overlapping cubes hold 6,144 distinct sets, refused before the
  # product is formed.
  fourteen <- limit_columns(14L)
  shifted <- grouping_spec(
    grouping_sets(cube(!!!twelve), cube(!!!fourteen$syms[2:13])), x14
  )
  expect_error(
    inspect_grouping(fourteen$data, .grouping = shifted, .duplicates = "drop"),
    "`grouping_spec()` in the Grouping specification expands to more",
    fixed = TRUE, class = "marginplyr_error"
  )
})

test_that("a product walked for a duplicate stops at the limit", {
  # No factor has a private column, so under "error" the product is walked
  # for a repeat; its first repeat is at position 513, past the limit of 300.
  withr::local_options(marginplyr.max_grouping_sets = 300)
  ten <- limit_columns(10L)
  product <- grouping_spec(
    grouping_sets(x1, x2), grouping_sets(x1, x2), cube(!!!ten$syms[3:10])
  )
  expect_error(
    inspect_grouping(ten$data, .grouping = product),
    "`grouping_spec()` in the Grouping specification expands to more",
    fixed = TRUE, class = "marginplyr_error"
  )
  withr::local_options(marginplyr.max_grouping_sets = 600)
  expect_error(
    inspect_grouping(ten$data, .grouping = product),
    "257, 513", fixed = TRUE, class = "marginplyr_error"
  )
})

test_that("a product nested in a union counts its prefix as written flat", {
  # Crossed with two prefix sets, the nested product's first factor keeps
  # four sets, which its second factor would merge back to three (#1749).
  withr::local_options(marginplyr.max_grouping_sets = 3L)
  data <- data.frame(a = 1L, b = 2L)
  inner <- list(grouping_sets(b, grouping_set()), grouping_sets(a, b))
  nested <- grouping_spec(
    grouping_sets(a, grouping_set()),
    grouping_sets(grouping_spec(!!!inner))
  )
  flat <- grouping_spec(grouping_sets(a, grouping_set()), !!!inner)
  for (spec in list(nested, flat)) {
    expect_error(
      inspect_grouping(data, .grouping = spec, .duplicates = "drop"),
      "`grouping_spec()` in the Grouping specification", fixed = TRUE,
      class = "marginplyr_error"
    )
  }
  withr::local_options(marginplyr.max_grouping_sets = 4L)
  plan <- inspect_grouping(data, .grouping = nested, .duplicates = "drop",
                           .format = "list")
  expect_identical(plan$included, list(c("a", "b"), "a", "b"))
})

test_that("a cube crossed under \"drop\" names the family that passed", {
  withr::local_options(marginplyr.max_grouping_sets = 8L)
  data <- data.frame(a = 1L, b = 2L, c = 3L, d = 4L, x = 5L)
  # Crossed with one prefix set, the cube's own family passes the limit.
  expect_error(
    inspect_grouping(data, .grouping = grouping_spec(x, cube(a, b, c, d)),
                     .duplicates = "drop"),
    "`cube()` in the Grouping specification", fixed = TRUE,
    class = "marginplyr_error"
  )
  # Crossed with two, the cube is built only as part of the product, as is a
  # family resolved in full.
  crossed <- list(
    grouping_spec(grouping_sets(x, grouping_set()), cube(a, b, c, d)),
    grouping_spec(grouping_sets(x, grouping_set()), rollup(a, b, c, d))
  )
  for (spec in crossed) {
    expect_error(
      inspect_grouping(data, .grouping = spec, .duplicates = "drop"),
      "`grouping_spec()` in the Grouping specification", fixed = TRUE,
      class = "marginplyr_error"
    )
  }
  # A union past the limit stops before its cube branch.
  withr::local_options(marginplyr.max_grouping_sets = 1L)
  union <- grouping_spec(grouping_sets(b, x, cube(a, d)), c)
  expect_error(
    inspect_grouping(data, .grouping = union, .duplicates = "drop"),
    "`grouping_spec()` in the Grouping specification", fixed = TRUE,
    class = "marginplyr_error"
  )
})
