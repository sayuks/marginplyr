# These inputs exercise the defensive seams whose malformed states cannot be
# assembled by a public verb, plus small valid states omitted by earlier tests.
test_that("internal classification guards reject bad states", {
  expect_error(
    static_spelling_rule("missing-family"),
    "Unknown static-spelling family", fixed = TRUE
  )
  expect_error(
    backend_capabilities("missing-backend"),
    "Unknown marginplyr backend kind", fixed = TRUE
  )
  expect_error(
    share_adapter("missing-backend"),
    "Unknown contextual-share backend kind", fixed = TRUE
  )
  expect_error(
    share_kind_rule("missing-kind"),
    "Unknown contextual-share kind", fixed = TRUE
  )
  expect_error(
    grouping_sql_expr("x", NULL),
    "A database connection is required", fixed = TRUE
  )
  expect_error(
    expand_grouping_family(
      list(spec = list(type = "future")), data.frame(x = 1L)
    ),
    "Unknown Grouping specification kind", fixed = TRUE
  )

  expect_error(
    attach_grouping_sets_query(list(), list()),
    "dbplyr query representation has changed", fixed = TRUE
  )
  malformed <- structure(list(lazy_query = 1L), class = "tbl_lazy")
  expect_error(
    attach_grouping_sets_query(malformed, list()),
    "dbplyr query representation has changed", fixed = TRUE
  )
  expect_error(
    validate_grouping_sets_query(list()),
    "dbplyr query representation has changed", fixed = TRUE
  )
})

test_that("internal readers preserve valid edge states", {
  expect_identical(restate_margin_keys(character(), c(x = "y")), character())
  expect_identical(
    factor_missing_sentinel(
      list(levels = "..marginplyr_missing_factor_code"), "Margin"
    ),
    "..marginplyr_missing_factor_code_"
  )
  expect_identical(
    encode_factor_for_margin(factor(c("a", NA)), "missing", FALSE),
    c("a", NA_character_)
  )
  expect_null(find_grouping_kind_rule(c("set", "cube")))
  expect_false(holds_grouping_spec_function(new.env(parent = emptyenv())))
  expect_identical(total_set_ids(list(
    sets = list("x"), by = character(), set_ids = 1L
  )), NA_integer_)
  expect_identical(
    reflective_lookup_symbols(quote(get()), character()), character()
  )
  expect_identical(
    captured_call_parts(quote(substitute(env = parent.frame()))), FALSE
  )
  expect_identical(
    check_dtplyr_share_scalar(
      2L, "unselected", list(inputs = "selected"), "call"
    ),
    2L
  )
  expect_identical(
    wrap_dtplyr_share_across(
      quote(dplyr::across(x, list(sum))), list(), quote(call())
    ),
    quote(dplyr::across(x, list(sum)))
  )
  expected <- data.frame(x = 1L)
  expect_identical(
    execute_shares(
      structure(list(), class = "marginplyr_margin_operation"),
      new_margin_summary_stage(expected, "set_id"), list(), TRUE
    ),
    expected
  )
  expect_identical(
    parse_pick_selection(quote(dplyr::pick())),
    quote(dplyr::everything())
  )
  proxy <- data.frame(x = 1L)
  expect_identical(
    restore_input_window_order(
      dbplyr::lazy_frame(x = 1L), list(quote(absent))
    ),
    dbplyr::lazy_frame(x = 1L)
  )
  expect_identical(
    rewrite_across_selection(
      quote(dplyr::across(.fns = sum)), environment(), proxy,
      normalize_across_names = FALSE, call_name = "across"
    ),
    quote(dplyr::across(.cols = dplyr::all_of("x"), .fns = sum))
  )
  expect_null(data_frame_valued_summary_kind(1L))
  expect_identical(
    known_data_frame_output_names(
      quote(dplyr::pick(x)), environment(), proxy
    ),
    "x"
  )
  expect_identical(
    names(resolve_summary_selection(
      rlang::quo(x), environment(), data.frame(x = 1L)
    )),
    "x"
  )
  expect_identical(
    known_injected_argument_name(quote(`:=`(paste0("a", "b"), 1L))),
    ""
  )
  expect_identical(
    known_injected_argument_name(quote(identity(x))), ""
  )
  expect_identical(
    across_output_provenance(
      quote(dplyr::across(x, sum)), environment(), data.frame(x = 1L),
      output_names = c("other", "another"), expands_own_names = TRUE
    ),
    list(
      inputs = c(NA_character_, NA_character_),
      functions = c(NA_integer_, NA_integer_)
    )
  )
  expect_identical(
    known_across_output_names(
      quote(dplyr::across(x, sum, .names = 1L)), environment(), proxy
    ),
    character()
  )
})

test_that("a planner without preflight still rejects nested shares", {
  plan <- compile_grouping_spec(
    rollup(g), c("g", "v"), duplicates_choices = margin_duplicates_choices
  )
  expect_error(
    plan_share_expressions(
      list(invalid = rlang::quo(share_of_parent(v) + 1L)),
      data.frame(g = "a", v = 1L), plan, "set_id"
    ),
    "must be the complete right-hand side", fixed = TRUE
  )
})

test_that("cardinality mapping tolerates an absent analyzed source", {
  requests <- list(list(
    kind = "parent", outputs = "part", sources = "total"
  ))
  expect_identical(share_cardinality_records(list(), requests), list())
})

test_that("a known invalid across name expansion is refused", {
  expect_error(
    check_across_name_count(c("one", "two"), "{.col*}", "x"),
    "must produce one name per column", fixed = TRUE
  )
})

test_that("a newly classified frame shape needs an output-name reader", {
  local_mocked_bindings(
    data_frame_valued_summary_kind = function(expr) "future"
  )
  expect_error(
    known_data_frame_output_names(
      quote(future(x)), environment(), data.frame(x = 1L)
    ),
    "Unhandled data-frame-valued summary kind", fixed = TRUE
  )
})
