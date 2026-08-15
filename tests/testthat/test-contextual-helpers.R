# The regression suite for ADR 0019. Every case below is derived from
# `static_spelling_rules()` rather than written out, because the failure this
# has to catch is a spelling nobody remembered: an enumeration records what
# someone thought to add, and a registry entry with no coverage is exactly the
# entry that reintroduces #172.
#
# Two layers, because they fail differently. The reader assertions run over
# every registered spelling and every namespace form and need no data, so they
# hold for a spelling whose end-to-end shape nobody has written yet. The
# behaviour assertions need a call to write, which no rule can derive; the
# probe table below supplies one per Contextual helper and is checked against
# the registry, so a spelling added without a probe fails here rather than
# going uncovered.

# A data frame every probe reads. Two numeric columns so that a selection can
# select more than one, and two dimensions so that a rollup and a cube of them
# differ -- which is what the constructor case below needs to tell a caller's
# own function from the package's.
contextual_probe_data <- function() {
  data.frame(
    region = c("E", "E", "W", "W"),
    grade = c("a", "b", "a", "b"),
    units = c(1, 2, 3, 4),
    qty = c(4, 5, 6, 7),
    stringsAsFactors = FALSE
  )
}

# One call per Contextual helper, as an expression rather than as a closure.
# It has to be an expression because the shadow the assertions install is a
# binding in the environment the summary is *written* in: a probe that closed
# over its own environment would capture its quosures there and never see the
# binding, which is the shape a first draft of this file had and which passes
# whatever the package does.
#
# `head` is how the three writings differ -- a bare symbol, a qualified call,
# or a foreign qualifier -- and `.probe_data` is the symbol the input is bound
# to in the evaluation environment.
#
# Every name below sits inside a defused expression, so `codetools` reads the
# input symbol and the grouping columns as undefined globals.
# nolint start: object_usage_linter.
contextual_probes <- function() {
  summary_probe <- function(build) {
    function(head) {
      rlang::expr(summarize_with_margins(
        .probe_data,
        k = !!build(head),
        .grouping = rollup(region),
        .sort = "last"
      ))
    }
  }
  share_probe <- function(head) {
    rlang::expr(summarize_with_margins(
      .probe_data,
      t = sum(units),
      k = !!rlang::call2(head, quote(t)),
      .grouping = rollup(region),
      .sort = "last"
    ))
  }
  arguments <- function(...) {
    args <- rlang::exprs(...)
    function(head) rlang::call2(head, !!!args)
  }

  list(
    # A per-branch constant, so the value itself is what a caller binding would
    # have replaced.
    grouping_bit = summary_probe(arguments(region)),
    grouping_id = summary_probe(arguments(region)),
    share_of_parent = share_probe,
    share_of_total = share_probe,
    # Wrapped so that the selection helper's own result reaches the summary as
    # a scalar. A caller binding answering a string is then a different value
    # or an error rather than a differently shaped result, which is what makes
    # the two outcomes comparable.
    across = summary_probe(function(head) {
      rlang::expr(ncol(!!rlang::call2(head, quote(c(units, qty)), quote(sum))))
    }),
    pick = summary_probe(function(head) {
      rlang::expr(ncol(!!rlang::call2(head, quote(units))))
    }),
    if_any = summary_probe(function(head) {
      rlang::expr(sum(!!rlang::call2(head, quote(units), quote(~ .x > 1))))
    }),
    if_all = summary_probe(function(head) {
      rlang::expr(sum(!!rlang::call2(head, quote(units), quote(~ .x > 1))))
    }),
    # A selection predicate in a contextual share's `across()` selection, which
    # is the one position the `predicate` family decides. It has to be the full
    # share `across()` grammar -- an explicit `.names`, the helper as `.fns` --
    # because a shorter spelling is refused by the source-shape rule before
    # `contains_selection_predicate()` is ever reached, and a probe rejected
    # upstream of the code under test asserts nothing about it.
    #
    # The share is refused here, so this probe compares diagnostics rather than
    # values, which is the other half of what a Contextual helper promises.
    where = function(head) {
      rlang::expr(summarize_with_margins(
        .probe_data,
        t = sum(units),
        u = sum(qty),
        dplyr::across(
          !!rlang::call2(head, quote(is.numeric)),
          share_of_total,
          .names = "{.col}_share"
        ),
        .grouping = rollup(region),
        .sort = "last"
      ))
    },
    cur_group = summary_probe(arguments()),
    cur_group_id = summary_probe(arguments()),
    cur_group_rows = summary_probe(arguments()),
    cur_data = summary_probe(arguments()),
    cur_data_all = summary_probe(arguments())
  )
}
# nolint end

# Every spelling the registry holds, in one flat table of family and name, so
# that a loop over it covers a family added later without being told about it.
contextual_registry_table <- function(families = static_spelling_families()) {
  rows <- lapply(
    families,
    function(family) {
      lapply(
        static_spelling_names(family),
        function(name) list(family = family, name = name)
      )
    }
  )
  unlist(rows, recursive = FALSE)
}

# The environment a probe is evaluated in: the package namespace, plus the
# input, plus optionally a binding of the spelling under test. The namespace is
# the parent rather than this file's environment so that the binding really is
# the only difference between the two calls each assertion makes -- a test-local
# name reachable from one and not the other would be a second difference
# nothing declared. Everything a probe names resolves through it: the verb and
# `rollup()` are exported, and `sum()` and `ncol()` reach base along the
# namespace's own parents.
contextual_probe_env <- function(data, shadow = NULL) {
  env <- rlang::new_environment(parent = rlang::ns_env("marginplyr"))
  rlang::env_bind(env, .probe_data = data)
  if (!is.null(shadow)) {
    rlang::env_bind(env, !!shadow := function(...) "CALLER")
  }
  env
}

# What a probe did, in a form two writings can be compared by. An error is
# reported as its message rather than propagated, because a family that refuses
# its spelling has to refuse all three writings identically, which is an
# agreement between messages and not the absence of one.
contextual_probe_outcome <- function(expr, data, shadow = NULL) {
  env <- contextual_probe_env(data, shadow = shadow)
  tryCatch(
    list(value = as.data.frame(collect_probe_result(
      rlang::eval_bare(expr, env)
    ))),
    error = function(cnd) list(error = conditionMessage(cnd))
  )
}

collect_probe_result <- function(result) {
  if (is.data.frame(result)) {
    return(result)
  }
  dplyr::collect(result)
}

test_that("the probe table covers exactly the Contextual helper spellings", {
  # The assertion that makes every loop below derived rather than enumerated:
  # a spelling added to `static_spelling_rules()` with no probe fails here, and
  # a probe naming no registered spelling fails here too, so neither side can
  # drift ahead of the other.
  registered <- vapply(
    contextual_registry_table(contextual_helper_families()),
    `[[`,
    character(1),
    "name"
  )
  expect_setequal(names(contextual_probes()), registered)
  # Every loop iterates over this set, so a set that arrived empty is a set
  # that passes.
  expect_gt(length(registered), 0L)
  # And the registry holds more than the helpers, which is what the constructor
  # case below exists for.
  expect_gt(length(contextual_registry_table()), length(registered))
})

test_that("a registered spelling is recognized bare and under every owner", {
  for (entry in contextual_registry_table()) {
    expect_identical(
      static_spelling_name(rlang::call2(entry$name), entry$family),
      entry$name,
      info = paste(entry$family, entry$name)
    )
    for (namespace in static_spelling_namespaces(entry$family)) {
      qualified <- rlang::call2(rlang::call2(
        "::",
        rlang::sym(namespace),
        rlang::sym(entry$name)
      ))
      expect_identical(
        static_spelling_name(qualified, entry$family),
        entry$name,
        info = paste(entry$family, namespace, entry$name)
      )
    }
  }
})

test_that("a foreign namespace passes a registered spelling through", {
  # `stats` owns none of these names, so every one of them qualified with it is
  # an ordinary call this package must not claim. Recognizing one would send a
  # caller's `stats::pick()` down a rewrite written for dplyr's.
  for (entry in contextual_registry_table()) {
    foreign <- rlang::call2(rlang::call2(
      "::",
      quote(stats),
      rlang::sym(entry$name)
    ))
    expect_null(
      static_spelling_name(foreign, entry$family),
      info = paste(entry$family, entry$name)
    )
  }
})

test_that("no registered spelling is recognized by another family", {
  # Each site asks about one family, so a spelling answered by two would be
  # rewritten twice, or refused in a position that accepts it.
  for (entry in contextual_registry_table()) {
    others <- setdiff(static_spelling_families(), entry$family)
    for (family in others) {
      expect_null(
        static_spelling_name(rlang::call2(entry$name), family),
        info = paste(entry$name, "must not be", family)
      )
    }
  }
})

test_that("a Contextual helper resolves the same bare and qualified", {
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    probe <- probes[[entry$name]]
    bare <- contextual_probe_outcome(probe(rlang::sym(entry$name)), data)
    for (namespace in static_spelling_namespaces(entry$family)) {
      head <- rlang::call2("::", rlang::sym(namespace), rlang::sym(entry$name))
      expect_identical(
        contextual_probe_outcome(probe(head), data),
        bare,
        info = paste(namespace, entry$name)
      )
    }
  }
})

test_that("a foreign qualifier is not the same request", {
  # What stops every probe above from being vacuous. Each of those asserts that
  # two writings agree, which a spelling the registry has stopped recognizing
  # satisfies just as well -- both writings would then change together and the
  # comparison would still hold. This one asserts a difference: `stats` owns
  # none of these names, so a `stats::`-qualified probe must not reach the
  # rewrite the bare one reaches, and it fails the moment recognition stops.
  #
  # It is the assertion that gives `where` its only end-to-end coverage. A
  # caller binding cannot change a spelling that is refused statically -- the
  # function is never called on either arm -- so the shadow assertion below can
  # only ever confirm that the refusal is stable, never that it happens.
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    probe <- probes[[entry$name]]
    foreign <- rlang::call2("::", quote(stats), rlang::sym(entry$name))
    expect_false(
      identical(
        contextual_probe_outcome(probe(foreign), data),
        contextual_probe_outcome(probe(rlang::sym(entry$name)), data)
      ),
      info = paste(entry$family, entry$name)
    )
  }
})

test_that("asking about one family builds no other", {
  # `static_spelling_rules()` derives the share family from `share_kind_rules()`
  # and the constructor family from `grouping_kind_rules()`. Built eagerly that
  # would make a grouping-family lookup evaluate the contextual-share module for
  # a fact that is not about shares, which is the reach `design/architecture.md`
  # separates on #179's authority. The table holds one builder per family so
  # that it does not, and this is what says so rather than the comment.
  local_mocked_bindings(
    share_kind_rules = function() stop("the share family was built"),
    grouping_kind_rules = function() stop("the constructor family was built")
  )
  expect_identical(
    static_spelling_name(quote(grouping_id(region)), "grouping"),
    "grouping_id"
  )
  expect_identical(
    static_spelling_name(quote(across(a, sum)), "selection"),
    "across"
  )
  expect_identical(
    static_spelling_name(quote(cur_group_id()), "refused"),
    "cur_group_id"
  )
  # And the mocks really would have fired, so the three above are not passing
  # because the bindings were never reachable.
  expect_error(static_spelling_names("share"), "the share family was built")
  expect_error(
    static_spelling_names("grouping_constructor"),
    "the constructor family was built"
  )
})

test_that("an unregistered selection helper is refused rather than rewritten", {
  # The `selection` family's rewrite dispatches by name, and every name it holds
  # has a branch. A spelling registered without one used to fall through to
  # `pick()`'s rewrite, which is a silently wrong selection rather than a
  # missing one; ADR 0015 makes that an invariant.
  local_mocked_bindings(
    static_spelling_name = function(expr, family) {
      if (identical(family, "selection")) "slice_head" else NULL
    }
  )
  expect_error(
    rewrite_summary_selections(
      quote(slice_head(units)),
      env = rlang::current_env(),
      data_proxy = data.frame(units = double()),
      normalize_across_names = FALSE
    ),
    "No rewrite is registered for the selection helper"
  )
})

test_that("a caller binding never changes a Contextual helper", {
  # The regression #172 recorded. Each spelling is bound, in the environment
  # the summary is written in, to a function answering something no probe could
  # produce, and the outcome must be the outcome of the unshadowed call.
  #
  # Both halves of ADR 0019 are asserted at once: a spelling that runs must run
  # the owning package's function, and a spelling that is refused must stay
  # refused with the same message.
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    expr <- probes[[entry$name]](rlang::sym(entry$name))
    expect_identical(
      contextual_probe_outcome(expr, data, shadow = entry$name),
      contextual_probe_outcome(expr, data),
      info = paste(entry$family, entry$name)
    )
  }
})

test_that("a caller binding never changes a helper on a lazy input", {
  # dtplyr rather than a database, because several probes wrap their result in
  # `ncol()`, which SQL cannot translate. What the lazy half proves is that the
  # rewrite survives a backend that re-reads the expression: dtplyr and dbplyr
  # both re-analyze the call, and dbplyr's `partial_eval()` matches these names
  # without examining the qualifier at all, so a qualified head reaches the
  # translation an unqualified one did.
  skip_if_backend_absent("dtplyr")
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    expr <- probes[[entry$name]](rlang::sym(entry$name))
    expect_identical(
      contextual_probe_outcome(
        expr,
        dtplyr::lazy_dt(data),
        shadow = entry$name
      ),
      contextual_probe_outcome(expr, dtplyr::lazy_dt(data)),
      info = paste(entry$family, entry$name)
    )
  }
})

test_that("a shadowed `where()` still selects by predicate", {
  # The half of `where`'s contract that the registry does not keep. It is
  # recognized only to be refused inside a contextual share's `across()`, so
  # every probe above compares two diagnostics; a `where()` in an ordinary
  # selection reaches `tidyselect::eval_select()` instead, which binds the name
  # in the selection mask itself.
  #
  # This is asserted here because the Rd, the vignette, and NEWS all say a
  # binding of `where` never wins, and nothing else in this change would notice
  # tidyselect stopping. The promise is this package's; the code keeping it is
  # another package's, which is exactly the kind of claim that needs a test
  # rather than a comment.
  data <- contextual_probe_data()
  select_numeric <- rlang::expr(summarize_with_margins(
    .probe_data,
    dplyr::across(where(is.numeric), sum),
    .grouping = rollup(region),
    .sort = "last"
  ))
  expect_identical(
    contextual_probe_outcome(select_numeric, data, shadow = "where"),
    contextual_probe_outcome(select_numeric, data)
  )
  # And the selection really was made by the predicate, so the agreement above
  # is not two calls that both selected nothing.
  expect_named(
    contextual_probe_outcome(select_numeric, data)$value,
    c("region", "units", "qty")
  )
})

test_that("a Grouping specification constructor is an ordinary name", {
  # The other direction of ADR 0019, and the reason the constructor family
  # carries `contextual = FALSE`. Its spelling decides only whether a nested
  # argument is evaluated; what runs once the gate opens is whatever the name
  # is bound to, so a caller's own function reaches the nested position and its
  # result is what the verb compiles.
  data <- contextual_probe_data()
  summarize <- function(spec) {
    summarize_with_margins(
      data,
      k = sum(units),
      .grouping = !!spec,
      .sort = "last"
    )
  }
  rollup <- function(...) marginplyr::cube(...)
  shadowed <- summarize_with_margins(
    data,
    k = sum(units),
    .grouping = grouping_sets(rollup(region, grade)),
    .sort = "last"
  )
  expect_identical(
    shadowed,
    summarize(rlang::expr(grouping_sets(marginplyr::cube(region, grade))))
  )
  # And the caller's function really won: a cube of two dimensions holds a set
  # the rollup this spelling names does not.
  expect_false(identical(
    shadowed,
    summarize(rlang::expr(grouping_sets(marginplyr::rollup(region, grade))))
  ))
})

test_that("the refusal names the helper and keeps its opening", {
  data <- contextual_probe_data()
  # The opening phrase six other assertions match by regular expression. It is
  # asserted here as well so that a rewording is caught where the wording is
  # decided rather than only where it is relied on.
  expect_error(
    summarize_with_margins(data, k = dplyr::cur_group_id(), .by = region),
    "^`summarize_with_margins\\(\\)` does not support `cur_group_id\\(\\)`\\."
  )
  expect_error(
    summarize_with_margins(data, k = dplyr::cur_group_id(), .by = region),
    "reserved inside a Margin summary and is not resolved from the calling"
  )
  # A caller who bound the name themselves is the reader that sentence was
  # added for, so it has to survive the shadow that motivates it.
  expect_error(
    local({
      cur_group_id <- function() 1L
      summarize_with_margins(data, k = cur_group_id(), .by = region)
    }),
    "is not resolved from the calling environment"
  )
  # More than one refused spelling in one call reads as a list.
  expect_error(
    summarize_with_margins(
      data,
      k = dplyr::cur_group_id(),
      j = list(dplyr::cur_group()),
      .by = region
    ),
    "These spellings are reserved"
  )
})
