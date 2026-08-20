# The regression suite for ADR 0019 and #178. Every case below is derived from
# `static_spelling_rules()` rather than written out, because the failure this
# has to catch is a spelling nobody remembered: an enumeration records what
# someone thought to add, and a registry entry with no coverage is exactly the
# entry that reintroduces #172.
#
# The writings each spelling is exercised in are derived too, from the rule
# rather than from the registry: a name is written bare or under each owner the
# family declares, and each of those through the redundant parentheses #178
# makes transparent. An enumeration of demonstrated spellings is what that
# ticket found in the code, where four families recognized whichever forms
# someone had written out, so an enumeration of them here would assert the
# defect rather than its absence.
#
# Two layers, because they fail differently. The reader assertions run over
# every registered spelling and every writing and need no data, so they hold
# for a spelling whose end-to-end shape nobody has written yet. The behaviour
# assertions need a call to write, which no rule can derive; the probe table
# below supplies one per Contextual helper and is checked against the registry,
# so a spelling added without a probe fails here rather than going uncovered.

# The input every probe reads is `contextual_probe_data()`, in
# `helper-contextual-probes.R` because the other suite probing these helpers
# reads the same one.

# One call per Contextual helper, as an expression rather than as a closure.
# It has to be an expression because the shadow the assertions install is a
# binding in the environment the summary is *written* in: a probe that closed
# over its own environment would capture its quosures there and never see the
# binding, which is the shape a first draft of this file had and which passes
# whatever the package does.
#
# `spell` is how the writings differ. A probe writes its helper's arguments
# once and hands them to the writing, which decides how the name in front of
# them is spelled -- bare, qualified, foreign, or wrapped in parentheses. A
# probe that built the call itself would fix one writing per probe, which is
# the enumeration this file exists not to be.
#
# `.probe_data` is the symbol the input is bound to in the evaluation
# environment.
#
# Every name below sits inside a defused expression, so `codetools` reads the
# input symbol and the grouping columns as undefined globals.
# nolint start: object_usage_linter.
contextual_probes <- function() {
  summary_probe <- function(build) {
    function(spell) {
      rlang::expr(summarize_with_margins(
        .probe_data,
        k = !!build(spell),
        .grouping = rollup(region),
        .sort = "last"
      ))
    }
  }
  share_probe <- function(spell) {
    rlang::expr(summarize_with_margins(
      .probe_data,
      t = sum(units),
      k = !!spell(list(quote(t))),
      .grouping = rollup(region),
      .sort = "last"
    ))
  }
  arguments <- function(...) {
    args <- rlang::exprs(...)
    function(spell) spell(args)
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
    across = summary_probe(function(spell) {
      rlang::expr(ncol(!!spell(list(quote(c(units, qty)), quote(sum)))))
    }),
    pick = summary_probe(function(spell) {
      rlang::expr(ncol(!!spell(list(quote(units)))))
    }),
    if_any = summary_probe(function(spell) {
      rlang::expr(sum(!!spell(list(quote(units), quote(~ .x > 1)))))
    }),
    if_all = summary_probe(function(spell) {
      rlang::expr(sum(!!spell(list(quote(units), quote(~ .x > 1)))))
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
    where = function(spell) {
      rlang::expr(summarize_with_margins(
        .probe_data,
        t = sum(units),
        u = sum(qty),
        dplyr::across(
          !!spell(list(quote(is.numeric))),
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

# One writing of a spelling: a function from a call's arguments to the call,
# labelled by what it produces so that a failure says which writing failed
# rather than which loop index did. `wrap` is the parenthesis around the whole
# call, as distinct from the one around its head; both are pairs R evaluates as
# the identity function, and #178 is the ticket that made the two agree with the
# call written without either.
contextual_writing <- function(head, wrap = FALSE) {
  spell <- function(args) {
    call <- rlang::call2(head, !!!args)
    if (wrap) {
      return(rlang::call2("(", call))
    }
    call
  }
  list(label = rlang::as_label(spell(list(quote(...)))), spell = spell)
}

# Every writing of the head spellings it is given. Derived from the parenthesis
# rule rather than listed: bare, one pair around the head, two pairs around it,
# and one pair around the whole call. Two pairs are what says the reading is not
# one pair deep, which a single-step unwrapping would pass while `((pick))(x)`
# still went unrecognized.
contextual_writings <- function(spellings) {
  unlist(
    lapply(
      spellings,
      function(spelling) {
        parenthesized <- rlang::call2("(", spelling)
        list(
          contextual_writing(spelling),
          contextual_writing(parenthesized),
          contextual_writing(rlang::call2("(", parenthesized)),
          contextual_writing(spelling, wrap = TRUE)
        )
      }
    ),
    recursive = FALSE
  )
}

# The head spellings a family owns: the bare name, and the name under each
# namespace the registry records as an owner of it.
contextual_owned_spellings <- function(family, name) {
  c(
    list(rlang::sym(name)),
    lapply(
      static_spelling_namespaces(family),
      function(namespace) {
        rlang::call2("::", rlang::sym(namespace), rlang::sym(name))
      }
    )
  )
}

# `stats` owns none of these names, so every writing of one qualified with it is
# an ordinary call this package must not claim.
contextual_foreign_spellings <- function(name) {
  list(rlang::call2("::", quote(stats), rlang::sym(name)))
}

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
# reported rather than propagated, because a family that refuses its spelling
# has to refuse every writing identically, which is an agreement between
# refusals and not the absence of one.
#
# Its class is reported beside its message, so the agreement covers the typed
# behaviour and not only the wording. That is what the writings need most: a
# spelling recognition misses is not usually silent but reaches the data mask
# and fails there, and `object 'pick' not found` is an untyped condition of the
# class ADR 0015 separates from this package's own.
contextual_probe_outcome <- function(expr, data, shadow = NULL) {
  env <- contextual_probe_env(data, shadow = shadow)
  tryCatch(
    list(value = as.data.frame(collect_probe_result(
      rlang::eval_bare(expr, env)
    ))),
    error = function(cnd) {
      list(error = conditionMessage(cnd), class = class(cnd))
    }
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

test_that("a registered spelling is recognized however it is written", {
  # Bare, under every owner, and through every arrangement of the parentheses
  # R evaluates as the identity function. Each site reads the name through one
  # shared reader, so a writing missed here is a writing missed by the
  # recognition, the rewrite, and the refusal alike.
  for (entry in contextual_registry_table()) {
    spellings <- contextual_owned_spellings(entry$family, entry$name)
    for (writing in contextual_writings(spellings)) {
      expect_identical(
        static_spelling_name(writing$spell(list()), entry$family),
        entry$name,
        info = paste(entry$family, writing$label)
      )
    }
  }
})

test_that("a foreign namespace passes a registered spelling through", {
  # `stats` owns none of these names, so every one of them qualified with it is
  # an ordinary call this package must not claim. Recognizing one would send a
  # caller's `stats::pick()` down a rewrite written for dplyr's. Parentheses
  # change nothing about that: they are transparent to which name is written,
  # not to whose it is.
  for (entry in contextual_registry_table()) {
    foreign <- contextual_writings(contextual_foreign_spellings(entry$name))
    for (writing in foreign) {
      expect_null(
        static_spelling_name(writing$spell(list()), entry$family),
        info = paste(entry$family, writing$label)
      )
    }
  }
})

test_that("a head this cannot resolve statically is no spelling at all", {
  # The boundary ADR 0019 draws for #178, asserted at the reader. Identity is
  # syntactic: a pair of parentheses is read through because the name is still
  # written inside it, while a head that has to be *evaluated* to know what it
  # calls stays unresolved under the conservative #130 policy -- whether the
  # evaluation is a lookup, a string R would refuse to apply, or a literal
  # function.
  for (entry in contextual_registry_table()) {
    computed <- rlang::call2("get", entry$name)
    # The string is written inside the parentheses rather than in front of them
    # because R's parser reads a bare `"pick"(units)` as the symbol `pick` and
    # calls it, while `("pick")(units)` keeps the string and raises "attempt to
    # apply non-function". Recognizing the second would make this package accept
    # a call R refuses.
    unresolved <- list(
      rlang::call2(computed),
      rlang::call2(rlang::call2("(", computed)),
      rlang::call2(rlang::call2("(", entry$name)),
      rlang::call2(quote(function() NULL))
    )
    for (call in unresolved) {
      expect_null(
        static_spelling_name(call, entry$family),
        info = paste(entry$family, rlang::as_label(call))
      )
    }
  }
})

test_that("a helper reference is read through parentheses too", {
  # The `.fns` position takes a helper by value rather than by call, and `(f)`
  # is the value `f` is. It reads the same registry through the same namespace
  # rule, so leaving it out would put one position back on a spelling of its
  # own -- which is what #172 found at four sites and #178 finds at this one.
  for (entry in contextual_registry_table()) {
    references <- c(
      contextual_owned_spellings(entry$family, entry$name),
      lapply(
        contextual_owned_spellings(entry$family, entry$name),
        function(spelling) rlang::call2("(", rlang::call2("(", spelling))
      )
    )
    for (reference in references) {
      expect_identical(
        static_spelling_reference_name(reference, entry$family),
        entry$name,
        info = paste(entry$family, rlang::as_label(reference))
      )
    }
    for (foreign in contextual_foreign_spellings(entry$name)) {
      expect_null(
        static_spelling_reference_name(
          rlang::call2("(", foreign),
          entry$family
        ),
        info = paste(entry$family, entry$name)
      )
    }
    expect_null(
      static_spelling_reference_name(
        rlang::call2("(", rlang::call2("get", entry$name)),
        entry$family
      ),
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

test_that("a Contextual helper resolves the same however it is written", {
  # What the reader assertions above say about one function, said about the
  # verb: two writings R evaluates identically must produce identical results,
  # or identical diagnostics for a spelling that is refused. Parentheses are in
  # this loop rather than in one of their own because they are the same
  # question the namespace forms are -- which written call this is -- and #178
  # is the ticket that found them answered differently.
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    probe <- probes[[entry$name]]
    plain <- contextual_writing(rlang::sym(entry$name))
    bare <- contextual_probe_outcome(probe(plain$spell), data)
    spellings <- contextual_owned_spellings(entry$family, entry$name)
    for (writing in contextual_writings(spellings)) {
      expect_identical(
        contextual_probe_outcome(probe(writing$spell), data),
        bare,
        info = paste(entry$family, writing$label)
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
    plain <- contextual_writing(rlang::sym(entry$name))
    bare <- contextual_probe_outcome(probe(plain$spell), data)
    foreign <- contextual_writings(contextual_foreign_spellings(entry$name))
    for (writing in foreign) {
      expect_false(
        identical(contextual_probe_outcome(probe(writing$spell), data), bare),
        info = paste(entry$family, writing$label)
      )
    }
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
  #
  # Every writing is shadowed, not only the bare one. A parenthesized head is
  # the writing where a binding is hardest to lose to: R evaluates `(pick)` as
  # a value, so ordinary lookup would find the caller's function without even
  # the function-lookup rule that skips a non-function binding (#178).
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    probe <- probes[[entry$name]]
    spellings <- contextual_owned_spellings(entry$family, entry$name)
    for (writing in contextual_writings(spellings)) {
      expr <- probe(writing$spell)
      expect_identical(
        contextual_probe_outcome(expr, data, shadow = entry$name),
        contextual_probe_outcome(expr, data),
        info = paste(entry$family, writing$label)
      )
    }
  }
})

test_that("a caller binding never changes a helper on a lazy input", {
  # dtplyr rather than a database, because several probes wrap their result in
  # `ncol()`, which SQL cannot translate. What the lazy half proves is that the
  # rewrite survives a backend that re-reads the expression: dtplyr and dbplyr
  # both re-analyze the call, and dbplyr's `partial_eval()` matches these names
  # without examining the qualifier at all, so a qualified head reaches the
  # translation an unqualified one did.
  skip_if_suggest_absent("dtplyr")
  data <- contextual_probe_data()
  probes <- contextual_probes()
  for (entry in contextual_registry_table(contextual_helper_families())) {
    probe <- probes[[entry$name]]
    spellings <- contextual_owned_spellings(entry$family, entry$name)
    for (writing in contextual_writings(spellings)) {
      expr <- probe(writing$spell)
      expect_identical(
        contextual_probe_outcome(
          expr,
          dtplyr::lazy_dt(data),
          shadow = entry$name
        ),
        contextual_probe_outcome(expr, dtplyr::lazy_dt(data)),
        info = paste(entry$family, writing$label)
      )
      # And the lazy plan is the plan the local input produced, so a writing
      # the rewrite reached locally has reached the backend's re-analysis too.
      expect_identical(
        contextual_probe_outcome(expr, dtplyr::lazy_dt(data)),
        contextual_probe_outcome(expr, data),
        info = paste("lazy", entry$family, writing$label)
      )
    }
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

test_that("a nested constructor is gated however it is written", {
  # The constructor family is not a Contextual helper, so the probe loops above
  # leave it out -- and it still reads its spelling statically, which is what
  # puts it under the parenthesis rule (#178). What its gate decides is whether
  # a nested argument is evaluated at all, so a writing the gate misses is not
  # refused but *selected*: the specification reaches tidyselect as a column
  # selection and the caller is told their `rollup()` is not a column.
  data <- contextual_probe_data()
  probe <- function(spell) {
    rlang::expr(summarize_with_margins(
      .probe_data,
      k = sum(units),
      .grouping = grouping_sets(
        !!spell(list(quote(region), quote(grade)))
      ),
      .sort = "last"
    ))
  }
  entries <- contextual_registry_table("grouping_constructor")
  # Every loop iterates over this set, so a set that arrived empty is a set
  # that passes.
  expect_gt(length(entries), 0L)
  for (entry in entries) {
    plain <- contextual_writing(rlang::sym(entry$name))
    bare <- contextual_probe_outcome(probe(plain$spell), data)
    spellings <- contextual_owned_spellings(entry$family, entry$name)
    for (writing in contextual_writings(spellings)) {
      expect_identical(
        contextual_probe_outcome(probe(writing$spell), data),
        bare,
        info = paste(entry$family, writing$label)
      )
    }
    # And the gate really is what the agreement above rests on: a qualifier
    # naming a package that owns none of these names does not open it, so those
    # writings must not agree.
    foreign <- contextual_writings(contextual_foreign_spellings(entry$name))
    for (writing in foreign) {
      expect_false(
        identical(contextual_probe_outcome(probe(writing$spell), data), bare),
        info = paste(entry$family, writing$label)
      )
    }
  }
})

test_that("the refusal names the helper and keeps its opening", {
  data <- contextual_probe_data()
  # Both arms byte-exactly, and #223's re-authoring is what they are worth
  # writing out for. The sentence #172 added inflects a demonstrative, a noun,
  # and two verbs, and all four now go through `{?}` behind one `cli::qty()`
  # rather than through an `if` spelling each arm. What stood here before was
  # a substring per arm, and neither covered its own arm whole: the singular's
  # reached `is not resolved` but opened after `This spelling is`, and the
  # plural's stopped at `These spellings are reserved`. Read together they
  # covered every inflection once and no arm at all.
  #
  # Here rather than in `test-diagnostic-pluralization.R`, on that file's own
  # sentence that re-authoring a message is not where the question of which
  # diagnostics its baseline covers gets decided. Not on its selection rule:
  # this refusal does suffix a noun, so admitting it would be a defensible
  # call -- it is just not this pull request's to make, any more than the
  # duplicate-grouping-set refusal's exclusion was, which is pinned at its own
  # site for a reason that file states and revisits.
  #
  # Each identity also asserts the opening phrase six other assertions match
  # by regular expression, so a rewording is caught where the wording is
  # decided rather than only where it is relied on -- and it asserts both ends
  # of the split #223 gave the refusal, the phrase closing the main line and
  # the helper opening the bullet under it. Five of those six read across that
  # break instead, matching the phrase alone or reaching the helper with `.*`;
  # the sixth spells the break, having been re-pinned with these.
  singular <- expect_error(
    summarize_with_margins(data, k = dplyr::cur_group_id(), .by = region)
  )
  expect_identical(
    conditionMessage(singular),
    paste0(
      "`summarize_with_margins()` does not support:\n",
      "i `cur_group_id()`.\n",
      "i This spelling is reserved inside a Margin summary and is not ",
      "resolved from the calling environment.\n",
      "i These helpers describe one branch-local dplyr grouping or data ",
      "mask, but a margin result combines multiple grouping sets.\n",
      "i Use `grouping_bit()` or `grouping_id()` when identifying margin ",
      "levels."
    )
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
  # More than one refused spelling in one call reads as a list, joined with
  # cli's serial `and` since #223 adopted its vector defaults.
  plural <- expect_error(
    summarize_with_margins(
      data,
      k = dplyr::cur_group_id(),
      j = list(dplyr::cur_group()),
      .by = region
    )
  )
  expect_identical(
    conditionMessage(plural),
    paste0(
      "`summarize_with_margins()` does not support:\n",
      "i `cur_group_id()` and `cur_group()`.\n",
      "i These spellings are reserved inside a Margin summary and are not ",
      "resolved from the calling environment.\n",
      "i These helpers describe one branch-local dplyr grouping or data ",
      "mask, but a margin result combines multiple grouping sets.\n",
      "i Use `grouping_bit()` or `grouping_id()` when identifying margin ",
      "levels."
    )
  )
  # A prohibited context is a Package condition, and parentheses do not turn it
  # into one of R's. Before #178 `(cur_group_id)()` reached the data mask and
  # failed there with `object 'cur_group_id' not found`, which is an untyped
  # condition of the class ADR 0015 separates -- and `(grouping_id)(region)`
  # reached the exported stub, which reports that the helper can only be used
  # inside the verb the caller is already inside.
  parenthesized <- expect_error(
    summarize_with_margins(data, k = (cur_group_id)(), .by = region)
  )
  # Against the singular arm above rather than a pattern of its own, which is
  # the whole of what this asserts: the parenthesized spelling reaches the same
  # refusal, not merely one that opens alike.
  expect_identical(
    conditionMessage(parenthesized),
    conditionMessage(singular)
  )
  expect_s3_class(parenthesized, "marginplyr_error")
  expect_identical(
    summarize_with_margins(
      data,
      k = (grouping_id(region)),
      j = (grouping_bit)(region),
      .grouping = rollup(region),
      .sort = "last"
    ),
    summarize_with_margins(
      data,
      k = grouping_id(region),
      j = grouping_bit(region),
      .grouping = rollup(region),
      .sort = "last"
    )
  )
})

test_that("parentheses do not let a selection reach a grouping column", {
  # The grouping-column exclusion is checked against the call the rewrite
  # recognizes and applied by resolving the selection against a proxy the
  # dimensions are not in, so a writing recognition missed would resolve
  # somewhere else -- or, before #178, fail in the data mask with `object
  # 'pick' not found`. `everything()` is what makes this an assertion rather
  # than a restatement: a selection excluding nothing would take the grouping
  # dimensions with it.
  data <- contextual_probe_data()
  selections <- list(
    across = list(
      probe = function(spell) {
        rlang::expr(summarize_with_margins(
          .probe_data,
          !!spell(list(
            quote(dplyr::everything()),
            quote(sum),
            .names = "{.col}_s"
          )),
          .grouping = rollup(region, grade),
          .sort = "last"
        ))
      },
      selected = function(outcome) names(outcome$value)
    ),
    pick = list(
      probe = function(spell) {
        rlang::expr(summarize_with_margins(
          .probe_data,
          k = ncol(!!spell(list(quote(dplyr::everything())))),
          .grouping = rollup(region, grade),
          .sort = "last"
        ))
      },
      selected = function(outcome) outcome$value$k[[1L]]
    )
  )
  expected <- list(
    across = c("region", "grade", "units_s", "qty_s"),
    pick = 2L
  )
  for (name in names(selections)) {
    selection <- selections[[name]]
    spellings <- contextual_owned_spellings("selection", name)
    for (writing in contextual_writings(spellings)) {
      outcome <- contextual_probe_outcome(selection$probe(writing$spell), data)
      expect_identical(
        selection$selected(outcome),
        expected[[name]],
        info = writing$label
      )
    }
  }
})

test_that("a share `.fns` reference is the helper through parentheses", {
  # The end-to-end half of the reference reading above, and the position whose
  # refusal names what it is refusing: a `.fns` this does not recognize is
  # reported as a formula, an anonymous function, or a function list, none of
  # which `(share_of_total)` is.
  data <- contextual_probe_data()
  share_across <- function(fns) {
    rlang::inject(summarize_with_margins(
      data,
      t = sum(units),
      dplyr::across(t, !!fns, .names = "{.col}_share"),
      .grouping = rollup(region),
      .sort = "last"
    ))
  }
  bare <- share_across(quote(share_of_total))
  expect_identical(share_across(quote((share_of_total))), bare)
  expect_identical(share_across(quote(((share_of_total)))), bare)
  expect_identical(share_across(quote((marginplyr::share_of_total))), bare)
  # And the reference really is read rather than run: a qualifier naming a
  # package that does not own the helper is refused in the same position.
  expect_error(
    share_across(quote((stats::share_of_total))),
    "`across\\(\\)` `.fns` must be"
  )
})

test_that("a pair around an ordinary call changes nothing it evaluates", {
  # The readers unwrap a redundant pair for every node, not only for a
  # recognized one, and the rewrites rebuild each call they descend into from
  # the node the readers gave them -- so a caller's pair does not survive into
  # the staged expression. What has to survive is the value, and the reason it
  # does is that R's parser has already recorded the grouping in the tree: a
  # pair that is doing work is not a redundant pair, and unwrapping the node
  # cannot move an operand from one operator to another.
  data <- contextual_probe_data()
  summarize <- function(expr) {
    rlang::inject(summarize_with_margins(
      data,
      k = !!expr,
      .grouping = rollup(region),
      .sort = "last"
    ))
  }
  expect_identical(
    summarize(quote(sum((units + qty)))),
    summarize(quote(sum(units + qty)))
  )
  expect_identical(summarize(quote((sum(units))))$k, c(3, 7, 10))
  # A pair the parser needed keeps its meaning, and the assertion is that the
  # two groupings still differ: `(units + qty) * 2` is not `units + qty * 2`.
  expect_identical(summarize(quote(sum((units + qty) * 2)))$k, c(24, 40, 64))
  expect_false(identical(
    summarize(quote(sum((units + qty) * 2))),
    summarize(quote(sum(units + qty * 2)))
  ))
})

test_that("parentheses leave an injected quosure its own environment", {
  # A quosure a caller injects carries the environment its expression resolves
  # in, and the rewrites rebuild every call they descend into. Parentheses put
  # one more node between the summary and the quosure, and unwrapping a node
  # means rebuilding it: `rebuild_static_call()` takes the attributes of the
  # node it was given, so unwrapping the pair *around* a quosure would rebuild
  # a bare `~` call and hand dplyr a formula where the caller injected a
  # quosure. That is the identity loss #165 removed, and #178 is where it could
  # have come back.
  data <- contextual_probe_data()
  multiplier <- 10
  injected <- rlang::quo(sum(units) * multiplier)
  expect_identical(
    rlang::inject(summarize_with_margins(
      data,
      k = (!!injected),
      .grouping = rollup(region),
      .sort = "last"
    )),
    rlang::inject(summarize_with_margins(
      data,
      k = !!injected,
      .grouping = rollup(region),
      .sort = "last"
    ))
  )
  # And the environment really was needed: `multiplier` is bound nowhere the
  # summary would find it without the quosure.
  expect_false(exists("multiplier", envir = rlang::ns_env("marginplyr")))
  expect_identical(
    rlang::inject(summarize_with_margins(
      data,
      k = (!!injected),
      .grouping = rollup(region),
      .sort = "last"
    ))$k,
    c(30, 70, 100)
  )
})
