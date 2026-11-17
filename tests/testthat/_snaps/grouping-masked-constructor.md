# the masked-constructor refusal names the call and its cause

    Code
      summarize_with_margins(data, n = dplyr::n(), .grouping = rollup(region))
    Condition
      Error in `summarize_with_margins()`:
      ! `rollup()` in `.grouping` resolves to `stats::rollup()`, not to marginplyr's constructor.
      i Write `marginplyr::rollup()` to build a Grouping specification.
      Caused by error:
      ! object 'region' not found
    Code
      inspect_grouping(data, .grouping = grouping_sets(store, rollup(region)))
    Condition
      Error in `inspect_grouping()`:
      ! `rollup()` in `.grouping` resolves to `stats::rollup()`, not to marginplyr's constructor.
      i Write `marginplyr::rollup()` to build a Grouping specification.
      Caused by error:
      ! object 'region' not found

