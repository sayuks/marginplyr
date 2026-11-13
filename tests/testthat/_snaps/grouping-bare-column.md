# the bare-column refusal names the call and its cause

    Code
      summarize_with_margins(data, n = dplyr::n(), .grouping = region)
    Condition
      Error in `summarize_with_margins()`:
      ! `.grouping` must be created with `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`.
      i Column `region` must be wrapped in a constructor, for example `rollup(region)` or `grouping_sets(region)`.
      Caused by error:
      ! object 'region' not found
    Code
      inspect_grouping(data, .grouping = c(region, store))
    Condition
      Error in `inspect_grouping()`:
      ! `.grouping` must be created with `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`.
      i Columns `region` and `store` must be wrapped in a constructor, for example `rollup(region, store)` or `grouping_sets(region, store)`.
      Caused by error:
      ! object 'region' not found

