# every margin verb refuses a mutable dtplyr step

    Code
      expand_with_margins(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE),
      .grouping = rollup(region))
    Condition
      Error in `expand_with_margins()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

