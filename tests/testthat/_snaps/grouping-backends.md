# the refusal reads as it is written, for every verb

    Code
      verb(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE), NULL, rollup(
        region))
    Condition
      Error in `summarize_with_margins()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

---

    Code
      verb(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE), NULL, rollup(
        region))
    Condition
      Error in `summarise_with_margins()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

---

    Code
      verb(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE), NULL, rollup(
        region))
    Condition
      Error in `expand_with_margins()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

---

    Code
      verb(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE), NULL, rollup(
        region))
    Condition
      Error in `nest_with_margins()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

---

    Code
      verb(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE), NULL, rollup(
        region))
    Condition
      Error in `nest_by_with_margins()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

---

    Code
      verb(dtplyr::lazy_dt(mutable_step_data(), immutable = FALSE), NULL, rollup(
        region))
    Condition
      Error in `inspect_grouping()`:
      ! `.data` comes from `dtplyr::lazy_dt(immutable = FALSE)`.
      i A margin verb builds one branch per grouping set from the same step, and data.table writes each branch to your table by reference.
      i Rebuild the input with `dtplyr::lazy_dt(immutable = TRUE)`.

