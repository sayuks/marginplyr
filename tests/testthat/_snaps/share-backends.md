# Arrow rejects Parent shares before constructing a query

    Code
      conditionMessage(error)
    Output
      [1] "Arrow backends do not support Parent shares because marginplyr cannot enforce their scalar-summary contract safely before an Arrow query is constructed. Other Arrow Margin operations remain supported. Omit `share_of_parent()` or explicitly collect the data before calling `summarize_with_margins()`."

# RSQLite refuses a share its dialect cannot establish

    Code
      conditionMessage(refusal)
    Output
      [1] "marginplyr cannot establish that the source summaries of Parent shares are plain integer or double scalars on this backend, because its SQL dialect converts a value of another type to a number rather than refusing it, so an ineligible source summary is indistinguishable from an eligible one. Set `.check_share_source = FALSE` to calculate `share_of_parent()` from sources you have established yourself, or explicitly collect the data before calling `summarize_with_margins()`."

# a lazy backend that answers nothing refuses to establish a share

    Code
      conditionMessage(refusal)
    Output
      [1] "marginplyr cannot establish that the source summaries of Parent shares are plain integer or double scalars on this backend, because it could not be asked whether its SQL dialect converts a value of another type to a number rather than refusing it, and a dialect that converts rejects nothing. Set `.check_share_source = FALSE` to calculate `share_of_parent()` from sources you have established yourself, or explicitly collect the data before calling `summarize_with_margins()`."

# DuckDB reports an ineligible share source against its summary

    Code
      unique(unlist(regmatches(message, gregexpr("[.][.]marginplyr_[A-Za-z0-9_]+",
        message))))
    Output
      [1] "..marginplyr_denominator_of_lab_1"

# Arrow rejects Total shares before constructing a query

    Code
      conditionMessage(error)
    Output
      [1] "Arrow backends do not support Total shares because marginplyr cannot enforce their scalar-summary contract safely before an Arrow query is constructed. Other Arrow Margin operations remain supported. Omit `share_of_total()` or explicitly collect the data before calling `summarize_with_margins()`."

---

    Code
      conditionMessage(both)
    Output
      [1] "Arrow backends do not support Parent shares and Total shares because marginplyr cannot enforce their scalar-summary contract safely before an Arrow query is constructed. Other Arrow Margin operations remain supported. Omit `share_of_parent()` and `share_of_total()` or explicitly collect the data before calling `summarize_with_margins()`."

