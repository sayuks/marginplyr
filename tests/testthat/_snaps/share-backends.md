# Arrow rejects Parent shares before constructing a query

    Code
      conditionMessage(error)
    Output
      [1] "Arrow backends do not support Parent shares because marginplyr cannot enforce their scalar-summary contract safely before an Arrow query is constructed. Other Arrow Margin operations remain supported. Omit `share_of_parent()` or explicitly collect the data before calling `summarize_with_margins()`."

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

