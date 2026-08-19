# Parent-share sources are numeric scalar summaries

    Code
      conditionMessage(cardinality_error)
    Output
      [1] "Parent share `share` requires source summary `total` to return exactly one value per grouping row.\ni Define `total` as one scalar summary; for multiple statistics, create separate named summaries and a Parent share for each one."

# Parent-share across classifies source-name failures

    Code
      conditionMessage(duplicate_error)
    Output
      [1] "`across()` can't select source summary `total` for `share_of_parent()` because summary `total` was defined more than once.\ni Define it once with a complete ordinary summary expression, then select that unique preceding summary by name."

---

    Code
      conditionMessage(unavailable_error)
    Output
      [1] "`across()` can't select source summary `hidden` for `share_of_parent()` because summary `hidden` is not available as a unique, preceding, self-contained ordinary summary.\ni Define it as a top-level named summary or a statically named output from a preceding `across()`.\ni Select only eligible preceding ordinary summaries by name."

---

    Code
      conditionMessage(unknown_error)
    Output
      [1] "`across()` refers to unknown summary `missing` for `share_of_parent()`.\ni Select only eligible preceding ordinary summaries by name, such as `total`."

---

    Code
      conditionMessage(predicate_error)
    Output
      [1] "Parent-share `across()` only supports name-based tidyselect.\ni Replace `where()` or another type/value predicate with explicit summary names."

# Total-share diagnostics name the helper the caller wrote

    Code
      conditionMessage(cardinality_error)
    Output
      [1] "Total share `whole` requires source summary `total` to return exactly one value per grouping row.\ni Define `total` as one scalar summary; for multiple statistics, create separate named summaries and a Total share for each one."

---

    Code
      conditionMessage(predicate_error)
    Output
      [1] "Total-share `across()` only supports name-based tidyselect.\ni Replace `where()` or another type/value predicate with explicit summary names."

---

    Code
      conditionMessage(unknown_error)
    Output
      [1] "`across()` refers to unknown summary `missing` for `share_of_total()`.\ni Select only eligible preceding ordinary summaries by name, such as `total`."

