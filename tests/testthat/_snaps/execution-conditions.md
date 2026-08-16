# the reported conditions read as they are written

    Code
      cat(conditionMessage(warnings[[1L]]))
    Output
      There were 3 warnings in `dplyr::summarize()`.
      The first warning was:
      i In argument: `total = sum(as.numeric(grade))`.
      i In group 1: `region = "East"`, `grade = "a"`.
      Caused by warning:
      ! NAs introduced by coercion
      i Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
      i 3 further grouping sets raised this warning.

---

    Code
      cat(conditionMessage(error))
    Output
      i In argument: `x = stop("my error")`.
      i In group 1: `g = "a"`.
      Caused by error:
      ! my error

