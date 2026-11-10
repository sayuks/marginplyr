# Variable labels on native summary outputs

Investigated: 2026-11-10

## Measurement

The probe compared native local dplyr, dtplyr and Arrow Table summaries, without
marginplyr. Each case used a fresh tibble with numeric `x` carrying
`label = "Source"`; grouped cases had two groups. Versions were R 4.6.1,
dplyr 1.2.1, vctrs 0.7.3, tibble 3.3.1, dtplyr 1.3.3,
data.table 1.18.6.1 and Arrow 25.0.1.

| Input | Grouped | `x = mean(x)` label | `avg = mean(x)` label |
| --- | --- | --- | --- |
| Local tibble | No | NULL | NULL |
| Local tibble | Yes | NULL | NULL |
| dtplyr | No | NULL | NULL |
| dtplyr | Yes | Source | Source |
| Arrow Table | No | Source | NULL |
| Arrow Table | Yes | Source | NULL |

All three retained the ordinary grouping key's `label = "Group"` in grouped
results. `mean(x)` by itself returned no label. Thus a scalar expression's
attributes did not determine the final summary column's metadata by themselves;
the native backend could attach source metadata too.

## Reproduction

The following reproduces one row. Vary `backend`, `grouped` and `output_name`
over the values in the table, creating a fresh input each time.

```r
backend <- "arrow" # "local", "dtplyr", "arrow"
grouped <- TRUE
output_name <- "avg" # "x", "avg"
df <- tibble::tibble(
  g = structure(c("a", "a", "b", "b"), label = "Group"),
  x = structure(c(1, 3, 5, 7), label = "Source")
)
src <- switch(backend,
  local = df,
  dtplyr = dtplyr::lazy_dt(df),
  arrow = arrow::Table$create(df)
)
if (grouped) src <- dplyr::group_by(src, g)
result <- dplyr::collect(dplyr::summarise(
  src, !!output_name := mean(x), .groups = "drop"
))
attr(result[[output_name]], "label", exact = TRUE)
```

## Interpretation and sources

These were versioned observations, not a guarantee that all calculated columns
retained or discarded their attributes. They supported delegating summary
evaluation and its metadata to the corresponding native backend, rather than
assigning one cross-backend metadata rule to all arithmetic outputs.

[dplyr's reference](https://dplyr.tidyverse.org/reference/summarise.html)
described creating a new data frame and not preserving its frame attributes;
that statement concerned the frame, rather than a blanket rule about every
column attribute. [dtplyr's reference](https://dtplyr.tidyverse.org/reference/summarise.dtplyr_step.html)
described translating the method into data.table's `j` argument.
[Arrow's guide](https://arrow.apache.org/docs/r/articles/data_wrangling.html)
described its native lazy dplyr interface. The live Arrow guide identified
26.0.1 when retrieved; the empirical table above measured installed 25.0.1.
