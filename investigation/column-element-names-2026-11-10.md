# Column-vector element names across native operations

Investigated: 2026-11-10

## Scope and environment

The investigation concerned `names(data[[column]])`, not data-frame column
names or row names. It ran against package source at
`aec8d8573bfe9d83c776465c6766671abb865c95` with R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, data.table 1.18.6.1, dtplyr 1.3.3,
Arrow 25.0.1, dbplyr 2.6.0 and RSQLite 3.53.3.

The [R probe](column-element-names-2026-11-10.R) reproduced representative
native operations and lazy-input boundaries. Its [captured output](column-element-names-2026-11-10.txt)
recorded values, element names and metadata warnings. It did not execute a
package test suite.

## Input construction

A normal tibble retained named columns:

```r
source <- tibble::tibble(
  g = c(k1 = "A", k2 = "A", k3 = "B"),
  x = c(r1 = 10, r2 = 20, r3 = 30)
)
```

The plain data.frame probe used `structure()` over these columns and verified
their names before each operation. The direct data.table probe attached names
with `data.table::setattr()` after construction. This avoided confusing input
conversion with losses during the operation under study.

## Representative local observations

| Native operation | tibble | Plain data.frame | Direct data.table |
| --- | --- | --- | --- |
| `summarise(first = x[1], total = sum(x), .by = g)` | Key names `k1,k3`; first names `r1,r3`; total unnamed | All three unnamed | All three unnamed |
| `tidyr::nest(.by = g)`, outer key | `k1,k3` | `k1,k3` | Unnamed |
| The first nested cell's `x` | `r1,r2` | `r1,r2` | Unnamed |

For the tibble, mutation replacing `g` with unnamed `"Total"` or
`NA_character_` removed its element names while retaining `x`'s names.
Native descending ordering of `x` reordered its names to `r3,r2,r1` and the
key's names to `k3,k2,k1`.

Repeated, empty-string and missing names were accepted by the representative
native operations. Acceptance did not imply literal preservation: missing
names on the summary key stayed missing, while names on `x[1]` became empty
strings in the measured summary.

## Lazy boundaries and Arrow observations

`dtplyr::lazy_dt(source)` already held unnamed columns in its parent table.
SQLite collection after `copy_to()` also returned unnamed columns.

Arrow Table construction retained the original names in the column attributes
under its schema's R metadata. Native results demonstrated that metadata
retention did not imply correspondence with the source elements:

| Arrow native operation | Observed values of `x` | Observed element names |
| --- | --- | --- |
| `arrange(desc(x))` then collect | `30,20,10` | `r1,r2,r3`, still in original order |
| `filter(x >= 20)` then collect | `20,30` | `NULL`; metadata restoration warned |
| `union_all()` of original and display-replaced branches | Six original values in two branches | `r1,r2,r3,NA,NA,NA` |

The union's branch row order was not asserted. The captured run emitted two
`Invalid metadata$[["r"]]` warnings. These observations concerned Arrow
25.0.1; they were not claims about every future Arrow version.

The measured metadata reader obtained names from the existing schema without
reading source rows. Raw metadata capture was therefore possible, but that
observation did not establish a mechanism for tracking names through row
filtering or reordering.

## Primary-source reading

- R's [names help](https://stat.ethz.ch/R-manual/R-devel/library/base/html/names.html)
  described empty and missing names; the
  [data.frame help](https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.frame.html)
  described removing names from ordinary vector inputs not protected by `I()`.
- [vctrs concatenation](https://vctrs.r-lib.org/reference/vec_c.html)
  described retaining names when inputs carried them. Its
  [proxy/restore contract](https://vctrs.r-lib.org/reference/vec_proxy.html)
  assumed the restored value already had correct names, rather than promising
  to reconstruct them from an original template.
- Arrow's [Schema R metadata documentation](https://arrow.apache.org/docs/r/reference/Schema-class.html)
  described round-trip restoration of R attributes. The page read for this
  investigation identified Arrow 26.0.1, while the probes used 25.0.1.
  A guarantee that names tracked source elements through filtering or ordering
  was not found on that page.

## Limits

The observations were representative, not an exhaustive backend or class
matrix. Arrow Dataset, arbitrary frame subclasses, mutable external-source
updates and every empty-result or materialization path were not investigated.
The research distinguished native attribute behavior from a guarantee of row
identity; it did not establish such a guarantee.
