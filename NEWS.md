# marginplyr 0.1.0

* Ordinary first `across()` name-template warnings retain their authored
  context and report once across grouping sets beside pattern-selected shares,
  while preserving callback order and warning-conversion failures (#1553).

* Parent and Total shares use their complete denominator partitions even
  when SQL inputs carry bounded window frames (#1555).

* Local Parent and Total shares keep distinct complex missing grouping keys
  in their own fixed partitions and Parent rows without multiplying rows,
  including retained duplicate grouping sets (#1556).

* Parent and Total shares preserve each grouping set's ordinary input
  selection for the first `across()` source with stable output names,
  including parenthesized per-group selections (#1547).

* Recognized constructor-shaped caller calls use the same faithful selection
  context for bare and qualified spellings, invoking each occurrence once and
  retaining its first answer. Unproven calls and later arguments wait for typed
  metadata; unsupported SQL contexts and unsafe lazy sources refuse before
  caller invocation (#1551).

* Lazy Margin results retain their construction-time schema. Local and dtplyr
  factor processing rejects changed captured levels or ordered status before
  values can be lost, including native typed-missing label paths and empty
  inputs. Rebuild the result after schema changes; same-schema value changes
  retain ordinary backend behavior (#1550).

* Local Parent and Total shares preserve Grouping helpers in the first ordinary
  `across()` selector and name template, including summaries unrelated to the
  share source (#1546).

* Parent and Total shares retain invocation-time dependencies through known
  base closure returns from `structure()`, `unname()`, `withVisible()`,
  `eval()`, `evalq()`, `get0()` and `forceAndCall()` (#1545).

* Parent and Total shares accept eligible columns from parenthesized ordinary
  `across()` sources while preserving their per-group evaluation (#1530).

* Direct SQL Parent and Total share sources retain their `UNNEST()` cardinality
  check when standard-quoted strings or identifiers contain comment markers
  (#1532).

* Typed DuckDB inspection and expansion refuse opaque authored macros and calls
  to `query()` or `query_table()` before binding can infer types from source
  values. Captured names, explicit reader metadata and ordinary table functions
  remain available (#1533).

* Parent and Total shares preserve dplyr expansion of later bare `across()`
  sources, including function factory context and ordinary lambda masks
  (#1529).

* Summaries preserve caller-owned errors and their payloads when their cause
  is a caught Package condition, including invalid nested Margin calls (#1531).
  Generated share-source alias refusals retain their Package condition class.

* Typed DuckDB Grouping inspection follows authored SQL views regardless of
  their names before binding, so ordinary names cannot hide source-value
  inference. Safe nested common table expressions retain typed inspection
  (#1526).

* Included-only dtplyr expansions acquire faithful multi-slot S4 metadata,
  allowing character displays of Period and Interval dimensions without an
  internal prototype error (#1528).

* Parent and Total shares preserve each grain's ordinary first `across()`
  function factories, including stateful sources and Grouping helpers (#1527).

* Typed DuckDB Grouping inspection checks views captured by direct table names
  and qualified identifiers before binding, including derived inputs, so a
  stored reader cannot bypass source-inference admission (#1525).

* Local DuckDB JSON tests prepare and reuse a signed extension cache, and fail
  when required support is unavailable. Review-ready and the local CRAN check
  wrapper prepare the same support before checking (#1540).

* Typed DuckDB Grouping inspection refuses CSV filename replacement scans
  before source-value inference, while preserving captured-name inspection
  and explicit reader metadata. Common table expressions and catalog relations
  with CSV or JSON names retain ordinary typed inspection (#1519).

* Parent and Total shares preserve requested sf summary geometry columns and
  explicit output order without exposing private denominator geometry (#1515).

* Top-level `.grouping` factories preserve caller-owned constructor syntax
  through custom expression capture and quotation aliases, while rebound quotation names
  retain the authored-empty refusal of constructors they execute (#1517).

* Typed DuckDB Grouping inspection refuses JSON type and record inference
  before sampling source values, including JSON file replacement scans, while
  preserving name-only inspection and established reader metadata (#1516).

* Typed dtplyr inspection excludes object columns before numeric admission
  checks, preventing implicit S4 method calls on source values (#1518).

* Both Nest verbs preserve local cell values, fixed partitions, and displayed
  keys under registered `as_tibble.data.frame` converters (#1512).

* Top-level `.grouping` factories preserve authored empty constructor positions
  in evaluated quotation operands, including `bquote()` substitutions and
  `substitute()` environments (#1514).

* Parent and Total share dependency checks follow closure body, formal and
  enclosure replacements through known list and fresh environment members,
  including replacements that remove an earlier dependency (#1513).

* `share_of_parent()` and `share_of_total()` now follow visible closure enclosure replacements when validating dependencies, refusing earlier-summary dependencies while preserving unchanged and proven independent enclosures (#1504).

* Typed DuckDB Grouping inspection refuses CSV auto-detection before binding
  reader SQL, preserving name-only inspection and explicit-schema CSV metadata
  with `auto_detect = false` (#1506).

* `share_of_parent()` and `share_of_total()` now validate dependencies against local closures' replaced bodies and formals, refusing earlier-summary dependencies while preserving proven independent replacements (#1502).

* Top-level `.grouping` factories now preserve quoted language and supplied function bodies, while directly authored empty constructor arguments before splices remain refused (#1505).

* `nest_by_with_margins()` preserves typed empty dtplyr payloads when frame conversion methods are registered (#1503).

* Parent and Total share dependency checks follow unchanged fresh closure
  environments through `invisible()` and local aliases, while preserving
  independent field replacements and dependencies through conditional alias
  rebinding (#1493, #1501).

* dtplyr Parent and Total shares preserve supported dot-dot summary output
  names through lazy construction and data-frame conversion (#1495).

* Parent and Total share dependency checks retain ordinary local closures
  replaced by a definite literal singleton loop (#1492).

* Public `.grouping` capture preserves authored empty constructor arguments
  before empty splices and refuses them with their original position (#1489).

* Parent and Total shares match active geometry fixed keys on local sf
  inputs without requesting a spatial join (#1494).

* Both Nest verbs preserve local cell values under registered
  `as_tibble.list` converters (#1490).

* Parent and Total share dependency checks retain local closures returned by
  `base::as.function()` and by `base::Vectorize()` when the closure has no
  formal arguments, including after non-class attribute writes, and follow
  independent closures returned by caller-defined attribute setters
  (#1491, #1501).

* Parent and Total share dependency checks retain local closures returned by
  `rlang::as_function()` and `compiler::cmpfun()` (#1482).

* SQLite Margin expansions preserve source dimension types when directly
  collecting finite unsorted prefixes with typed-missing labels (#1484).

* DuckDB Grouping context-name selections honor registered tibble selection
  proxy methods for fixed keys, dimensions, and summary grain (#1481).

* Typed DuckDB Grouping inspection refuses dynamic pivots before evaluating
  source rows and checks that metadata preserves captured column names
  (#1461, #1462).

* Grouping context-name selections honor tibble names methods on lazy inputs
  and tidyselect data proxy methods on immutable dtplyr inputs (#1463, #1469).

* The default Grouping duplicate policy can reject large repeated cubes and
  Cartesian products without building every occurrence (#1464).

* `inspect_grouping()` checks Option arguments before normalizing a direct
  Arrow reader (#1465).

* Both Nest verbs preserve dtplyr cell values when a caller supplies an
  `as.data.table.list` method, including at deferred collection (#1466).

* Parent and Total share dependency checks follow closure members through
  selected branches, plain-list remapping and accessors, inline list factories,
  known `do.call()` invocations, singleton loops and fresh environment fields
  (#1470, #1471, #1472, #1473, #1474, #1475, #1476).

* Unnamed local frame summaries check for collisions using the columns they
  expand into the result (#1477).

* Grouping selections preserve dtplyr receiver methods, ordinary tidyselect name repair, and the selection context of explicitly qualified constructor calls (#1439, #1449, #1457).

* `summarize_with_margins()` identifies incompatible unnamed frame fields across grouping sets and explains how to use consistent fields or pack the summary (#1441).

* Parent and Total shares on DuckDB and PostgreSQL check that a direct single-argument SQL `UNNEST()` source returns one value per grouping row (#1442).

* Parent and Total share dependency checks follow closures through nested list writes, replacement dispatch, inline call frames, and literal list indices (#1443, #1444, #1445, #1446).

* Ordinary dtplyr Margin summaries read special-name key columns such as `.N` with their original row values, while `dplyr::n()` still counts rows (#1440).

* The first ordinary `across()` beside a contextual share selects the stored local input columns (#1447).

* Margin label checks compare the character displays emitted by expansion and the stored level strings of factor dimensions (#1450, #1451).

* dtplyr Margin operations preserve generated label checks, omitted labels, empty Grouping set identifiers, and Nest cell membership when a caller overrides namespace lookup (#1452, #1453, #1456).

* SQLite Margin expansions check labels on derived Boolean-word columns and collect unsorted Date displays as text when a text Margin label is requested (#1454, #1455).

* Parent and Total share dependency checks follow literal member paths through nested lists of local closures, including extracted inner lists (#1432).

* Generated dtplyr Margin-label display and factor restoration preserve source dimensions when a caller overrides the namespace operator (#1433).

* Ordinary `across()` avoids false grouping key and identifier collisions when preceding summaries visibly change function or unpack settings (#1428).

* Parent and Total share dependency checks follow existing closures after literal list names and names-attribute replacement, including swapped names (#1431).

* Grouping context-name selections preserve local receiver dispatch, keeping fixed keys, grouping dimensions and summary or nested grains consistent with typed tidyselect (#1434).

* Both Nest verbs preserve dtplyr source keys and cell contents when a caller overrides the namespace operator, including deferred collection (#1427).

* Parent and Total share dependency checks retain local closures in plain lists transferred through base `identity()`, `invisible()` and `force()` (#1430).

* Formula-shaped caller-qualified Grouping selections use faithful typed metadata, preserving fixed keys and avoiding false duplicate grouping sets across inspection and summaries, including immutable dtplyr inputs (#1429).

* Parent and Total share dependency checks retain invocation-time dependencies
  of local literal closures installed by `delayedAssign()` (#1417).

* Parent and Total share dependency checks retain local closures selected by
  unique partial list names through `$` and `[[` with `exact = FALSE` (#1415).

* Ordinary `across()` function expressions respect authored namespace lookup
  when determining output names, avoiding false grouping-column and identifier
  collisions while retaining static share-source eligibility (#1421).

* Grouping selections use faithful typed metadata when a helper operand installs
  a pattern method before matching, preserving fixed keys and dimensions across
  inspection and summaries, including immutable dtplyr inputs (#1420).

* Parent and Total share dependency checks follow surviving closures after
  literal list deletions and single-bracket replacements (#1414).

* Parent and Total share dependency checks retain known mask environments
  returned through blocks, literal branches, numeric switch arms and base
  identity calls (#1416).

* Typed inspection and Nest verbs refuse replaced registered dtplyr converters
  before they can change Grouping dimensions or fixed keys (#1419).

* Immutable dtplyr expansions preserve source rows and payload when a caller
  overrides the namespace operator, including cached inputs (#1422).

* Parent and Total share dependency checks retain local closures through plain
  list concatenation, append and literal subsets (#1413).

* Typed inspection and Nest verbs refuse replaced registered data.table
  brackets before direct, cached or grouped dtplyr roots can lose Grouping
  dimensions or fixed keys (#1418).

* Grouping callbacks that rebind an unproven local function resolve against typed
  metadata, keeping fixed keys and dimensions consistent across inspection and
  summaries, including immutable dtplyr inputs (#1400).

* Authored namespace calls around nested Grouping specifications retain the
  caller's result and captured environment, with one evaluation per argument
  occurrence across inspection and both Nest verbs (#1396).

* Unnamed local data-frame summaries apply compatible frame-subclass casts
  before expanding their columns, preserving converted values (#1399).

* Parent and Total share dependency checks retain known summary-mask
  environments saved in local variables through recognized field reads,
  environment accessors and list conversion (#1404).

* Parent and Total share dependency checks follow closures installed by
  literal list element additions, replacements and dollar-field writes
  (#1403).

* Parent and Total share dependency checks retain local closures transferred
  through `force()`, literal `get()` and `match.fun()` (#1402).

* Parent and Total share dependency checks retain invocation-time dependencies
  through blocks, literal branches and visible local function factories,
  including selected early returns and named `switch()` arms (#1401).

* SQLite expansion preserves Boolean-word public column names and applicable
  source types on executable derived inputs, including default one-set
  expansions (#1398).

* Typed dtplyr Grouping-plan inspection and nesting safely refuse replaced
  registered `head.data.table` methods before they can receive source rows
  (#1397).

* SQLite Margin order reuses public value columns instead of projecting
  duplicate ordering columns, so a single-dimension included-only expansion
  with 1,999 public columns fits the supported result-column limit (#1384).

* Forwarded Grouping selections preserve qualified-call evaluation and captured
  environments across constructors, inspection, and Margin verbs (#1383).

* Parent and Total share dependency checks retain computed mask-member reads,
  unproven environment search bounds, bulk environment reads, and known local
  closure transfers while preserving proven independent reads (#1385, #1386,
  #1388, #1389).

* Typed dtplyr Grouping-plan inspection and nesting safely refuse root table
  subclasses before truncation methods can change metadata or source rows
  (#1382).

* Local list-backed matrix and array dimensions accept representable
  typed-missing Margin labels, including packed dimensions (#1364).

* Margin label collision checks retain accepted logical choices independently
  of class-specific negation methods across all Margin verbs (#1360).

* Unnamed local data-frame summaries expand their actual columns without
  invoking subclass list conversion on evaluated rows (#1365).

* Parent and Total share dependency checks follow locally copied closures,
  literal-list elements and closures returned by literal function factories
  using the bindings present when they are invoked (#1361).

* Parent and Total share dependency checks reject unresolved dynamic binding
  names in mask-reaching `rlang::env_get()` calls, while retaining independent
  external and local non-inheriting reads (#1362).

* Parent and Total share dependency checks recognize equivalent explicit
  environment paths to the summary mask, including nested literal functions
  and local evaluation (#1368).

* Deterministic empty dtplyr projections support expansion and typed
  Grouping-plan inspection without running source operations (#1366).

* Typed dtplyr Grouping-plan inspection and nesting safely refuse caller
  bracket methods that cannot establish faithful column metadata (#1359).

* Grouping selections using standard stringr patterns preserve typed column
  metadata when matching dispatches through vctrs proxy methods (#1363).

* dtplyr Margin order now works when the caller binds `::` in the lazy root
  environment (@LeonidasZhak, #1357).

* dtplyr Margin order preserves missing-last and margins-first/last ordering
  independently of caller bindings for `is.na` and `%in%`, including bindings
  changed after lazy result construction (#1176).

* Zero-column dtplyr expansion and nesting preserve empty-input results when
  the caller binds `rep`, including after lazy result construction (#1171).

* Unnamed top-level `across()` summaries preserve populated `...` forwarding
  while retaining dplyr's function-factory evaluation timing (#1151).

* Ordinary local `across()`, `if_any()`, and `if_all()` selectors preserve
  populated `...` and `..1` forwarding through inline functions (#1138).

* Both dtplyr nesting verbs preserve displayed grouping keys independently of
  caller bindings for `min` and `seq_len`, including bindings changed after
  lazy result construction (#1089).

* Ordinary local summary selections resolve function arguments and local
  bindings at execution while retaining the environments of injected
  `quosures` (#1070).

* Supported Arrow summaries preserve caller warnings containing the absorption
  marker, while genuine absorption is still refused before reading input rows
  (#1072).

* dtplyr Margin verbs preserve `.SD` dimensions during opt-in observed Margin
  label collision checks and report collisions under the caller's column names
  (#1046).

* dtplyr Parent and Total shares preserve `.ROW` share outputs and Grouping set
  identifiers, including `across()` output names (#1048).

* Qualified constructor-shaped Grouping selections retain the caller's first
  answer and conditions with one invocation per argument and public call
  (#1038).

* Qualified selections resolved by caller namespace operators use typed
  metadata for fixed keys and Grouping dimensions, including calls inside
  selection helper arguments (#1039).

* dtplyr summaries with shares keep lexical `.BY` bindings separate from input
  measures and refuse expressions whose two readings cannot be separated safely
  (#1033).

* SQL Server expansions omit unbounded input ordering inside source relations
  while preserving limited row selection and usable window ordering (#994).

* Both share helpers preserve dtplyr fixed keys named `.`, `..0`, `..00`,
  and `..01` through cardinality validation and denominator grouping (#999).

* Generated local and dtplyr column reads protect admitted signed and
  whitespace dot-dot names, including fixed keys and share sources (#1002).

* Expansions keep active sf geometry keys and dimensions among the leading
  Margin columns, including with Margin ordering (#993).

* Portable summaries restore active sf geometry fixed keys through the
  subclass rename method, keeping retained geometry metadata valid (#1000).

* Portable summaries retain dynamic output names `.`, `.da`, and `.dat`
  without matching the backend `.data` argument (#970).

* SQL summaries preserve accepted dot-dot output and Grouping set identifier
  names through Margin ordering, collection, and materialization (#971).

* SQLite expansions compose wide Grouping plans through bounded compounds,
  including zero-row type anchors, so a nine-dimension cube can be collected
  without exceeding the compound SELECT term limit (#980).

* Both nesting Margin verbs preserve list-column names such as `...` and `..1`
  across grouping sets, identifiers, and Margin ordering (#950).

* Margin operations isolate immutable dtplyr inputs at execution, including
  row-preserving filters and cached steps. Nesting also accepts `.SDall` and
  `.ROW` columns (#959).

* Filtered dtplyr inputs retain `.SD` columns in typed Grouping selections
  and both nesting verbs, preserving column names and cell values (#947).

* SQL Margin label collision checks now accept an `.env` grouping dimension
  and report actual observed collisions with the public column name (#930).

* SQLite expansions now directly materialize a Grouping set identifier named
  `.data`, preserving its integer values, public columns, and Margin order
  without needing an unrelated `.env` column (#863).

* SQLite expansions retain unchanged source-column types through direct
  collection and materialization without `.id`, including all-missing fixed
  keys and payloads in one-set and unsorted results (#862).

* Captured Grouping helpers retain their literal syntax in qualified
  `base::bquote()`, `rlang::expr()`, and `rlang::quo()` calls. Explicit `bquote()`
  substitutions still evaluate; unsupported splicing is refused (#832).

* Local higher-dimensional array grouping dimensions now refuse scalar display
  labels with an actionable Package condition on both populated and empty
  input. Typed-missing labels preserve their shape (#834).

* DuckDB `TIME`, `INTERVAL`, and `BLOB` dimensions now expand with
  typed-missing Margin labels, preserving values and SQL types through
  collection and materialization (#836).

* Local summaries retain dplyr-compatible output names such as `...` and
  `..1` across grouping sets. Dot-dot `.id` names also work with Margin
  ordering and nesting (#827).

* Generated Arrow Margin label transformations that would automatically
  collect the input are now refused before collection in summaries and
  expansions. See *Arrow inputs* in `?summarize_with_margins` (#783).

* `last_sent_queries(include_result = FALSE)` selects the latest call's
  internal SQL while preserving the default two-column record (#790).

* `last_sent_queries(details = TRUE)` adds the SQL `source`, internal-operation
  `status`, and ordered `conditions` summaries. These describe observed outcomes
  at the package boundary, including handled errors; they establish neither
  whole-call success nor server completion (#791). The query-policy help also
  distinguishes DuckDB's catalog-only admission from its typed snapshot, and
  the accessor example clarifies that restoring the audit option leaves the
  saved call's record readable (#1539).

* SQLite Parent shares now match equivalent grouping keys under `NOCASE` and
  `RTRIM` collations, including when children retain different spellings of
  their Parent key (#767).
* Arrow partition fields used as Grouping dimensions now retain their Margin
  labels in `expand_with_margins()`, including with `.id` or `.sort`, through
  direct collection and materialization (#750).
* Portable SQL summaries containing only scalar constants now return one
  Grand total row per grouping-set occurrence, including on empty input and
  with `grouping_bit()` or `grouping_id()` (#602).
* Initial CRAN submission.
* Added `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, and
  `grouping_spec()` for arbitrary SQL-style Grouping plans, including empty
  sets, nesting, Cartesian products, and composite dimensions.
* A nested argument a Grouping specification constructor cannot read is refused
  with marginplyr's own diagnostic naming the spelling that works, in place of
  a tidyselect error about a call you never wrote. This reaches a specification
  a function of your own returned, a name that a column and a nested
  specification both claim, an empty argument, and a name written on the
  constructor's own argument (#265, #365). See *How an argument is read* in
  `?grouping_set`.
* Added contextual `grouping_bit()` and `grouping_id()` summary helpers.
  `grouping_id()` written with no columns reads every `.grouping` column of the
  resolved plan, in plan order, so widening a Grouping specification widens the
  mask instead of leaving it at its old width (#366). See `?grouping_bit`.
* Added the contextual `share_of_parent()` summary helper, which divides a
  preceding numeric scalar summary by the same measure one `rollup()` level up,
  partitioned by the fixed `.by` keys. Local data frames, dbplyr, and dtplyr
  are supported and lazy inputs stay lazy; a source marginplyr cannot establish
  as numeric is refused, and `.check_share_source = FALSE` opts out
  (#195, #196, #198). See *Eligible source summaries* in `?share_of_parent`.
* Added the contextual `share_of_total()` summary helper, which divides the
  same kind of source summary by the Grand total set within each fixed `.by`
  partition, and so accepts any Grouping plan that contains one.
* Each of those four Contextual helpers takes a bare name and accepts one
  forwarded by injection, so a function of your own can pass either
  `!!rlang::enquo(col)` or `!!rlang::ensym(col)` (#169).
* Added `inspect_grouping()` for reading the resolved Grouping plan as an
  ordinary local tibble, without executing a Margin operation. Its `.format`
  argument gives `"text"` for compact display values or `"list"` for the exact
  character vectors to program against. See *Formats and ordinary tibble
  behavior* in `?inspect_grouping`.
* Added `.id` to every Margin verb for one-based grouping set occurrence
  identifiers, including duplicate-aware local, lazy, expansion, and nesting
  paths.
* Added `.sort` to every Margin verb for an opt-in Margin order, taking
  `"none"` (the default), `"last"`, or `"first"`. It orders a result by the
  structure of its Grouping plan rather than by displayed values, so a subtotal
  sits with the rows it summarizes rather than wherever its Margin label falls,
  and lazy inputs stay lazy. See *Margin order* in `?summarize_with_margins`.
* Added the `retail_sales` data set, 24 rows of synthetic monthly sales.
  See `?retail_sales`.
* Added guides for Grouping identity and explicit key completion, and made the
  function references the canonical source of the Margin, Parent-share, and
  Margin-label contracts.
* Added explicit duplicate-set policies: `"error"`, `"drop"`, and `"keep"`.
* An argument documented as taking one of a listed set of strings —
  `.duplicates`, `.margin_label_position`, and `.sort` on the Margin verbs, and
  `.format` on `inspect_grouping()` — takes one of those strings spelled in
  full, and refuses an abbreviation and `NULL` alike. Passing the whole
  vocabulary as the signature spells it asks for the default, which is what
  lets a wrapper repeat the signature and forward the argument it was given
  (#110, #144, #210). See *Option arguments* in `?summarize_with_margins`.
* Changed the default display label to `"Total"`; `.margin_label = NULL`
  preserves grouping-column types and typed missing values.
* `.margin_label` also takes a per-dimension spelling: a named character
  vector labels each resolved Margin dimension separately, and a named list
  does the same while letting one dimension take `NULL` where another takes
  a label (#16, #371). See *Display labels and grouping identity* in
  `?summarize_with_margins`.
* `.check_margin_label` controls only the half of the Margin label collision
  check that reads your data, so it defaults to `TRUE` for local data frames
  and `FALSE` for lazy inputs. Where marginplyr holds factor-level metadata, a
  label equal to a declared factor level is rejected whatever this argument
  says. Arrow does not retain or restore those levels: it returns a factor
  dimension with a non-missing Margin label as character, permits an unused
  declared level, and refuses an observed collision only when
  `.check_margin_label = TRUE` (#122, #492).
* Added `.margin_label_position` to every Margin verb, taking `"last"` (the
  default) or `"first"`. It decides where a non-missing Margin label's
  synthetic level sits in a factor dimension's level order, and is a no-op for
  a typed-missing Margin label (#16). See *Display labels and grouping
  identity* in `?summarize_with_margins`.
* Added `.check_share_source` to `summarize_with_margins()`, `TRUE` by default
  on every backend including lazy ones, because a share source's eligibility
  can be established without reading your data. See *When marginplyr queries
  your data* in `?summarize_with_margins`.
* DuckDB and PostgreSQL use native `GROUPING SETS`; other backends use the
  portable `UNION ALL` adapter.
* `summarize_with_margins()`, `summarise_with_margins()`,
  `expand_with_margins()`,
  `nest_with_margins()`, and `nest_by_with_margins()` now share one normalized
  Grouping-plan implementation.
* Existing `dplyr::group_by()` columns act as implicit fixed `.by` keys across
  local and lazy backends. Grouped input cannot also supply `.by`; margin
  summaries, row expansions, and regular nests return ungrouped results, while
  `nest_by_with_margins()` returns a row-wise result.
* Summary column selection now excludes every fixed key and grouping
  dimension consistently across grouping-set branches. Summary results cannot
  overwrite grouping keys, always return ungrouped output, and reject
  branch-local `cur_group*()` helpers in favor of `grouping_bit()` and
  `grouping_id()`.
* Summary expressions now resolve every Contextual helper by spelling, so
  `grouping_bit()`, `grouping_id()`, `share_of_parent()`, `share_of_total()`,
  `across()`, `if_any()`, `if_all()`, `pick()`, `where()`, and the rejected
  `cur_group*()` helpers mean what marginplyr rewrites them into, and a binding
  of the same name in the calling environment never changes what the verb does
  with one. This changed three resolutions that previously ran a caller's
  binding: `across()`, `if_any()`, and `if_all()`; a shadowed `pick()` inside a
  lambda; and a `where()` qualified with a package that does not own it. See
  *Relationship to dplyr summaries* in `?summarize_with_margins`.
* An unnamed summary now takes its column name from the expression you wrote
  rather than from the one marginplyr rewrote it into, identically on every
  backend, so `sum(v) + grouping_bit(a)` and `nrow(pick(v, w))` no longer name
  a column after a SQL literal or a rewritten selection. A local input goes on
  expanding a data-frame value's columns into the result under that naming, and
  a name of your own still packs them (#430, #435).
* A data-frame-valued summary you gave a name to is no longer read for the
  names it packs, because dplyr packs a named data-frame result into the one
  column that name gives. The unnamed forms are unpacked to the top level and
  stay refused for a collision, as is a named one colliding by its own name.
* Dynamically named data-frame summaries now reserve collision-free internal
  grouping names, and opaque collisions fail with a targeted diagnostic.
  Lazy Margin-label checks use portable numeric `CASE` aggregates across
  supported SQL dialects.
* Backend detection now validates the documented Arrow and dtplyr minimum
  versions, centralizes backend capabilities, and reports incompatible dbplyr
  query representations explicitly.
* A summary Arrow's own engine cannot evaluate is now refused before any row is
  read, naming the two rewrites that compute it, in place of Arrow reading the
  whole input to answer it in R. Through arrow 16.0.0 such a call returned the
  right answer, so on those versions this is a breaking change; from arrow
  17.0.0 it aborted with an error carrying no class of its own and naming
  nothing you wrote. See
  *Summaries a lazy backend cannot carry* in `?summarize_with_margins`.
* A dtplyr input built with `dtplyr::lazy_dt(immutable = FALSE)` is now refused
  before any branch is built, because such a step lets data.table write every
  grouping-set branch back to your own table by reference. The refusal names
  the one rewrite, `dtplyr::lazy_dt(immutable = TRUE)`, which is what
  `lazy_dt()` does by default. See *Other lazy backends* in
  `vignette("database_backends")`.
* A `.grouping` or `.by` selection whose failure the column names already
  settle is now refused without querying your input, so a disconnected or slow
  connection no longer hands you its own failure in place of the column
  diagnostic. This reaches the set-difference operator `/`, the arithmetic and
  scalar-boolean spellings `*`, `^`, `&&`, and `||`, `one_of()`, a formula, and
  the `.data` pronoun; a selection carrying `where()` still resolves against
  your input's column types.
* Every error marginplyr raises for a correctable call now inherits the
  `"marginplyr_error"` class, so `tryCatch(marginplyr_error = )` catches them
  all, and the columns, values, and arguments such an error quotes are spelled
  as you spelled them. Errors from your summary expressions, tidyselect, dplyr,
  or a backend keep their original class, diagnostic, and cause, and so do
  internal invariant checks. See *Errors and warnings* in `?marginplyr`.
* An argument you leave empty is now answered by name, in place of R's own
  missing-argument error: a summary written `z = ` is refused as an empty
  summary named `z`, an unnamed one by its position, and an empty `.by` selects
  no columns as it does in dplyr (#340). An empty argument one level further
  down — inside a selection, as in `.by = c(, region)` — is read as tidyselect
  reads one (#351).
* A selection a wrapper forwards into `across()` is now the selection dplyr
  makes of it, so `across({{ cols }}, sum)` works instead of being refused with
  advice to wrap it in `where()`, and a forwarded omission selects no columns
  at both the standalone and nested positions. An `across()` `.names` template
  that cannot be evaluated now reaches dplyr, which names the argument you
  wrote (#349, #350).
* A selection or a source your wrapper forwards into a share now reaches the
  share it names. `across({{ cols }}, share_of_total)` and
  `share_of_total({{ col }})` — with the `!!rlang::enquo()` and
  `!!rlang::ensym()` spellings of each — reported `object 'cols' not found`,
  naming your own wrapper's argument in place of the summary you wrote (#357).
* A Margin verb piped into another one now names itself when it refuses.
  `f(x) |> g()` runs `f` while `g` reads its input, and `g` reported every
  marginplyr error raised there against its own call (#455).
* A condition raised while your summary expression runs now reports its context
  in names you can act on: grouping values under the columns you wrote rather
  than internal ones, and an error blaming the Margin verb you called. A
  warning every grouping set raises is reported once, saying how many further
  sets raised it, in place of one identical warning per set (#141, #108, #411,
  #432). See *Errors and warnings* in `?marginplyr`.
* Added `.key` to `nest_with_margins()` and `nest_by_with_margins()`, a string
  naming the list column. Each follows the function it resembles for `NULL`:
  `nest_with_margins()` reads it as `"data"`, as `tidyr::nest()` does, and
  `nest_by_with_margins()` refuses it, as `dplyr::nest_by()` does. See `.key`
  in `?nest_with_margins` and `?nest_by_with_margins`.
* `nest_with_margins()` and `nest_by_with_margins()` now use collision-free
  internal columns, and reject duplicate sets with `.duplicates = "keep"`
  because their visible outer keys would be indistinguishable.
* `.keep = TRUE` nests the original, pre-margin values of the fixed `.by`
  columns and grouping dimensions inside each nested data frame, so an outer
  `East / Total` subtotal keeps the store values it stands for while its outer
  keys go on showing the Margin label. See *Relationship to tidyr and dplyr* in
  `?nest_with_margins`.
* A nesting that leaves no payload column now nests one inner row per source
  row, on detail groups, subtotals, and the Grand total set alike, as
  `dplyr::nest_by()` does, and local and `dtplyr` results agree once collected.
  An input with rows and no columns is outside that agreement, because a
  `data.table` reads its row count from its first column and so cannot carry
  those rows in at all.
* Attaching a Grouping set identifier to a `dtplyr` input with no columns no
  longer invents a row, which `expand_with_margins(.id = )` and both nesting
  verbs used to report. Such an input now expands to the row count the local
  backend gives it, and the lazy path stays lazy (#184).
* A `data.frame` subclass whose `[` is not column selection — a raw
  `data.table` is the case — now reaches every Margin verb that accepts local
  data frames, and is not modified by reference. The result's class still
  follows the dplyr verb the Margin verb ends in, and is not promised to be the
  input's.
* Added `last_sent_queries()` and the `marginplyr.audit_sql` option, off by
  default. With the option set to `TRUE`, a Margin verb against a SQL backend
  records the queries it sends — the `"result"` query it hands you unexecuted,
  and the ones it sends for its own reasons on the way — and the accessor reads
  the most recent call's record back as a two-column tibble of `purpose` and
  `sql`. Piping one Margin verb into another is two calls, and the record you
  read is the outer one's alone (#318, #400, #401, #409, #455). See *What the
  record promises* in `?last_sent_queries`.
