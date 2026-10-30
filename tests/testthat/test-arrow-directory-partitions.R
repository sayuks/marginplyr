test_that("hidden Directory fields cannot replace occurrence identifiers", {
  skip_if_suggest_absent("arrow")
  path <- tempfile("margin-directory-")
  on.exit(unlink(path, recursive = TRUE))
  dir.create(file.path(path, "7"), recursive = TRUE)
  arrow::write_parquet(data.frame(g = 9L), file.path(path, "7", "data.parquet"))
  source <- arrow::open_dataset(
    path, schema = arrow::schema(g = arrow::int32()),
    partitioning = arrow::DirectoryPartitioning$create(
      arrow::schema(sid = arrow::int32())
    )
  )
  query <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
  )
  expect_s3_class(query, "arrow_dplyr_query")
  actual <- dplyr::collect(query)
  expected <- data.frame(g = c(9L, NA_integer_), sid = 1:2)
  expect_identical(arrow_partition_bag(actual), arrow_partition_bag(expected))
})

test_that("Directory schemas preserve values, labels, occurrences and order", {
  skip_if_suggest_absent("arrow")
  scenarios <- list(
    public = list(partition = "sid", id = "sid"),
    dimension = list(partition = "..marginplyr_arrow_dimension_1", id = "sid"),
    staged = list(partition = "..marginplyr_arrow_id_1", id = "..1"),
    sorting = list(partition = "..marginplyr_sort_1", id = NULL),
    dimension_suffix = list(partition = "..marginplyr_arrow_dimension_1_",
                            collision = "..marginplyr_arrow_dimension_1",
                            id = "sid"),
    staged_suffix = list(partition = "..marginplyr_arrow_id_1_",
                         collision = "..marginplyr_arrow_id_1", id = "..1"),
    sorting_suffix = list(partition = "..marginplyr_sort_1_",
                          collision = "..marginplyr_sort_1", id = NULL),
    unrelated = list(partition = "unrelated", id = "sid"),
    boundary = list(partition = "..marginplyr_arrow_boundary_1", id = "sid"),
    boundary_suffix = list(partition = "..marginplyr_arrow_boundary_1_",
                           collision = "..marginplyr_arrow_boundary_1",
                           id = "sid")
  )
  for (scenario in scenarios) {
    for (empty in c(FALSE, TRUE)) {
      fixture <- arrow_directory_fixture(
        scenario$partition, collision = scenario$collision, empty = empty
      )
      for (source in fixture$sources) {
        expect_directory_expansion(source, scenario$id)
      }
      unlink(fixture$path, recursive = TRUE)
    }
  }
})

test_that("Directory expansion agrees with successful Arrow controls", {
  skip_if_suggest_absent("arrow")
  fixture <- arrow_directory_fixture("sid")
  hive <- arrow_hidden_partition_fixture("sid")
  on.exit(unlink(c(fixture$path, hive$path), recursive = TRUE))
  input <- fixture$sources$explicit
  sources <- list(
    projected = dplyr::select(fixture$full, dplyr::all_of(fixture$public)),
    materialized = dplyr::compute(input),
    table = arrow::Table$create(dplyr::collect(input)),
    arranged = dplyr::arrange(input, dplyr::desc(row_id)),
    joined = dplyr::left_join(input, arrow::arrow_table(row_id = 1:2),
                              by = "row_id"),
    hive = arrow_narrow_partition_sources(hive, "sid")$explicit
  )
  for (source in sources) expect_directory_expansion(source, "sid")
  ordinary <- dplyr::union_all(
    dplyr::mutate(input, sid = 1L),
    dplyr::mutate(input, g = NA_integer_, sid = 2L)
  )
  expected <- expand_with_margins(
    dplyr::collect(input), .grouping = rollup(g), .margin_label = NULL,
    .id = "sid"
  )
  ordinary <- dplyr::collect(ordinary)[names(expected)]
  expect_identical(arrow_partition_bag(ordinary),
                   arrow_partition_bag(expected))
  query <- expand_with_margins(input, .grouping = rollup(g),
                               .margin_label = NULL, .id = "sid")
  expect_identical(arrow_partition_bag(dplyr::collect(dplyr::compute(query))),
                   arrow_partition_bag(expected))
})

test_that("Directory isolation retains nested payload types and values", {
  skip_if_suggest_absent("arrow")
  path <- tempfile("margin-directory-nested-")
  on.exit(unlink(path, recursive = TRUE))
  dir.create(file.path(path, "7"), recursive = TRUE)
  type <- arrow::fixed_size_list_of(arrow::int32(), 2L)
  table <- arrow::Table$create(
    g = c(9L, 8L),
    payload = arrow::Array$create(list(1:2, 3:4), type = type)
  )
  arrow::write_parquet(table, file.path(path, "7", "facts.parquet"))
  source <- arrow::open_dataset(
    path, schema = table$schema,
    partitioning = arrow::DirectoryPartitioning$create(
      arrow::schema(sid = arrow::int32())
    )
  )
  query <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL, .id = "sid",
    .sort = "last"
  )
  expect_true(arrow::schema(query)$GetFieldByName("payload")$type$Equals(type))
  actual <- dplyr::collect(query)
  expect_identical(actual$g, c(8L, 9L, NA_integer_, NA_integer_))
  expect_identical(actual$sid, c(1L, 1L, 2L, 2L))
  for (id in 1:2) {
    payload <- actual$payload[actual$sid == id]
    expect_equal(sort(vapply(payload, sum, integer(1))), c(3L, 7L))
  }
})
