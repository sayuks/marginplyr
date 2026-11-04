# Mark the prepared input below a collapse so branch projections can change
# their own query without changing the relation shared by all occurrences.
mark_arrow_expansion_input <- function(input, marker) {
  attr(input, "marginplyr_arrow_expansion_marker") <- marker
  dplyr::collapse(input)
}

# Replace Arrow query nodes without descending into a substituted relation.
# A replacement returns NULL for any node it leaves to the traversal.
rewrite_arrow_expansion_sources <- function(query, replacement) {
  if (!inherits(query, "arrow_dplyr_query")) {
    return(query)
  }
  new_source <- replacement(query)
  if (!is.null(new_source)) {
    return(new_source)
  }
  query$.data <- rewrite_arrow_expansion_sources(query$.data, replacement)
  for (node in c("join", "union_all")) {
    if (!is.null(query[[node]])) {
      query[[node]]$right_data <- rewrite_arrow_expansion_sources(
        query[[node]]$right_data, replacement
      )
    }
  }
  query
}

# The reader advertises the result types and the metadata Arrow normally
# carries forward from the left source, including R column attributes.
arrow_expansion_schema <- function(result, source) {
  schema <- arrow::infer_schema(result)
  root <- source
  while (inherits(root, "arrow_dplyr_query") &&
           !inherits(root, "margin_arrow_expansion")) {
    root <- root$.data
  }
  old_schema <- root$schema
  metadata <- old_schema$metadata
  r_metadata <- utils::getFromNamespace("get_r_metadata_from_old_schema",
                                       "arrow")
  metadata$r <- r_metadata(schema, old_schema)
  schema$WithMetadata(metadata)
}

# Wrap a multi-set expansion in an Arrow query whose source is evaluated on
# consumption. Arrow builds a fresh reader for a collapsed unordered slice.
# The stub's right input keeps Arrow's source classification intact, while
# the reader method supplies the actual source instead of running the stub.
defer_arrow_expansion <- function(result, source, marker) {
  schema <- arrow_expansion_schema(result, source)
  stub <- dplyr::mutate(arrow::Table$create(schema = schema))
  stub$schema <- schema
  stub$union_all <- list(right_data = source)
  stub$head <- 0L
  stub$margin_expansion <- list(result = result, source = source, marker = marker)
  class(stub) <- c("margin_arrow_expansion", class(stub))
  dplyr::collapse(stub)
}

# Open a fresh expansion reader. Arrow asks for the reader during plan
# construction, before executing its outer plan. Materializing there allows
# the input to use Arrow operations that invoke R during execution.
#' @exportS3Method arrow::as_record_batch_reader
as_record_batch_reader.margin_arrow_expansion <- function(x, ...) {
  # Arrow's show_query() and explain() share this plan builder. Its reader
  # needs only the schema because it displays the plan without consuming it.
  show_plan <- utils::getFromNamespace("show_exec_plan", "arrow")
  displaying_plan <- any(vapply(seq_len(sys.nframe()), function(frame) {
    identical(sys.function(frame), show_plan)
  }, logical(1L)))
  if (displaying_plan) {
    return(arrow::as_record_batch_reader(x$.data))
  }

  expansion <- x$margin_expansion
  input <- dplyr::mutate(arrow::as_arrow_table(expansion$source))
  query <- rewrite_arrow_expansion_sources(expansion$result, function(query) {
    if (identical(attr(query, "marginplyr_arrow_expansion_marker"),
                  expansion$marker)) {
      input
    }
  })
  arrow::as_record_batch_reader(query)
}
