# the scanned entry-point set is the ADR 0020 execution catalog

    Code
      lazy_execution_entry_points()
    Output
         package              name subject_test
      1    dplyr           collect        FALSE
      2    dplyr           compute        FALSE
      3    dplyr              pull        FALSE
      4    dplyr           explain        FALSE
      5     base     as.data.frame         TRUE
      6   tibble         as_tibble         TRUE
      7      DBI        dbGetQuery        FALSE
      8      DBI       dbSendQuery        FALSE
      9      DBI   dbSendStatement        FALSE
      10     DBI           dbFetch        FALSE
      11     DBI       dbReadTable        FALSE
      12     DBI         dbExecute        FALSE
      13  dbplyr        db_collect        FALSE
      14  dbplyr        db_compute        FALSE
      15  dbplyr remote_query_plan        FALSE
      16   arrow    as_arrow_table        FALSE

# marginplyr functions reaching an execution entry point

    Code
      cat(reach, sep = "\n")
    Output
      check_dialect_share_sources
      check_observed_label_collision
      collect.marginplyr_sqlite_typed_order
      compute.marginplyr_sqlite_typed_order
      dtplyr_join_set_types_match
      dtplyr_metadata_safe_step
      dtplyr_selection_proxy
      execute_margin_expand
      execute_margin_nest
      execute_margin_summary
      expand_with_margins
      grouping_selection_proxy
      inspect_grouping
      nest_by_with_margins
      nest_margin_pipeline
      nest_with_margins
      prepare_grouping_plan
      prepare_margin_operation
      probe_share_dialect
      probe_share_dialect_answer
      share_dialect_verdict
      summarise_with_margins
      summarize_with_margins
      validate_margin_label
      validate_margin_operation

# backend kinds granted the collect_selection_proxy capability

    Code
      kinds_with_proxy
    Output
      [1] "dtplyr" "duckdb"

