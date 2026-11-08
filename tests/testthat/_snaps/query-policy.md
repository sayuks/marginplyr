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
      13     DBI     dbExistsTable        FALSE
      14  dbplyr        db_collect        FALSE
      15  dbplyr        db_compute        FALSE
      16  dbplyr remote_query_plan        FALSE
      17   arrow    as_arrow_table        FALSE

# marginplyr functions reaching an execution entry point

    Code
      cat(reach, sep = "\n")
    Output
      as_record_batch_reader.margin_arrow_expansion
      check_dialect_share_sources
      check_observed_label_collision
      collect.marginplyr_sqlite_typed_result
      compute.marginplyr_duckdb_env
      compute.marginplyr_sqlite_typed_result
      duckdb_metadata_catalog
      execute_margin_expand
      execute_margin_nest
      execute_margin_summary
      execute_share_probe_control
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
      sqlite_compute_destination
      sqlite_compute_name_resolves
      sqlite_new_compute_name
      sqlite_plan_temp_destination
      sqlite_pragma_sources_clear
      sqlite_publish_destination
      sqlite_restore_compute_names
      sqlite_temp_overwrite_safe
      sqlite_temp_virtual_safe
      sqlite_virtual_sources_clear
      sqlite_with_compute_savepoint
      summarise_with_margins
      summarize_with_margins
      validate_margin_label
      validate_margin_operation

# backend kinds granted the collect_selection_proxy capability

    Code
      kinds_with_proxy
    Output
      [1] "dtplyr" "duckdb"

