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
      12  dbplyr remote_query_plan        FALSE
      13   arrow    as_arrow_table        FALSE

# marginplyr functions reaching an execution entry point

    Code
      reach
    Output
       [1] "check_dialect_share_sources"    "check_observed_label_collision"
       [3] "dtplyr_join_set_types_match"    "dtplyr_metadata_safe_step"     
       [5] "dtplyr_selection_proxy"         "execute_margin_expand"         
       [7] "execute_margin_nest"            "execute_margin_summary"        
       [9] "expand_with_margins"            "grouping_selection_proxy"      
      [11] "inspect_grouping"               "nest_by_with_margins"          
      [13] "nest_margin_pipeline"           "nest_with_margins"             
      [15] "prepare_grouping_plan"          "prepare_margin_operation"      
      [17] "probe_share_dialect"            "probe_share_dialect_answer"    
      [19] "share_dialect_verdict"          "summarise_with_margins"        
      [21] "summarize_with_margins"         "validate_margin_label"         
      [23] "validate_margin_operation"     

# backend kinds granted the collect_selection_proxy capability

    Code
      kinds_with_proxy
    Output
      [1] "dtplyr" "duckdb"

