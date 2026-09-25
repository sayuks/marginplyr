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
       [3] "dtplyr_selection_proxy"         "execute_margin_expand"         
       [5] "execute_margin_nest"            "execute_margin_summary"        
       [7] "expand_with_margins"            "grouping_selection_proxy"      
       [9] "inspect_grouping"               "nest_by_with_margins"          
      [11] "nest_margin_pipeline"           "nest_with_margins"             
      [13] "prepare_grouping_plan"          "prepare_margin_operation"      
      [15] "probe_share_dialect"            "probe_share_dialect_answer"    
      [17] "share_dialect_verdict"          "summarise_with_margins"        
      [19] "summarize_with_margins"         "validate_margin_label"         
      [21] "validate_margin_operation"     

# backend kinds granted the collect_selection_proxy capability

    Code
      kinds_with_proxy
    Output
      [1] "dtplyr" "duckdb"

