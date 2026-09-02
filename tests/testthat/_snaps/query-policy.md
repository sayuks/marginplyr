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

# marginplyr functions reaching an execution entry point

    Code
      reach
    Output
       [1] "check_dialect_share_sources"    "check_observed_label_collision"
       [3] "execute_margin_expand"          "execute_margin_nest"           
       [5] "execute_margin_summary"         "expand_with_margins"           
       [7] "grouping_selection_proxy"       "inspect_grouping"              
       [9] "nest_by_with_margins"           "nest_margin_pipeline"          
      [11] "nest_with_margins"              "prepare_grouping_plan"         
      [13] "prepare_margin_operation"       "probe_share_dialect"           
      [15] "probe_share_dialect_answer"     "share_dialect_verdict"         
      [17] "summarise_with_margins"         "summarize_with_margins"        
      [19] "validate_margin_label"          "validate_margin_operation"     

# backend kinds granted the collect_selection_proxy capability

    Code
      kinds_with_proxy
    Output
      [1] "dtplyr" "duckdb"

