suppressPackageStartupMessages({library(dplyr); pkgload::load_all(".", quiet = TRUE)})
main <- function() {
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(con))
for (col in c("BINARY", "NOCASE", "RTRIM")) {
  DBI::dbExecute(con, sprintf("CREATE TABLE src(f TEXT COLLATE %s, g TEXT COLLATE %s, h INTEGER, v INTEGER)", col, col))
  DBI::dbExecute(con, "INSERT INTO src VALUES ('a','b',1,1),('A','B',1,2),('a','b ',NULL,4),('B','a',2,8),(NULL,'a',NULL,16),(NULL,NULL,1,32),('b','A',NULL,64),('a',NULL,3,128)")
  for (boolean in c(FALSE, TRUE)) {
    f <- if (boolean) "true" else "f"
    g <- if (boolean) "false" else "g"
    source <- tbl(con, "src")
    if (boolean) source <- rename(source, "true" = "f", "false" = "g")
    for (duplicate in c(FALSE, TRUE)) {
      specs <- if (duplicate) list(character(), g, c(g, "h"), g, character(), "h") else list(character(), g, c(g, "h"), "h")
      grouping <- rlang::exec(grouping_sets, !!!lapply(specs, function(x) rlang::exec(grouping_set, !!!rlang::syms(x))))
      for (sort in c("first","last","none")) {
        for (label in list(NULL, "Margin")) {
          result <- summarize_with_margins(source, value = sum(v, na.rm = TRUE), fraction = share_of_total(value),
            .by = all_of(f), .grouping = grouping, .duplicates = "keep", .id = "set", .sort = sort, .margin_label = label, .check_share_source = FALSE)
          sets <- lapply(seq_along(specs), function(i) {
            vars <- sub(paste0("^", g, "$"), "g", specs[[i]])
            terms <- c("f", vars)
            output <- c("f", if ("g" %in% terms) "g" else if(is.null(label)) "NULL AS g" else "'Margin' AS g",
              if ("h" %in% terms) "h" else if(is.null(label)) "NULL AS h" else "'Margin' AS h")
            z <- DBI::dbGetQuery(con, paste0("SELECT ", paste(output, collapse=", "), ", sum(v) AS value FROM src GROUP BY ", paste(terms, collapse=", ")))
            z$set <- i
            z
          })
          expected <- bind_rows(lapply(sets, function(z) mutate(z, h=as.character(h))))
          norm <- switch(col, NOCASE = tolower, RTRIM = function(x) sub(" +$","",x), identity)
          normal <- function(z) {
            names(z)[match(c(f,g), names(z))] <- c("f", "g")
            mutate(z, f=norm(f), g=if_else(g == "Margin",g,norm(g), missing=NA_character_), h=as.character(h))
          }
          expected <- mutate(expected, f=norm(f), g=if_else(g == "Margin",g,norm(g), missing=NA_character_))
          key <- function(z) arrange(select(z, f,g,h,set,value,fraction), set,value)
          total <- function(f, i, value) {
            part <- if (is.na(f)) is.na(sets[[1]]$f) else !is.na(sets[[1]]$f) & norm(sets[[1]]$f)==f
            if (!length(specs[[i]])) return(1)
            value / sets[[1]]$value[part]
          }
          expected$fraction <- mapply(total, expected$f, expected$set, expected$value)
          for (retrieval in c("collect", "compute")) {
            got <- if (retrieval == "compute") {
              out <- compute(result, name = "dst", analyze = FALSE)
              materialized <- collect(out)
              DBI::dbExecute(con, "DROP TABLE dst")
              materialized
            } else {
              collect(result)
            }
            got <- normal(got)
            val <- isTRUE(all.equal(key(got), key(expected), check.attributes=FALSE))
            if (sort != "none") {
              desc <- if (sort == "first") -1L else 1L
              bg <- as.integer(!vapply(specs, function(x) g %in% x, logical(1)))
              bh <- as.integer(!vapply(specs, function(x) "h" %in% x, logical(1)))
              expected_sorted <- arrange(expected, f, desc * bg[set], g, desc * bh[set],
                if (is.null(label)) as.integer(h) else h, set)
              order <- identical(as.integer(got$set), as.integer(expected_sorted$set)) &&
                identical(as.numeric(got$value), as.numeric(expected_sorted$value))
            } else order <- NA
            cat(col,boolean,duplicate,sort,is.null(label),retrieval,val,order,"\n")
            if (!is.na(order) && !order) {
              cat("GOT ",paste(got$value,collapse=","),"\n")
              cat("WANT",paste(expected_sorted$value,collapse=","),"\n")
            }
            stopifnot(val, is.na(order) || order)
          }
        }
      }
    }
  }
  DBI::dbExecute(con, "DROP TABLE src")
}
}
main()
