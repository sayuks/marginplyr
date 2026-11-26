# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# ?marginplyr_conditions says no field of the condition names the refused argument, while share refusals carry such fields.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({ library(dplyr) })
df <- data.frame(g = c("a","a","b","b"), v = 1:4)
cat("EXPECTED (Rd): no field names the refused argument; only the message does.\n")
e1 <- tryCatch(summarize_with_margins(df, a = range(v), s = share_of_parent(a), .grouping = rollup(g)), error = function(e) e)
cat("share cardinality: fields =", paste(names(e1), collapse = ","), "| $share_output =", e1$share_output, "| $source_summary =", e1$source_summary, "\n")
e2 <- tryCatch(summarize_with_margins(df, a = sum(v), across(c(a, zz), share_of_parent, .names = "{.col}_s"), .grouping = rollup(g)), error = function(e) e)
cat("unknown across source: class[1] =", class(e2)[1], "| $source_summary =", e2$source_summary, "\n")
e3 <- tryCatch(summarize_with_margins(df, a = sum(v), a = mean(v), across(a, share_of_parent, .names = "{.col}_s"), .grouping = rollup(g)), error = function(e) e)
cat("duplicate across source: class[1] =", class(e3)[1], "| $source_summary =", e3$source_summary, "\n")
cat("NATIVE CONTROL: not applicable (the sentence is about marginplyr's own conditions); a refusal with no subclass has no such field:\n")
e4 <- tryCatch(summarize_with_margins(df, n = n(), .grouping = rollup(zz_nope)), error = function(e) e)
cat("   tidyselect refusal fields =", paste(names(e4), collapse = ","), "\n")
