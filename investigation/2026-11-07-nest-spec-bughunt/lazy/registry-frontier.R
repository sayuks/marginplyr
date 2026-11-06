Sys.setenv(TESTTHAT_PARALLEL = "false")
source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE")
if (!nzchar(source_dir)) stop("Set MARGINPLYR_HUNT_SOURCE to the investigated source directory.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
evidence_dir <- dirname(normalizePath(script))
pkgload::load_all(source_dir, quiet = TRUE)

one <- function(entry, binding) {
  source <- data.table::data.table(g = c("a", "b"), v = 1:2)
  before <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  ns <- if (entry == "bracket") baseenv() else asNamespace("data.table")
  name <- if (entry == "bracket") "[.data.table" else "as.data.table.dtplyr_step"
  registry <- get(".__S3MethodsTable__.", ns)
  original <- get(name, registry, inherits = FALSE)
  on.exit({
    if (exists(name, registry, inherits = FALSE)) rm(list = name, envir = registry)
    assign(name, original, registry)
  })
  looked_up <- 0L
  source_sums <- numeric()
  read <- function() {
    looked_up <<- looked_up + 1L
    source_sums <<- c(source_sums, sum(source$v))
    original
  }
  rm(list = name, envir = registry)
  if (binding == "active") {
    makeActiveBinding(name, read, registry)
  } else if (binding == "lazy") {
    delayedAssign(name, read(), assign.env = registry)
  }
  name_plan <- inspect_grouping(root, .grouping = grouping_set(g))
  stopifnot(identical(name_plan$included, "(g)"), identical(looked_up, 0L))
  result <- tryCatch({
    plan <- inspect_grouping(root, .grouping = grouping_set(where(is.character)))
    plan$included
  }, error = function(e) paste(class(e)[[1L]], conditionMessage(e)))
  stopifnot(identical(serialize(source, NULL), before))
  data.frame(entry, binding, looked_up, source_sums = paste(source_sums, collapse = ","), result)
}
grid <- expand.grid(entry = c("bracket", "converter"), binding = c("absent", "active", "lazy"), stringsAsFactors = FALSE)
results <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) do.call(one, as.list(grid[i, ]))))
print(results, row.names = FALSE)
write.csv(results, Sys.getenv("MARGINPLYR_HUNT_OUTPUT", file.path(evidence_dir, "registry-frontier.csv")), row.names = FALSE)
