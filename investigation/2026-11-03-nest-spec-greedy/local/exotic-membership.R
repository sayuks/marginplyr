# Dated finite public-seam oracle; run from the investigated checkout root.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
out <- "investigation/2026-11-03-nest-spec-greedy/local"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
key_vectors <- list(
  posixlt = as.POSIXlt(c("2026-10-01", "2026-10-01", NA, "2026-10-02", NA, "2026-10-02"), tz = "Asia/Tokyo"),
  difftime = as.difftime(c(1, 1, NA, 2, NaN, 2), units = "mins"),
  hms = hms::hms(c(1, 1, NA, 2, NaN, 2)),
  units = units::set_units(c(1, 1, NA, 2, NaN, 2), "m"),
  labelled = haven::labelled(c(1, 1, NA, 2, haven::tagged_na("a"), 2), labels = c(A=1, B=2)),
  labelled_spss = haven::labelled_spss(c(1, 1, NA, 2, 3, 2), na_values=3),
  fs_path = fs::path(c("a", "a", NA, "b", NA, "b")),
  hexmode = as.hexmode(c(1,1,NA,2,NA,2)),
  octmode = as.octmode(c(1,1,NA,2,NA,2)),
  blob = blob::blob(as.raw(1), as.raw(1), NULL, as.raw(2), NULL, as.raw(2)),
  list_of = vctrs::list_of(1L, 1L, NULL, 2L, NA_integer_, 2L),
  empty_df = tibble::new_tibble(list(), nrow=6L),
  deep_df = tibble::tibble(a=tibble::tibble(b=c(1L,1L,NA,2L,NA,2L)), c=factor(c("a","a",NA,"b",NA,"b"), exclude=NULL)),
  matrix3 = I(array(c(1L,1L,NA,2L,NA,2L, 2L,2L,NA,3L,NA,3L), dim=c(6L,1L,2L))),
  sfc = sf::st_sfc(lapply(c(1,1,NA,2,NA,2), function(z) sf::st_point(c(z,z))), crs=4326),
  rvar = posterior::rvar(matrix(rep(c(1,1,NA,2,NA,2), each=4L), nrow=4L))
)
canonical <- function(x) sort(vapply(x, function(z) paste(z, collapse=","), character(1)))
records <- list(); k <- 0L; projection_checks <- 0L
for (kind in names(key_vectors)) for (empty in c(FALSE, TRUE)) {
  source <- tibble::tibble(g=key_vectors[[kind]], row_id=seq_len(6L),
    packed=tibble::tibble(a=factor(c("x","x",NA,"y","z",NA), exclude=NULL),
      b=I(matrix(seq_len(12L), nrow=6L))),
    refs=list(NULL, list(NA), quote(x), expression(a), new.env(), function() 1))
  if (empty) source <- source[integer(), ]
  for (role in c("dimension", "fixed")) for (family in c("single", "rollup")) {
    native <- tryCatch(dplyr::summarise(source, members=list(.data$row_id), .by="g"), error=identity)
    # rvar's native grouping fills its equality-proxy cache. Compare the
    # subsequent target call with that same upstream-materialized source.
    before <- serialize(source, NULL)
    for (label in c("missing", "display")) for (keep in c(FALSE,TRUE)) {
      for (verb in c("nest_with_margins", "nest_by_with_margins")) {
        k <- k + 1L
        grouping <- if (role == "fixed") NULL else if (family == "single") grouping_set(g) else rollup(g)
        actual <- tryCatch(get(verb)(source, .by=dplyr::all_of(if (role == "fixed") "g" else character()),
          .grouping=grouping, .margin_label=if (label == "missing") NULL else "ALL",
          .id="set", .keep=keep), error=identity)
        verdict <- "match"; detail <- ""
        if (inherits(actual,"error")) {
          verdict <- if (inherits(native,"error")) "native_error" else "target_error"
          detail <- paste(class(actual)[1], conditionMessage(actual))
        } else {
          sets <- if (role == "dimension" && family == "rollup") list("g",character()) else list("g")
          if (inherits(native,"error")) {
            verdict <- "native_only_error"; detail <- conditionMessage(native)
          } else for (sid in seq_along(sets)) {
            expected <- if (nrow(source)==0L) list() else if (length(sets[[sid]])==0L) list(seq_len(nrow(source))) else native$members
            cells <- actual$data[actual$set==sid]
            got <- lapply(cells,function(z) z$row_id)
            if (!identical(canonical(got),canonical(expected))) {
              verdict <- "membership_mismatch"; detail <- paste("set",sid,paste(canonical(got),collapse="/"),"vs",paste(canonical(expected),collapse="/"))
            }
            for (cell in cells) {
              wanted_names <- if (keep) names(source) else setdiff(names(source),"g")
              if (!identical(names(cell),wanted_names)) { verdict <- "projection_mismatch"; detail <- "column names" }
              for (nm in wanted_names) {
                wanted <- vctrs::vec_slice(source[[nm]],cell$row_id)
                if (!identical(cell[[nm]],wanted) && !isTRUE(all.equal(cell[[nm]],wanted,check.attributes=FALSE))) {
                  verdict <- "projection_mismatch"; detail <- nm
                }
                projection_checks <- projection_checks+1L
              }
            }
          }
        }
        stopifnot(identical(serialize(source,NULL),before))
        records[[k]] <- data.frame(kind,empty,role,family,label,keep,verb,verdict,detail)
      }
    }
  }
  cat("completed",kind,if(empty) "empty" else "populated","\n")
}
ledger <- do.call(rbind,records)
write.csv(ledger,file.path(out,"exotic-membership.csv"),row.names=FALSE)
print(table(ledger$verdict)); cat("Projection checks:",projection_checks,"\n")
print(unique(ledger[ledger$verdict!="match",c("kind","empty","role","family","label","verdict","detail")]),row.names=FALSE)
print(sessionInfo())
if ("--assert" %in% commandArgs(trailingOnly=TRUE)) stopifnot(!any(ledger$verdict %in% c("membership_mismatch","projection_mismatch")))
