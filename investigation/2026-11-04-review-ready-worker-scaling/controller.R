args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1L]], mustWork = TRUE)
workspace <- normalizePath(args[[2L]], mustWork = TRUE)
for (relative in c('.github/scripts/cran-note-policy.R', 'tools/dependency-requirements.R',
    'tools/required-test-suggests.R', 'tools/review-ready-progress.R',
    'tools/review-ready-fifo.R', 'tools/review-ready-check-lib.R')) {
  source(file.path(root, relative))
}
main <- function() {
  identity <- review_ready_identity(root)
  cat('Benchmark SHA:', identity$sha, '\nWorkspace:', workspace, '\n')
  metadata <- list(version = 1L, sha = identity$sha,
    started_utc = format(Sys.time(), '%Y-%m-%dT%H:%M:%OS6Z', tz = 'UTC'),
    benchmark_only = TRUE, scope = 'experimental-worker-comparison',
    reason = 'User explicitly requested measured 1x4, 1x6, 1x8 and 2x4 configurations.')
  saveRDS(metadata, file.path(workspace, 'metadata.rds'))
  source_root <- archive_review_ready_source(identity, workspace)
  prerequisites <- review_ready_prerequisites(read.dcf(file.path(source_root, 'DESCRIPTION')))
  saveRDS(list(session = sessionInfo(), packages = installed.packages(),
    hardware = system2('sysctl', c('-n', 'hw.memsize', 'hw.physicalcpu', 'hw.logicalcpu'), stdout = TRUE),
    initial_memory = ps::ps_system_memory(), initial_swap = ps::ps_system_swap()),
    file.path(workspace, 'environment.rds'))
  baseline <- file.path(workspace, 'baseline')
  dir.create(baseline)
  runner <- review_ready_child_runner(review_ready_child_env(baseline))
  source(file.path(source_root, 'tools/review-ready-priority.R'))
  review_ready_priority_read(readLines(file.path(source_root, 'DESCRIPTION')))
  required <- run_review_ready_source(source_root, prerequisites, runner = runner)
  saveRDS(list(status = 'passed', sha = identity$sha, required = required),
    file.path(baseline, 'baseline-result.rds'))
  configurations <- data.frame(name = c('1x4', '1x6', '1x8', '2x4'),
    capacity = c(1L, 1L, 1L, 2L), workers = c(4L, 6L, 8L, 4L))
  write.csv(configurations, file.path(workspace, 'configurations.csv'), row.names = FALSE)
  # Every job gets the same pristine archive and fresh private caches.
  for (config in configurations$name) for (index in 1:2) {
    directory <- file.path(workspace, config, paste0('job-', index))
    dir.create(directory, recursive = TRUE)
    dir.create(file.path(directory, 'source'))
    utils::untar(file.path(workspace, 'source.tar'), exdir = file.path(directory, 'source'))
    saveRDS(metadata, file.path(directory, 'invocation.rds'))
  }
  cat('All five baseline checks passed. Acquiring the shared heavy slot.\n')
  flush.console()
  lock <- review_ready_heavy_lock()
  on.exit(review_ready_fifo_release(lock), add = TRUE)
  saveRDS(list(acquired = as.numeric(Sys.time())), file.path(workspace, 'admission.rds'))
  active <- list()
  owned <- list()
  on.exit({
    for (id in names(active)) review_ready_stop_process(active[[id]]$process, owned[[id]])
  }, add = TRUE, after = FALSE)
  completions <- list()
  batches <- list()
  samples <- file(file.path(workspace, 'resources.csv'), open = 'wt')
  on.exit(close(samples), add = TRUE)
  writeLines('time,config,job,processes,rss_bytes,host_available_bytes,swap_used_bytes,swapin_bytes,swapout_bytes,cpu_user,cpu_system,cpu_idle,load1', samples)
  for (row in seq_len(nrow(configurations))) {
    config <- configurations[row, ]
    batch_start <- as.numeric(Sys.time())
    pending <- 1:2
    finished <- 0L
    heartbeat <- -Inf
    swap_start <- ps::ps_system_swap()
    cat('\nBEGIN', config$name, 'at', format(Sys.time(), tz = 'UTC'), '\n')
    flush.console()
    repeat {
      while (length(pending) && length(active) < config$capacity) {
        index <- pending[[1L]]
        pending <- pending[-1L]
        id <- paste0(config$name, '-job-', index)
        directory <- file.path(workspace, config$name, paste0('job-', index))
        job <- list(directory = directory, source = file.path(directory, 'source'),
          required = required, r = prerequisites$r, workers = config$workers, sha = identity$sha)
        saveRDS(job, file.path(directory, 'arguments.rds'))
        env <- review_ready_child_env(directory)
        env[c('MARGINPLYR_REVIEW_TIMING', 'MARGINPLYR_REVIEW_TIMING_HELPER')] <- ''
        values <- Sys.getenv()
        values[names(env)] <- env
        process <- processx::process$new(prerequisites$rscript,
          c(file.path(workspace, 'worker.R'), file.path(directory, 'arguments.rds')),
          wd = directory, env = values, stdout = file.path(directory, 'console.log'),
          stderr = file.path(directory, 'stderr.log'), cleanup_tree = TRUE, supervise = TRUE)
        active[[id]] <- list(process = process, directory = directory,
          started = as.numeric(Sys.time()), submitted = batch_start)
        owned[[id]] <- list()
        cat('START', id, 'PID', process$get_pid(), '\n')
        flush.console()
      }
      memory <- ps::ps_system_memory()
      swap <- ps::ps_system_swap()
      cpu <- ps::ps_system_cpu_times()
      load <- ps::ps_loadavg()[[1L]]
      for (id in names(active)) {
        item <- active[[id]]
        handles <- review_ready_children(item$process)
        for (handle in handles) owned[[id]][[as.character(ps::ps_pid(handle))]] <- handle
        if (item$process$is_alive()) handles <- c(list(ps::ps_handle(item$process$get_pid())), handles)
        rss <- vapply(handles, function(h) tryCatch(ps::ps_memory_info(h)[['rss']], error = function(e) 0), numeric(1))
        record <- c(as.numeric(Sys.time()), config$name, id, length(handles), sum(rss),
          memory$avail, swap$used, swap$sin, swap$sout, cpu[['user']], cpu[['system']], cpu[['idle']], load)
        writeLines(paste(record, collapse = ','), samples)
        if (!item$process$is_alive()) {
          item$process$wait()
          status <- item$process$get_exit_status()
          review_ready_stop_process(item$process, owned[[id]])
          ended <- as.numeric(Sys.time())
          completions[[id]] <- data.frame(config = config$name, job = id,
            submitted = item$submitted, started = item$started, finished = ended,
            wait_s = item$started - item$submitted, execution_s = ended - item$started,
            turnaround_s = ended - item$submitted, exit_status = status)
          active[[id]] <- NULL
          finished <- finished + 1L
          cat('END', id, 'exit', status, 'turnaround_s', round(ended - batch_start, 3), '\n')
          flush.console()
          saveRDS(completions, file.path(workspace, 'completions.rds'))
          if (status != 0L) stop('Benchmark check failed; retain logs and do not treat it as a performance pass: ', id)
        }
      }
      flush(samples)
      now <- as.numeric(Sys.time())
      if (now - heartbeat >= 30) {
        details <- vapply(active, function(item) {
          path <- file.path(item$directory, 'check', 'test-progress.tsv')
          progress <- review_ready_progress_snapshot(path)
          if (is.null(progress)) return('build/check setup')
          paste0(progress$completed, '/', progress$total, ' test files')
        }, character(1))
        cat('HEARTBEAT', config$name, 'elapsed_s', round(now - batch_start),
          'completed', finished, '/2', 'available_GiB', round(memory$avail / 1024^3, 2),
          'swap_GiB', round(swap$used / 1024^3, 2), paste(details, collapse = '; '), '\n')
        flush.console()
        heartbeat <- now
      }
      if (finished == 2L) break
      Sys.sleep(1)
    }
    batches[[config$name]] <- list(started = batch_start, finished = as.numeric(Sys.time()),
      swap_start = swap_start, swap_end = ps::ps_system_swap())
    saveRDS(batches, file.path(workspace, 'batches.rds'))
    write.csv(do.call(rbind, completions), file.path(workspace, 'completions.csv'), row.names = FALSE)
    cat('BATCH COMPLETE', config$name, '\n')
    flush.console()
  }
  saveRDS(list(status = 'passed', sha = identity$sha, finished = as.numeric(Sys.time()),
    benchmark_only = TRUE), file.path(workspace, 'terminal.rds'))
  cat('ALL EIGHT EXPERIMENTAL CHECKS COMPLETE\n')
}
main()
