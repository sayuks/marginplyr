# Command-level compatibility fixtures; actual rendering has its own fixture.
run_renderer_compatibility_contracts <- function() {
  fixture <- rr_mkdir(tempfile("renderer-compatibility-"))
  on.exit(unlink(fixture, recursive = TRUE, expand = FALSE), add = TRUE)
  bin <- rr_mkdir(file.path(fixture, "tool/bin"))
  command <- file.path(bin, "quarto")
  config <- file.path(bin, "fixture.json")
  writeLines(c("#!/usr/bin/env python3", "import json, os, pathlib, sys, tempfile",
    "root = pathlib.Path(__file__).parent",
    "config = json.loads((root / 'fixture.json').read_text())",
    "args = sys.argv[1:]",
    "if args == ['--version']: print(config['version'])",
    "elif args == ['render', '--help']: print('--to --output' if config.get('render', True) else '--help')",
    "elif args == ['pandoc', '--version']: print('pandoc 3.10\\nUser data directory: ' + config.get('pandoc_data', os.path.join(os.environ['XDG_DATA_HOME'], 'pandoc')))",
    "elif args[:2] == ['check', 'info']:",
    "    cache = config.get('cache', os.path.join(os.environ['XDG_CACHE_HOME'], config.get('layout', 'quarto')))",
    "    if 'cache' not in config: pathlib.Path(cache).mkdir(parents=True, exist_ok=True)",
    "    pathlib.Path(args[args.index('--output') + 1]).write_text(json.dumps({'version': config['version'], 'info': {'cacheDir': cache}}))",
    "elif args[:1] == ['run']:",
    "    cache = config.get('deno_cache', os.environ['DENO_DIR'])",
    "    temporary = config.get('temporary', tempfile.mkdtemp(dir=os.environ['TMPDIR']))",
    "    print(json.dumps({'version': '2.8.0', 'executable': str(root / 'deno'), 'cache': cache, 'temporary': temporary, 'home': config.get('home', os.environ['HOME']), 'libraries': config.get('libraries', os.environ['R_LIBS'])}))",
    "else: sys.exit('Unsupported capability query')"), command)
  Sys.chmod(command, "0755")
  writeLines("fixture Deno executable", file.path(bin, "deno"))
  writeLines("fixture Quarto script", file.path(bin, "quarto.js"))
  directory <- rr_mkdir(file.path(fixture, "request"))
  values <- rr_render_env(directory)
  values[["PATH"]] <- paste(bin, values[["PATH"]], sep = .Platform$path.sep)
  probe <- function(settings) {
    rr_write(config, settings)
    rr_in_environment(values, rr_renderer())
  }
  original <- probe(list(version = "1.10.18"))
  expect(identical(original$version, "1.10.18"), "A compatible renderer reports its actual version")
  another <- rr_render_env(rr_mkdir(file.path(fixture, "another-request")))
  another[["PATH"]] <- values[["PATH"]]
  expect(rr_same(original$identity, rr_in_environment(another, rr_renderer())$identity),
    "Independent private request paths preserve the same renderer input identity")
  cat("\n// Compatible implementation update.\n", file = file.path(bin, "quarto.js"), append = TRUE)
  same_version <- probe(list(version = "1.10.18"))
  expect(!rr_same(original$identity, same_version$identity), "Changed renderer contents invalidate identity even at the same version")
  future <- probe(list(version = "9.87.65-compatible", layout = "quarto-next/cache"))
  expect(identical(future$version, "9.87.65-compatible") && !rr_same(same_version$identity, future$identity),
    "Unknown compatible versions and owned cache layouts are accepted with new evidence identities")
  outside <- rr_mkdir(file.path(fixture, "outside"))
  writeLines("Keep unrelated bytes", file.path(outside, "sentinel"))
  rejects(probe(list(version = "9.87.65", cache = outside)), "Quarto cache isolation")
  rejects(probe(list(version = "9.87.65", deno_cache = outside)), "Deno cache isolation")
  rejects(probe(list(version = "9.87.65", pandoc_data = outside)), "Pandoc data isolation")
  rejects(probe(list(version = "9.87.65", temporary = outside)), "Deno temporary storage isolation")
  rejects(probe(list(version = "9.87.65", home = outside)), "Quarto HOME isolation")
  rejects(probe(list(version = "9.87.65", libraries = "reordered libraries")), "ordered R library environment")
  link <- file.path(directory, "cache", "linked")
  expect(file.symlink(outside, link), "Create a misleading private cache link")
  rejects(probe(list(version = "9.87.65", cache = link)), "Quarto cache isolation.*symlink")
  rejects(probe(list(version = "9.87.65", render = FALSE)), "render.*--to.*--output")
  expect(identical(readLines(file.path(outside, "sentinel")), "Keep unrelated bytes"),
    "Incompatible storage is refused without deleting unrelated data")

  # The source/Git and authenticated-store seams are held fixed here; the real
  # planner must distinguish the identities returned by the command probes.
  choose <- rr_plan
  environment(choose) <- new.env(parent = environment(rr_plan))
  adapter <- environment(choose)
  now <- rr_now(); sha <- paste(rep("a", 40), collapse = "")
  source <- paste(rep("b", 64), collapse = "")
  clock <- list(r = "4.6.1", checker = "2efcaa5190c038e60834bd5c4e502c82",
    timeout = "34ee7dea5eac7ba7138bd3c60651faee", leeway = 300)
  attempt <- list(status = 0L, timeout = FALSE, errors = 0L, warnings = 0L, notes = list())
  stage <- list(status = "passed", source = source, environment = rr_digest(original$identity),
    observation_sequence = 1L, timestamps = list(observed = now, paths = list(DESCRIPTION = now)),
    check = c(attempt, list(args = list("--as-cran"), attempts = list(attempt))))
  origin <- list(available = list("build-full"), payload = list(kind = "evidence", outcome = 0L,
    request = rr_id(), submitted = sha, repository = "fixture", sequence = 2L, finished = now,
    units = list(`build-full` = stage)))
  adapter$rr_with_lock <- function(path, code) force(code)
  adapter$rr_store_read <- function(...) list(records = list(origin))
  adapter$rr_store_history <- function(state, value) value$records
  adapter$rr_repository <- function(...) "fixture"
  adapter$rr_source_inputs <- function(...) source
  adapter$rr_git <- function(...) as.character(now)
  adapter$rr_run <- function(...) list(status = 0L)
  selection <- list(path = "Full", verifiers = list())
  plan <- function(renderer) choose(fixture, sha, selection, fixture, now = now,
    observe = function(...) list(sources = list(`build-full` = source),
      environment = list(digest = rr_digest(renderer$identity), clock = clock)))
  expect(plan(original)$units[["build-full"]]$action == "reuse", "Unchanged compatible renderer identity permits its original execution")
  expect(plan(same_version)$units[["build-full"]]$action == "execute" && plan(future)$units[["build-full"]]$action == "execute",
    "Compatible renderer upgrades execute instead of reusing old-version or old-content evidence")
  cached <- file.path(rr_mkdir(file.path(directory, "cache", "future-layout")), "probe-output")
  writeLines("Metadata output", cached)
  before <- rr_normalize_environment(values)
  rr_empty_renderer_cache(values)
  remaining <- list.files(file.path(directory, "cache"), all.files = TRUE, recursive = TRUE, full.names = TRUE)
  expect(!any(!dir.exists(remaining)) && rr_same(before, rr_normalize_environment(values)),
    "Validation resets all private cache layouts without changing its registered environment")
  expect(identical(readLines(file.path(outside, "sentinel")), "Keep unrelated bytes"),
    "Resetting a private cache removes inner links without following them")
  rr_remove(file.path(directory, "cache"), directory)
  expect(file.symlink(outside, file.path(directory, "cache")), "Create a misleading cache root")
  rejects(rr_empty_renderer_cache(values), "symlink")
  expect(!dir.exists(file.path(outside, "home")) && identical(readLines(file.path(outside, "sentinel")), "Keep unrelated bytes"),
    "A linked cache root is refused before creating any external role directory")
  cat("Renderer capabilities accept compatible updates, reject escaping storage, and invalidate old evidence.\n")
}
run_renderer_compatibility_contracts()
