# ---------------------------------------------------------------------------
# run_experiment.R
#
# Runs every workflow script in a fresh Rscript process and records wall-clock
# timing (from the script's own marks), peak resident memory (GNU time -v),
# attached packages, the selected learner, and test-set performance.
#
# Two arms:
#   datasets  every included biobench dataset x scenario x framework x seed
#   scaling   simulated data of increasing size, compare scenario only
#
# Usage, from the paper root (intro-funcml-compactness-metapackage-CONFERENCE-2026/):
#   Rscript experiment-biobench/R/run_experiment.R [options]
# Options:
#   --arm=datasets|scaling|all   default all
#   --reps=3                     seeds per cell (1001, 1002, ...)
#   --jobs=1                     parallel processes; keep 1 for timing results
#   --smoke                      one seed, sizes up to 2000, output to results-smoke/
#   --only=<regex>               run only cells whose id matches
#   --timeout=3600               seconds before a single run is killed
#
# Runs are resumable: a run whose directory already holds done.txt is skipped,
# so an interrupted session can be restarted with the same command.
# ---------------------------------------------------------------------------

EXP <- "experiment-biobench"
FRAMEWORKS <- c("funcml", "mlr3", "tidymodels")
SCENARIOS <- c("compare", "preprocess", "tune")
SIM_SIZES <- c(500L, 2000L, 10000L, 50000L)
DEFAULT_PKGS <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods")

opt <- function(name, default) {
  a <- grep(sprintf("^--%s(=|$)", name), commandArgs(TRUE), value = TRUE)
  if (!length(a)) return(default)
  if (!grepl("=", a[1])) return(TRUE)
  sub("^[^=]*=", "", a[1])
}
ARM <- opt("arm", "all")
SMOKE <- isTRUE(opt("smoke", FALSE))
REPS <- if (SMOKE) 1L else as.integer(opt("reps", "3"))
JOBS <- as.integer(opt("jobs", "1"))
ONLY <- opt("only", "")
TIMEOUT <- as.integer(opt("timeout", "3600"))
RESULTS <- file.path(EXP, if (SMOKE) "results-smoke" else "results")
RUNS <- file.path(RESULTS, "runs")
SEEDS <- 1000L + seq_len(REPS)

build_grid <- function() {
  ds <- utils::read.csv(file.path(EXP, "results", "datasets.csv"))
  ds <- ds[ds$included, ]
  g <- list()
  if (ARM %in% c("datasets", "all")) {
    g$datasets <- expand.grid(arm = "datasets", data = paste0("biobench:", ds$dataset),
                              scenario = SCENARIOS, framework = FRAMEWORKS,
                              seed = SEEDS, stringsAsFactors = FALSE)
  }
  if (ARM %in% c("scaling", "all")) {
    sizes <- if (SMOKE) SIM_SIZES[SIM_SIZES <= 2000L] else SIM_SIZES
    sims <- as.vector(outer(sizes, c("classification", "regression"),
                            function(n, t) sprintf("sim:%d:%s", n, t)))
    g$scaling <- expand.grid(arm = "scaling", data = sims, scenario = "compare",
                             framework = FRAMEWORKS, seed = SEEDS,
                             stringsAsFactors = FALSE)
  }
  grid <- do.call(rbind, g)
  grid$id <- with(grid, sprintf("%s__%s__%s__%s__%d", arm, gsub(":", "-", data),
                                scenario, framework, seed))
  if (nzchar(ONLY)) grid <- grid[grepl(ONLY, grid$id), ]
  rownames(grid) <- NULL
  grid
}

run_one <- function(g) {
  out <- file.path(RUNS, g$id)
  if (file.exists(file.path(out, "done.txt"))) return(invisible("skipped"))
  unlink(out, recursive = TRUE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(EXP, "R", "workflows", sprintf("%s_%s.R", g$scenario, g$framework))
  cmd <- sprintf(paste(
    "COMPACT_DATA=%s COMPACT_SCENARIO=%s COMPACT_SEED=%d COMPACT_OUT=%s",
    "OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 R_RANGER_NUM_THREADS=1",
    "timeout %d /usr/bin/time -v -o %s Rscript %s > %s 2>&1"),
    shQuote(g$data), g$scenario, g$seed, shQuote(out), TIMEOUT,
    shQuote(file.path(out, "time.txt")), shQuote(script), shQuote(file.path(out, "log.txt")))
  status <- system(cmd)
  ok <- status == 0 && file.exists(file.path(out, "metrics.csv"))
  writeLines(as.character(status), file.path(out, if (ok) "done.txt" else "failed.txt"))
  invisible(if (ok) "ok" else "failed")
}

read_run <- function(g) {
  out <- file.path(RUNS, g$id)
  if (!file.exists(file.path(out, "done.txt"))) {
    na <- NA_real_
    return(list(op = data.frame(g, status = "failed", selected = NA_character_,
                                n_train = na, n_test = na, p = na, library_seconds = na,
                                setup_seconds = na, workflow_seconds = na, total_seconds = na,
                                peak_memory_mib = na, packages_attached = na),
                perf = NULL))
  }
  m <- utils::read.csv(file.path(out, "metrics.csv"))
  tm <- utils::read.csv(file.path(out, "timings.csv"))
  tl <- readLines(file.path(out, "time.txt"))
  peak_kb <- as.numeric(sub(".*: ", "", grep("Maximum resident set size", tl, value = TRUE)))
  pk <- setdiff(readLines(file.path(out, "packages.txt")), DEFAULT_PKGS)
  op <- data.frame(g, status = "ok", selected = m$selected,
                   n_train = m$n_train, n_test = m$n_test, p = m$p,
                   library_seconds = tm$library_seconds, setup_seconds = tm$setup_seconds,
                   workflow_seconds = tm$workflow_seconds,
                   total_seconds = tm$library_seconds + tm$setup_seconds + tm$workflow_seconds,
                   peak_memory_mib = peak_kb / 1024, packages_attached = length(pk))
  pc <- setdiff(names(m), c("selected", "n_train", "n_test", "p"))
  perf <- data.frame(g[rep(1, length(pc)), c("arm", "data", "scenario", "framework", "seed", "id")],
                     metric = pc, value = as.numeric(m[1, pc]), row.names = NULL)
  list(op = op, perf = perf)
}

main <- function() {
  dir.create(RUNS, recursive = TRUE, showWarnings = FALSE)
  grid <- build_grid()
  message(sprintf("%d runs (%s arm, %d seed%s, %d job%s) -> %s", nrow(grid), ARM, REPS,
                  if (REPS > 1) "s" else "", JOBS, if (JOBS > 1) "s" else "", RESULTS))
  t0 <- Sys.time()
  step <- function(i) {
    res <- run_one(grid[i, ])
    message(sprintf("[%4d/%4d] %-8s %s  (%.1f min elapsed)", i, nrow(grid), res, grid$id[i],
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    res
  }
  status <- if (JOBS > 1) {
    unlist(parallel::mclapply(seq_len(nrow(grid)), step, mc.cores = JOBS,
                              mc.preschedule = FALSE))
  } else {
    vapply(seq_len(nrow(grid)), step, character(1))
  }
  rows <- lapply(seq_len(nrow(grid)), function(i) read_run(grid[i, ]))
  op <- do.call(rbind, lapply(rows, `[[`, "op"))
  perf <- do.call(rbind, lapply(rows, `[[`, "perf"))
  utils::write.csv(op, file.path(RESULTS, "runs.csv"), row.names = FALSE)
  if (!is.null(perf)) utils::write.csv(perf, file.path(RESULTS, "performance.csv"), row.names = FALSE)
  failed <- op$id[op$status != "ok"]
  writeLines(failed, file.path(RESULTS, "failed.txt"))
  message(sprintf("done: %d ok, %d failed (see %s/failed.txt and runs/<id>/log.txt)",
                  sum(op$status == "ok"), length(failed), RESULTS))
}

if (sys.nframe() == 0) main()
