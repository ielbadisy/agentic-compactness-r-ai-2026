# ---------------------------------------------------------------------------
# datasets.R
#
# Applies the dataset inclusion rule to the biobench registry and writes
# experiment-biobench/results/datasets.csv (one row per included dataset, plus
# the excluded ones with their reason). Run once before run_experiment.R.
#
# Inclusion: registry entry for classification or regression, not an
# additional analysis of another dataset, no grouping (repeated measures need
# grouped CV, which is out of scope), binary outcome for classification,
# at least 50 rows, at least 10 cases in the minority class, and at least two
# predictors (glmnet needs two columns).
#
# Selection: every eligible regression dataset (there are five) and five
# classification datasets chosen to span size (500 to 15,000 rows), missing
# data (0 to 25 percent), and factor predictors (none to many). The other
# eligible classification datasets are listed as "eligible, not selected".
# ---------------------------------------------------------------------------

source(file.path("experiment-biobench", "R", "harness.R"))
load_registry()

MIN_N <- 50L
MIN_MINORITY <- 10L
SELECTED_CLASSIF <- c("glow500", "pima2", "opt", "nhanes", "covidtest")

rows <- lapply(names(registry), function(nm) {
  s <- registry[[nm]]
  base <- data.frame(dataset = nm, task = s$task, n = NA_integer_, p_raw = NA_integer_,
                     p_ready = NA_integer_, pct_missing = NA_real_, n_factor = NA_integer_,
                     minority = NA_integer_, included = FALSE, reason = "")
  if (!s$task %in% c("classification", "regression")) return(NULL)
  if (!is.null(s$source)) { base$reason <- "additional analysis of another dataset"; return(base) }
  if (!is.null(s$group)) { base$reason <- "repeated measures (grouped CV needed)"; return(base) }
  p <- tryCatch(prepare(nm), error = function(e) e)
  if (inherits(p, "error")) { base$reason <- paste("prepare failed:", conditionMessage(p)); return(base) }
  base$n <- length(p$y)
  if (s$task == "classification") {
    tab <- table(droplevels(factor(p$y)))
    base$minority <- min(tab)
    if (length(tab) != 2L) { base$reason <- sprintf("%d classes (binary only)", length(tab)); return(base) }
    if (min(tab) < MIN_MINORITY) { base$reason <- "fewer than 10 minority cases"; return(base) }
  }
  if (base$n < MIN_N) { base$reason <- "fewer than 50 rows"; return(base) }
  fr <- biobench_frame(nm)
  X <- fr$data[, setdiff(names(fr$data), fr$outcome), drop = FALSE]
  base$p_raw <- ncol(X)
  base$pct_missing <- round(100 * mean(is.na(X)), 1)
  base$n_factor <- sum(vapply(X, is.factor, logical(1)))
  parts <- make_ready(make_split(fr$data, fr$outcome, fr$task), fr$outcome)
  base$p_ready <- ncol(parts$train) - 1L
  if (base$p_ready < 2L) { base$reason <- "fewer than two predictors"; return(base) }
  if (s$task == "classification" && !nm %in% SELECTED_CLASSIF) {
    base$reason <- "eligible, not selected"; return(base)
  }
  base$included <- TRUE
  base
})
tab <- do.call(rbind, rows)
tab <- tab[order(!tab$included, tab$task, tab$dataset), ]
out <- file.path(EXP_DIR, "results", "datasets.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(tab, out, row.names = FALSE)
print(tab, row.names = FALSE)
cat(sprintf("\nincluded: %d classification, %d regression -> %s\n",
            sum(tab$included & tab$task == "classification"),
            sum(tab$included & tab$task == "regression"), out))
