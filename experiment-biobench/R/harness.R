# ---------------------------------------------------------------------------
# harness.R
#
# Shared, framework-neutral harness. Nothing in this file is charged to any
# framework's compactness score. It fixes the data contract (dataset
# preparation, frozen split, candidate learners, hyperparameters, tuning grid)
# and the test-set scoring formulas, so that the framework workflows differ
# only in the code each framework needs.
#
# A workflow script asks for its data through load_context(), which reads:
#   COMPACT_DATA      "biobench:<name>"  or  "sim:<n>:<classification|regression>"
#   COMPACT_SCENARIO  compare | preprocess | tune
#   COMPACT_SEED      run seed (CV folds, learners)
#   COMPACT_OUT       output directory of the run
# ---------------------------------------------------------------------------

EXP_DIR <- "experiment-biobench"
SPLIT_SEED <- 42L
CV_FOLDS <- 5L
MIN_LEVEL_N <- 20L   # factor levels rarer than this are merged into "other"

# Prespecified candidate learners and hyperparameters, held constant across
# frameworks (compare and preprocess scenarios).
LEARNER_SPECS <- list(
  glmnet = list(alpha = 1, lambda = 0.01),
  random_forest = list(num.trees = 500L, min.node.size = 1L, num.threads = 1L),
  xgboost = list(nrounds = 100L, max_depth = 6L, eta = 0.3, gamma = 0,
                 min_child_weight = 1, subsample = 1, colsample_bytree = 1,
                 nthread = 1L)
)
CANDIDATES <- names(LEARNER_SPECS)

# Common tuning grid for xgboost (tune scenario): 12 configurations, every
# framework evaluates exactly these on the same CV folds (make_folds), so the
# three tuning workflows should select the same configuration.
TUNE_GRID <- expand.grid(nrounds = c(50L, 150L), max_depth = c(2L, 4L, 6L),
                         eta = c(0.05, 0.3))

# Which data form each scenario receives.
#   ready: imputed (training medians and modes) and dummy-encoded, all numeric
#   raw:   missing values and factors left in place; the workflow owns them
SCENARIO_DATA <- c(compare = "ready", preprocess = "raw", tune = "ready")

# ---------------------------------------------------------------------------
# biobench datasets (registry and preparation copied from project-biobench)
# ---------------------------------------------------------------------------

load_registry <- function() {
  suppressPackageStartupMessages(library(biobench))
  source(file.path(EXP_DIR, "R", "biobench_registry.R"), local = globalenv())
  source(file.path(EXP_DIR, "R", "biobench_prepare.R"), local = globalenv())
}

# Character, logical, and factor predictors become factors with syntactic
# level names; rare levels are merged; constant columns are dropped; integer
# predictors become double so every framework imputes the same median.
clean_predictors <- function(X) {
  for (v in names(X)) {
    x <- X[[v]]
    if (is.character(x) || is.logical(x) || is.factor(x)) {
      x <- as.character(x)
      ok <- !is.na(x)
      x[ok] <- make.names(x[ok])
      tab <- table(x)
      x[ok & x %in% names(tab)[tab < MIN_LEVEL_N]] <- "other"
      n_other <- sum(x == "other", na.rm = TRUE)
      if (n_other > 0 && n_other < MIN_LEVEL_N) {
        # merged level still too rare: fold it into the most frequent level
        tab <- table(x[x != "other"])
        if (length(tab)) x[ok & x == "other"] <- names(which.max(tab))
      }
      x <- factor(x)
    } else {
      x <- as.double(x)
    }
    X[[v]] <- x
  }
  keep <- vapply(X, function(x) length(unique(x[!is.na(x)])) > 1, logical(1))
  X <- X[, keep, drop = FALSE]
  names(X) <- make.names(names(X), unique = TRUE)
  X
}

biobench_frame <- function(name) {
  load_registry()
  p <- prepare(name)
  X <- clean_predictors(p$X)
  y <- p$y
  if (p$spec$task == "classification") {
    lv <- levels(droplevels(factor(y)))
    stopifnot(length(lv) == 2L)
    # positive class = second level of the source coding (e.g. pos, Yes, Died)
    y <- factor(ifelse(as.character(y) == lv[2], "Yes", "No"), levels = c("No", "Yes"))
  } else {
    y <- as.double(y)
  }
  outcome <- if (".y" %in% names(X)) ".outcome" else ".y"
  d <- X
  d[[outcome]] <- y
  list(data = d, outcome = outcome, task = p$spec$task)
}

# ---------------------------------------------------------------------------
# Simulated data for the size-scaling arm
# ---------------------------------------------------------------------------

sim_frame <- function(n, task) {
  set.seed(SPLIT_SEED + n)
  p <- 10L
  X <- as.data.frame(matrix(stats::rnorm(n * p), n, p))
  names(X) <- sprintf("x%02d", seq_len(p))
  eta <- 0.8 * X$x01 - 0.6 * X$x02 + 0.5 * X$x03 * X$x04 + 0.4 * (X$x05^2 - 1) +
    0.3 * X$x06
  d <- X
  if (task == "classification") {
    d$.y <- factor(ifelse(stats::runif(n) < stats::plogis(eta - 0.5), "Yes", "No"),
                   levels = c("No", "Yes"))
  } else {
    d$.y <- eta + stats::rnorm(n)
  }
  list(data = d, outcome = ".y", task = task)
}

# ---------------------------------------------------------------------------
# Frozen split and the two data forms
# ---------------------------------------------------------------------------

make_split <- function(d, outcome, task) {
  set.seed(SPLIT_SEED)
  ids <- seq_len(nrow(d))
  strata <- if (task == "classification") d[[outcome]] else rep(1L, nrow(d))
  train_ids <- unlist(lapply(split(ids, strata), function(i) {
    i[sample.int(length(i), floor(0.8 * length(i)))]
  }), use.names = FALSE)
  list(train = d[sort(train_ids), , drop = FALSE],
       test = d[-train_ids, , drop = FALSE])
}

impute_like <- function(x, ref) {
  for (v in names(x)) {
    if (!anyNA(x[[v]])) next
    fill <- if (is.numeric(ref[[v]])) stats::median(ref[[v]], na.rm = TRUE) else
      names(which.max(table(ref[[v]])))
    x[[v]][is.na(x[[v]])] <- fill
  }
  x
}

# Training-statistics imputation and treatment (dummy) coding; all numeric.
make_ready <- function(parts, outcome) {
  pred <- setdiff(names(parts$train), outcome)
  tr <- impute_like(parts$train[, pred, drop = FALSE], parts$train[, pred, drop = FALSE])
  te <- impute_like(parts$test[, pred, drop = FALSE], parts$train[, pred, drop = FALSE])
  tt <- stats::terms(~ ., data = tr)
  mm_tr <- stats::model.matrix(tt, tr)
  xlev <- lapply(Filter(is.factor, tr), levels)
  mm_te <- stats::model.matrix(tt, stats::model.frame(tt, te, xlev = xlev))
  keep <- colnames(mm_tr) != "(Intercept)"
  keep <- keep & apply(mm_tr, 2, function(z) length(unique(z)) > 1)
  nm <- make.names(colnames(mm_tr)[keep], unique = TRUE)
  out_tr <- stats::setNames(as.data.frame(mm_tr[, keep, drop = FALSE]), nm)
  out_te <- stats::setNames(as.data.frame(mm_te[, keep, drop = FALSE]), nm)
  out_tr[[outcome]] <- parts$train[[outcome]]
  out_te[[outcome]] <- parts$test[[outcome]]
  rownames(out_tr) <- NULL; rownames(out_te) <- NULL
  list(train = out_tr, test = out_te)
}

# Shared CV fold assignment for the tune scenario (stratified on the outcome
# for classification), so every framework scores the grid on the same folds.
make_folds <- function(y, v, seed) {
  set.seed(seed)
  strata <- if (is.factor(y)) y else rep(1L, length(y))
  folds <- integer(length(y))
  for (i in split(seq_along(y), strata)) folds[i] <- sample(rep_len(seq_len(v), length(i)))
  folds
}

parse_data_id <- function(id) {
  f <- strsplit(id, ":", fixed = TRUE)[[1]]
  if (f[1] == "biobench") return(biobench_frame(f[2]))
  if (f[1] == "sim") return(sim_frame(as.integer(f[2]), f[3]))
  stop("unknown data id: ", id)
}

# Everything a workflow script receives before its measured region.
load_context <- function() {
  data_id <- Sys.getenv("COMPACT_DATA", "biobench:pima2")
  scenario <- Sys.getenv("COMPACT_SCENARIO", "compare")
  fr <- parse_data_id(data_id)
  parts <- make_split(fr$data, fr$outcome, fr$task)
  if (SCENARIO_DATA[[scenario]] == "ready") parts <- make_ready(parts, fr$outcome)
  # Predictors in alphabetical order: mlr3 sorts task features, and xgboost
  # breaks split ties by column order, so all frameworks get the same order.
  ord <- c(sort(setdiff(names(parts$train), fr$outcome)), fr$outcome)
  parts <- lapply(parts, function(x) x[, ord, drop = FALSE])
  out_dir <- Sys.getenv("COMPACT_OUT", unset = file.path(tempdir(), "compact"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  seed <- as.integer(Sys.getenv("COMPACT_SEED", "1001"))
  list(train = parts$train, test = parts$test, outcome = fr$outcome,
       task = fr$task, is_classif = fr$task == "classification",
       form = stats::reformulate(".", response = fr$outcome),
       folds = make_folds(parts$train[[fr$outcome]], CV_FOLDS, seed),
       seed = seed, out_dir = out_dir, data_id = data_id, scenario = scenario)
}

# ---------------------------------------------------------------------------
# Shared test-set scoring (identical formulas for every framework)
# ---------------------------------------------------------------------------

score_classification <- function(truth, prob_yes) {
  stopifnot(is.factor(truth), identical(levels(truth), c("No", "Yes")))
  y <- as.integer(truth == "Yes")
  n1 <- sum(y == 1L); n0 <- sum(y == 0L)
  r <- rank(prob_yes, ties.method = "average")
  auc <- (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  pred <- ifelse(prob_yes >= 0.5, "Yes", "No")
  sens <- sum(pred == "Yes" & truth == "Yes") / n1
  spec <- sum(pred == "No" & truth == "No") / n0
  p <- pmin(pmax(prob_yes, 1e-6), 1 - 1e-6)
  cal <- tryCatch(stats::coef(stats::glm(y ~ stats::qlogis(p),
                                         family = stats::binomial()))[[2]],
                  error = function(e) NA_real_)
  data.frame(auc = auc, accuracy = mean(pred == truth),
             balanced_accuracy = (sens + spec) / 2,
             brier = mean((y - prob_yes)^2), calibration_slope = cal)
}

score_regression <- function(truth, pred) {
  err <- truth - pred
  data.frame(rmse = sqrt(mean(err^2)), mae = mean(abs(err)),
             r_squared = 1 - sum(err^2) / sum((truth - mean(truth))^2),
             bias = mean(pred - truth))
}

# ---------------------------------------------------------------------------
# Called before and after the measured region of every workflow script
# ---------------------------------------------------------------------------

record_packages <- function(ctx) {
  writeLines(setdiff(.packages(), "base"), file.path(ctx$out_dir, "packages.txt"))
}

finish <- function(ctx, selected, test_pred, marks) {
  truth <- ctx$test[[ctx$outcome]]
  metrics <- if (ctx$is_classif) score_classification(truth, as.numeric(test_pred)) else
    score_regression(truth, as.numeric(test_pred))
  metrics$selected <- paste(selected, collapse = "/")
  metrics$n_train <- nrow(ctx$train)
  metrics$n_test <- nrow(ctx$test)
  metrics$p <- ncol(ctx$train) - 1L
  utils::write.csv(metrics, file.path(ctx$out_dir, "metrics.csv"), row.names = FALSE)
  timings <- data.frame(
    library_seconds = unname(marks["t_loaded"] - marks["t0"]),
    setup_seconds = unname(marks["t_wf_start"] - marks["t_loaded"]),
    workflow_seconds = unname(marks["t_wf_end"] - marks["t_wf_start"])
  )
  utils::write.csv(timings, file.path(ctx$out_dir, "timings.csv"), row.names = FALSE)
  invisible(metrics)
}
