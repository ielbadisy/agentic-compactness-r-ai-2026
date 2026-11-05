# Copied verbatim from project-biobench/R/00-prepare.R.

# Shared dataset preparation (used by the feature table and the benchmark runners).
# Requires R/00-registry.R to be sourced and biobench to be attached.
`%||%` <- function(a, b) if (is.null(a)) b else a
# an entry that is an additional analysis of another dataset (registry `source`) reads that dataset
# Data are asked from biobench explicitly. get(name, envir = asNamespace("biobench")) does not find lazy data and
# falls through to the search path, where an attached package can shadow the name (funcml has its own `heartfailure`).
raw_data <- function(nm) getExportedValue("biobench", registry[[nm]]$source %||% nm)

# ---- type cleaning shared by every dataset -----------------------------------
# Empty strings and "." (SAS missing marker) become NA; text columns that are mostly numeric (>= 90% of
# non-missing values parse as numbers and more than 10 distinct values) become numeric.
clean_types <- function(X) {
  n_coerced <- 0L
  for (j in seq_along(X)) {
    v <- X[[j]]
    if (is.character(v) || is.factor(v)) {
      v <- as.character(v); v[trimws(v) %in% c("", ".")] <- NA
      num <- suppressWarnings(as.numeric(v)); ok <- !is.na(v)
      if (any(ok) && mean(!is.na(num[ok])) >= 0.9 && length(unique(v[ok])) > 10) { X[[j]] <- num; n_coerced <- n_coerced + 1L }
      else X[[j]] <- v
    }
  }
  attr(X, "n_coerced") <- n_coerced
  X
}

# ---- prepare one dataset according to the registry --------------------------
prepare <- function(nm) {
  s <- registry[[nm]]
  d <- raw_data(nm)
  n_raw <- nrow(d)
  if (!is.null(s$transform)) d <- s$transform(d)
  out <- list(spec = s, n_after_transform = nrow(d))
  if (s$task == "survival") {
    tm <- suppressWarnings(as.numeric(d[[s$time]]))
    ev_raw <- as.character(d[[s$event]])
    ev <- as.integer(ev_raw %in% as.character(s$event_is))
    ok <- !is.na(tm) & !is.na(ev_raw) & tm > 0
    out$time <- tm[ok]; out$event <- ev[ok]
    out$n_dropped_outcome <- sum(!ok)
    outcome_cols <- c(s$time, s$event)
  } else {
    yv <- d[[s$y]]
    if (s$task == "classification") { yv <- factor(yv); yv <- droplevels(yv) }
    ok <- !is.na(yv) & (if (is.factor(yv)) nzchar(as.character(yv)) else TRUE)
    out$y <- yv[ok]; out$n_dropped_outcome <- sum(!ok)
    outcome_cols <- s$y
  }
  # keep only rows with valid outcome, then choose predictors
  d <- d[ok, , drop = FALSE]
  grp <- if (!is.null(s$group)) d[[s$group]] else NULL
  preds <- if (!is.null(s$keep)) s$keep else setdiff(names(d), c(outcome_cols, s$drop, s$group))
  # drop derived helper outcome columns and any column that is all NA
  preds <- setdiff(preds, c("throat_pain_30min", "preterm", "case", "pacu30min_throatPain", "GA.at.outcome"))
  X <- d[, preds, drop = FALSE]
  X <- clean_types(X)
  nc <- attr(X, "n_coerced")
  X <- X[, colSums(!is.na(X)) > 0, drop = FALSE]
  out$n_coerced_numeric <- nc
  out$X <- X; out$group <- grp; out$n_raw <- n_raw
  out
}

