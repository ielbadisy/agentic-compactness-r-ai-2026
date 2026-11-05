# preprocess scenario, funcml: raw data with missing values and factors.
# Median/mode imputation is learned inside each CV fold and on the full
# training set before the refit; dummy coding is done by the model formula.
# funcml has no preprocessing step, so the workflow writes its own fold loop.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages(library(funcml))
source(file.path("experiment-biobench", "R", "harness.R"))
.M["t_loaded"] <- proc.time()[["elapsed"]]

ctx <- load_context()
train <- ctx$train; test <- ctx$test; form <- ctx$form; outcome <- ctx$outcome
seed <- ctx$seed; is_classif <- ctx$is_classif
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
specs <- list(glmnet = LEARNER_SPECS$glmnet,
              ranger = LEARNER_SPECS$random_forest,
              xgboost = LEARNER_SPECS$xgboost)
impute <- function(x, ref) {
  for (v in names(x)) {
    fill <- if (is.numeric(ref[[v]])) median(ref[[v]], na.rm = TRUE) else
      names(which.max(table(ref[[v]])))
    x[[v]][is.na(x[[v]])] <- fill
  }
  x
}
set.seed(seed)
strata <- if (is_classif) train[[outcome]] else rep(1, nrow(train))
fold <- ave(seq_len(nrow(train)), strata,
            FUN = function(i) sample(rep_len(seq_len(CV_FOLDS), length(i))))
cv_score <- sapply(names(specs), function(m) mean(sapply(seq_len(CV_FOLDS), function(k) {
  tr <- train[fold != k, ]
  te <- impute(train[fold == k, ], tr)
  model <- fit(form, data = impute(tr, tr), model = m, spec = specs[[m]], seed = seed)
  if (is_classif) auc(te[[outcome]], predict(model, newdata = te, type = "prob")[, "Yes"])
  else rmse(te[[outcome]], predict(model, newdata = te))
})))
best <- names(cv_score)[if (is_classif) which.max(cv_score) else which.min(cv_score)]
model <- fit(form, data = impute(train, train), model = best, spec = specs[[best]], seed = seed)
pred <- if (is_classif) {
  predict(model, newdata = impute(test, train), type = "prob")[, "Yes"]
} else {
  predict(model, newdata = impute(test, train))
}
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
finish(ctx, best, pred, .M)
