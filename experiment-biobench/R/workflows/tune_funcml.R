# tune scenario, funcml: grid search over the common 12-point xgboost grid
# on the shared 5 CV folds (model-ready data), refit of the best configuration, test
# prediction.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages(library(funcml))
source(file.path("experiment-biobench", "R", "harness.R"))
.M["t_loaded"] <- proc.time()[["elapsed"]]

ctx <- load_context()
train <- ctx$train; test <- ctx$test; form <- ctx$form
folds <- ctx$folds; seed <- ctx$seed; is_classif <- ctx$is_classif
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
tuned <- tune(train, form, model = "xgboost", grid = TUNE_GRID,
              resampling = group_cv(v = CV_FOLDS, group = folds),
              metric = if (is_classif) "auc" else "rmse", seed = seed, nthread = 1)
pred <- if (is_classif) {
  predict(tuned$fit_best, newdata = test, type = "prob")[, "Yes"]
} else {
  predict(tuned$fit_best, newdata = test)
}
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
best <- tuned$best[, names(TUNE_GRID)]
finish(ctx, sprintf("%s=%s", names(best), unlist(best)), pred, .M)
