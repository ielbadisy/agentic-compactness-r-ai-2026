# tune scenario, mlr3: grid search over the common 12-point xgboost grid with
# the shared 5 CV folds (model-ready data), refit of the best configuration, test
# prediction.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages({
  library(mlr3)
  library(mlr3learners)
  library(mlr3tuning)
})
source(file.path("experiment-biobench", "R", "harness.R"))
.M["t_loaded"] <- proc.time()[["elapsed"]]
lgr::get_logger("mlr3")$set_threshold("warn")  # measurement noise control
lgr::get_logger("bbotk")$set_threshold("warn")

ctx <- load_context()
train <- ctx$train; test <- ctx$test; outcome <- ctx$outcome
folds <- ctx$folds; seed <- ctx$seed; is_classif <- ctx$is_classif
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
set.seed(seed)
tt <- if (is_classif) {
  as_task_classif(train, target = outcome, positive = "Yes")
} else {
  as_task_regr(train, target = outcome)
}
rs <- rsmp("custom_cv")
rs$instantiate(tt, f = factor(folds))
learner <- lrn(if (is_classif) "classif.xgboost" else "regr.xgboost", nthread = 1)
if (is_classif) learner$predict_type <- "prob"
at <- auto_tuner(
  tuner = tnr("design_points", design = data.table::as.data.table(TUNE_GRID)),
  learner = learner,
  resampling = rs,
  measure = msr(if (is_classif) "classif.auc" else "regr.rmse"),
  search_space = ps(nrounds = p_int(1, 1000), max_depth = p_int(1, 20),
                    eta = p_dbl(0, 1))
)
at$train(tt)
new_tt <- if (is_classif) {
  as_task_classif(test, target = outcome, positive = "Yes")
} else {
  as_task_regr(test, target = outcome)
}
prediction <- at$predict(new_tt)
pred <- if (is_classif) prediction$prob[, "Yes"] else prediction$response
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
best <- at$tuning_result[, names(TUNE_GRID), with = FALSE]
finish(ctx, sprintf("%s=%s", names(best), unlist(best)), pred, .M)
