# preprocess scenario, mlr3: raw data with missing values and factors.
# Median/mode imputation and dummy coding run as a pipeline inside each CV
# fold and before the refit.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages({
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
})
source(file.path("experiment-biobench", "R", "harness.R"))
.M["t_loaded"] <- proc.time()[["elapsed"]]
lgr::get_logger("mlr3")$set_threshold("warn")  # measurement noise control

ctx <- load_context()
train <- ctx$train; test <- ctx$test; outcome <- ctx$outcome
seed <- ctx$seed; is_classif <- ctx$is_classif
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
set.seed(seed)
tt <- if (is_classif) {
  as_task_classif(train, target = outcome, positive = "Yes")
} else {
  as_task_regr(train, target = outcome)
}
if (is_classif) tt$set_col_roles(outcome, add_to = "stratum")
prep <- po("imputemedian") %>>% po("imputemode") %>>% po("encode", method = "treatment")
learners <- list(
  lrn(if (is_classif) "classif.glmnet" else "regr.glmnet",
      alpha = 1, lambda = 0.01),
  lrn(if (is_classif) "classif.ranger" else "regr.ranger",
      num.trees = 500, min.node.size = 1, num.threads = 1),
  lrn(if (is_classif) "classif.xgboost" else "regr.xgboost",
      nrounds = 100, max_depth = 6, eta = 0.3, nthread = 1)
)
learners <- lapply(learners, function(l) as_learner(prep %>>% l))
names(learners) <- CANDIDATES
if (is_classif) for (l in learners) l$predict_type <- "prob"
resampling <- rsmp("cv", folds = CV_FOLDS)$instantiate(tt)
measure <- msr(if (is_classif) "classif.auc" else "regr.rmse")
cv_score <- vapply(learners, function(l) {
  resample(tt, l, resampling)$aggregate(measure)
}, numeric(1))
best <- names(cv_score)[if (is_classif) which.max(cv_score) else which.min(cv_score)]
final <- learners[[best]]$train(tt)
new_tt <- if (is_classif) {
  as_task_classif(test, target = outcome, positive = "Yes")
} else {
  as_task_regr(test, target = outcome)
}
prediction <- final$predict(new_tt)
pred <- if (is_classif) prediction$prob[, "Yes"] else prediction$response
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
finish(ctx, best, pred, .M)
