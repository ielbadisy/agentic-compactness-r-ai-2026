# compare scenario, tidymodels: 5-fold CV comparison of three prespecified
# learners on model-ready data, refit of the selected learner, test prediction.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages(library(tidymodels))
source(file.path("experiment-biobench", "R", "harness.R"))
tidymodels_prefer()
.M["t_loaded"] <- proc.time()[["elapsed"]]

ctx <- load_context()
train <- ctx$train; test <- ctx$test; outcome <- ctx$outcome
seed <- ctx$seed; is_classif <- ctx$is_classif
# yardstick treats the first factor level as the event; relevel so the CV
# metric matches the shared scorer's positive class ("Yes").
if (is_classif) train[[outcome]] <- factor(train[[outcome]], levels = c("Yes", "No"))
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
set.seed(seed)
mode <- if (is_classif) "classification" else "regression"
rec <- recipe(stats::reformulate(".", outcome), data = train)
specs <- list(
  glmnet = set_engine(
    if (is_classif) logistic_reg(penalty = 0.01, mixture = 1)
    else linear_reg(penalty = 0.01, mixture = 1), "glmnet"),
  random_forest = set_mode(
    set_engine(rand_forest(trees = 500, min_n = 1), "ranger", num.threads = 1), mode),
  xgboost = set_mode(
    set_engine(boost_tree(trees = 100, tree_depth = 6, learn_rate = 0.3),
               "xgboost", nthread = 1), mode)
)
folds <- vfold_cv(train, v = CV_FOLDS, strata = all_of(outcome))
metric <- metric_set(if (is_classif) roc_auc else rmse)
cv_score <- vapply(specs, function(s) {
  collect_metrics(fit_resamples(workflow(rec, s), folds, metrics = metric))$mean[1]
}, numeric(1))
best <- names(cv_score)[if (is_classif) which.max(cv_score) else which.min(cv_score)]
final <- fit(workflow(rec, specs[[best]]), data = train)
pred <- if (is_classif) {
  predict(final, test, type = "prob")$.pred_Yes
} else {
  predict(final, test)$.pred
}
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
finish(ctx, best, pred, .M)
