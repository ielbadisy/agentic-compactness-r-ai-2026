# tune scenario, tidymodels: grid search over the common 12-point xgboost grid
# on the shared 5 CV folds (model-ready data), refit of the best configuration, test
# prediction.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages(library(tidymodels))
source(file.path("experiment-biobench", "R", "harness.R"))
tidymodels_prefer()
.M["t_loaded"] <- proc.time()[["elapsed"]]

ctx <- load_context()
train <- ctx$train; test <- ctx$test; outcome <- ctx$outcome
folds <- ctx$folds; seed <- ctx$seed; is_classif <- ctx$is_classif
# yardstick treats the first factor level as the event; relevel so the CV
# metric matches the shared scorer's positive class ("Yes").
if (is_classif) train[[outcome]] <- factor(train[[outcome]], levels = c("Yes", "No"))
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
set.seed(seed)
spec <- set_mode(
  set_engine(boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()),
             "xgboost", nthread = 1),
  if (is_classif) "classification" else "regression")
wf <- workflow(recipe(stats::reformulate(".", outcome), data = train), spec)
grid <- data.frame(trees = TUNE_GRID$nrounds, tree_depth = TUNE_GRID$max_depth,
                   learn_rate = TUNE_GRID$eta)
metric <- if (is_classif) "roc_auc" else "rmse"
metrics <- if (is_classif) metric_set(roc_auc) else metric_set(rmse)
rs <- manual_rset(lapply(seq_len(CV_FOLDS), function(k) {
  make_splits(list(analysis = which(folds != k), assessment = which(folds == k)), train)
}), paste0("Fold", seq_len(CV_FOLDS)))
res <- tune_grid(wf, rs, grid = grid, metrics = metrics)
best <- select_best(res, metric = metric)
final <- fit(finalize_workflow(wf, best), data = train)
pred <- if (is_classif) {
  predict(final, test, type = "prob")$.pred_Yes
} else {
  predict(final, test)$.pred
}
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
best <- stats::setNames(best[, c("trees", "tree_depth", "learn_rate")], names(TUNE_GRID))
finish(ctx, sprintf("%s=%s", names(best), unlist(best)), pred, .M)
