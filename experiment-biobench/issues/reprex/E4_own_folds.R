# E4: with each framework drawing its own 5 CV folds (the original scripts),
# funcml and mlr3 can select different xgboost configurations from the same
# 12-point grid. Run from the paper root.
suppressPackageStartupMessages({library(funcml); library(mlr3); library(mlr3learners); library(mlr3tuning)})
source("experiment-biobench/R/harness.R")
lgr::get_logger("mlr3")$set_threshold("warn"); lgr::get_logger("bbotk")$set_threshold("warn")
for (ds in c("nhanes", "opt")) {
  Sys.setenv(COMPACT_DATA = paste0("biobench:", ds), COMPACT_SCENARIO = "tune")
  ctx <- load_context(); train <- ctx$train; seed <- ctx$seed
  tf <- funcml::tune(train, ctx$form, model = "xgboost", grid = TUNE_GRID,
             resampling = funcml::cv(v = CV_FOLDS, strata = TRUE, seed = seed),
             metric = "auc", seed = seed, nthread = 1)
  set.seed(seed)
  tt <- as_task_classif(train, target = ctx$outcome, positive = "Yes")
  tt$set_col_roles(ctx$outcome, add_to = "stratum")
  l <- lrn("classif.xgboost", nthread = 1, predict_type = "prob")
  at <- auto_tuner(tuner = tnr("design_points", design = data.table::as.data.table(TUNE_GRID)),
                   learner = l, resampling = rsmp("cv", folds = CV_FOLDS),
                   measure = msr("classif.auc"),
                   search_space = ps(nrounds = p_int(1, 1000), max_depth = p_int(1, 20), eta = p_dbl(0, 1)))
  at$train(tt)
  cat(ds, "| funcml:", paste(unlist(tf$best[, names(TUNE_GRID)]), collapse = "/"),
      "| mlr3:", paste(unlist(at$tuning_result[, names(TUNE_GRID), with = FALSE]), collapse = "/"), "\n")
}
