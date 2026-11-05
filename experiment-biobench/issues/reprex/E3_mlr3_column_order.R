# E3: mlr3 Task sorts feature columns alphabetically; xgboost split tie-breaking
# depends on column order, so the same configuration gives different trees.
# Run from the paper root.
suppressPackageStartupMessages({library(mlr3); library(mlr3learners); library(xgboost)})
source("experiment-biobench/R/harness.R")
Sys.setenv(COMPACT_DATA="biobench:aidsdelay", COMPACT_SCENARIO="tune")
ctx <- load_context(); tr <- ctx$train; te <- ctx$test; y <- ctx$outcome
tt <- as_task_regr(tr, target = y)
cat("data order :", setdiff(names(tr), y), "\nmlr3 order :", tt$feature_names, "\n")
fitp <- function(cols) { p <- list(max_depth=2, eta=0.3, nthread=1, objective="reg:squarederror")
  b <- xgb.train(p, xgb.DMatrix(as.matrix(tr[cols]), label=tr[[y]]), nrounds=150)
  predict(b, as.matrix(te[cols])) }
a <- fitp(setdiff(names(tr), y)); b <- fitp(tt$feature_names)
l <- lrn("regr.xgboost", nrounds=150, max_depth=2, eta=0.3, nthread=1); l$train(tt)
m <- l$predict_newdata(te)$response
rmse <- function(p) sqrt(mean((te[[y]]-p)^2))
cat(sprintf("raw xgboost, data order : %.4f\nraw xgboost, sorted     : %.4f\nmlr3                    : %.4f\n", rmse(a), rmse(b), rmse(m)))
