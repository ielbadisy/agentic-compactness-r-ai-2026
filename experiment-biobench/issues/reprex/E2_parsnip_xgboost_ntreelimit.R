# E2: parsnip 1.3.3 still passed ntreelimit to xgboost when predicting.
# xgboost 3.x removed that argument, so every tuning fit failed with
# "Passed both iterationrange and ntreelimit". tune_grid() only reported
# the metric error of E1, which hid this one.
# Fix: update parsnip (>= 1.6.1) and tune (>= 2.1.0); no code change.
# With parsnip 1.3.3 and xgboost >= 3.0 the fits below fail;
# with the fixed versions they succeed.
# Run from the paper root: Rscript experiment-biobench/issues/reprex/E2_parsnip_xgboost_ntreelimit.R
suppressPackageStartupMessages({
  library(parsnip); library(tune); library(rsample); library(yardstick)
})
for (p in c("parsnip", "tune", "xgboost"))
  cat(p, as.character(packageVersion(p)), "\n")
set.seed(1)
d <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
d$y <- d$x1 + rnorm(200)
spec <- boost_tree(trees = 20, tree_depth = tune()) |>
  set_engine("xgboost", nthread = 1) |>
  set_mode("regression")
res <- tune_grid(spec, y ~ x1 + x2, resamples = vfold_cv(d, v = 3),
                 grid = data.frame(tree_depth = c(2, 4)),
                 metrics = metric_set(rmse))
notes <- collect_notes(res)
if (nrow(notes) == 0) {
  cat("All fits succeeded:\n"); print(show_best(res, metric = "rmse"))
} else {
  cat("Fits failed:\n"); print(unique(notes$note))
}
