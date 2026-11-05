# E1: yardstick::metric_set() names each metric after the expression it was
# given, so a conditional choice of metric gets a non-matching name and
# tune::select_best(metric = "rmse") cannot find it.
suppressPackageStartupMessages(library(yardstick))
is_classif <- FALSE
ms <- metric_set(if (is_classif) roc_auc else rmse)
print(names(attr(ms, "metrics")))      # not "rmse"
ok <- if (is_classif) metric_set(roc_auc) else metric_set(rmse)
print(names(attr(ok, "metrics")))      # "rmse"
cat("yardstick", as.character(packageVersion("yardstick")), "\n")
