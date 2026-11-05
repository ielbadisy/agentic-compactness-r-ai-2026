# ---------------------------------------------------------------------------
# measure_compactness.R (biobench experiment)
#
# Static counts on the measured region ("# >>> workflow" to "# <<< workflow")
# of the 9 workflow scripts (3 scenarios x 3 frameworks). Same metric
# definitions as experiment/R/measure_compactness.R; the namespace and
# constructor lists are extended to the packages these scripts use.
# Run from the paper root.
# ---------------------------------------------------------------------------

source("experiment/R/measure_compactness.R")  # extract_region(), measure_one()

FRAMEWORK_NS$mlr3 <- c(FRAMEWORK_NS$mlr3, "mlr3tuning", "mlr3pipelines", "bbotk")
FRAMEWORK_NS$tidymodels <- c(FRAMEWORK_NS$tidymodels, "tidymodels")
CONSTRUCTORS$mlr3 <- c(CONSTRUCTORS$mlr3, "ps", "p_int", "p_dbl")
CONSTRUCTORS$tidymodels <- c(CONSTRUCTORS$tidymodels, "manual_rset", "make_splits",
                             "step_impute_median", "step_impute_mode", "step_dummy",
                             "finalize_workflow")

main_biobench <- function() {
  suppressPackageStartupMessages({
    library(funcml); library(mlr3); library(mlr3learners); library(mlr3tuning)
    library(mlr3pipelines); library(tidymodels)
  })
  grid <- expand.grid(framework = c("funcml", "mlr3", "tidymodels"),
                      scenario = c("compare", "preprocess", "tune"),
                      stringsAsFactors = FALSE)
  res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    path <- file.path("experiment-biobench", "R", "workflows",
                      sprintf("%s_%s.R", grid$scenario[i], grid$framework[i]))
    cbind(scenario = grid$scenario[i], measure_one(grid$framework[i], path))
  }))
  out <- "experiment-biobench/results/compactness_static.csv"
  utils::write.csv(res, out, row.names = FALSE)
  print(res, row.names = FALSE)
}

main_biobench()
