# ---------------------------------------------------------------------------
# measure_compactness.R
#
# Static, deterministic measurement of the workflow region (between the
# "# >>> workflow" and "# <<< workflow" markers) of each framework script.
# Reports the structural compactness metrics used in the paper. Re-runnable;
# depends only on the script text and the framework namespaces.
# ---------------------------------------------------------------------------

FRAMEWORK_NS <- list(
  funcml = "funcml",
  mlr3 = c("mlr3", "mlr3learners", "mlr3measures", "mlr3misc", "paradox"),
  tidymodels = c("parsnip", "recipes", "workflows", "rsample", "tune",
                 "yardstick", "dials", "hardhat", "workflowsets", "generics")
)

# Calls that construct a framework object / container / class instance.
CONSTRUCTORS <- list(
  funcml = c("cv", "holdout", "group_cv", "time_cv", "fit", "compare",
             "evaluate", "tune"),
  mlr3 = c("as_task_classif", "as_task_regr", "tsk", "lrn", "as_learner",
           "rsmp", "msr", "po", "ppl", "as_pipeop", "auto_tuner", "tnr",
           "trm", "resample", "benchmark"),
  tidymodels = c("recipe", "workflow", "vfold_cv", "initial_split",
                 "logistic_reg", "linear_reg", "rand_forest", "boost_tree",
                 "set_engine", "set_mode", "metric_set", "fit_resamples",
                 "control_resamples", "tune_grid", "add_recipe", "add_model")
)

extract_region <- function(path) {
  lines <- readLines(path, warn = FALSE)
  a <- grep("^\\s*#\\s*>>> workflow", lines)
  b <- grep("^\\s*#\\s*<<< workflow", lines)
  stopifnot(length(a) == 1, length(b) == 1, b > a)
  lines[(a + 1):(b - 1)]
}

is_code_line <- function(l) {
  s <- trimws(l)
  nzchar(s) && !startsWith(s, "#")
}

measure_one <- function(framework, path) {
  region <- extract_region(path)
  code <- region[vapply(region, is_code_line, logical(1))]
  expr <- parse(text = paste(region, collapse = "\n"), keep.source = TRUE)
  pd <- utils::getParseData(expr)

  fn_calls <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]
  # R6 / reference-object method calls: `obj$method(...)`
  dollar_idx <- which(pd$token == "'$'")
  method_calls <- character(0)
  for (i in dollar_idx) {
    sibs <- pd[pd$parent == pd$parent[i], ]
    rhs <- sibs$text[sibs$token == "SYMBOL" & sibs$col1 > pd$col1[i]]
    if (length(rhs)) method_calls <- c(method_calls, rhs[1])
  }

  ns <- FRAMEWORK_NS[[framework]]
  exported <- unique(unlist(lapply(ns, function(n) {
    tryCatch(getNamespaceExports(n), error = function(e) character(0))
  })))
  fw_fn <- fn_calls[fn_calls %in% exported]
  fw_method <- if (framework == "mlr3") method_calls else character(0)

  ctor <- CONSTRUCTORS[[framework]]
  n_ctor <- sum(fn_calls %in% ctor)

  data.frame(
    framework = framework,
    workflow_loc = length(code),
    statements = length(expr),
    total_calls = length(fn_calls) + length(method_calls),
    framework_calls = length(fw_fn) + length(fw_method),
    distinct_framework_fns = length(unique(c(fw_fn, fw_method))),
    framework_constructors = n_ctor,
    stringsAsFactors = FALSE
  )
}

SCRIPTS <- c(
  funcml = "experiment/R/workflow_funcml.R",
  mlr3 = "experiment/R/workflow_mlr3.R",
  tidymodels = "experiment/R/workflow_tidymodels.R"
)

main <- function() {
  suppressPackageStartupMessages({
    library(funcml); library(mlr3); library(mlr3learners); library(tidymodels)
  })
  res <- do.call(rbind, Map(measure_one, names(SCRIPTS), SCRIPTS))
  rownames(res) <- NULL
  out <- "experiment/results/compactness_static.csv"
  utils::write.csv(res, out, row.names = FALSE)
  print(res)
  cat("\nwritten: ", out, "\n", sep = "")
}

if (sys.nframe() == 0) main()
