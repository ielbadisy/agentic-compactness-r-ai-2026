# compare scenario, funcml: 5-fold CV comparison of three prespecified
# learners on model-ready data, refit of the selected learner, test prediction.
.M <- c(t0 = proc.time()[["elapsed"]])
suppressPackageStartupMessages(library(funcml))
source(file.path("experiment-biobench", "R", "harness.R"))
.M["t_loaded"] <- proc.time()[["elapsed"]]

ctx <- load_context()
train <- ctx$train; test <- ctx$test; form <- ctx$form
seed <- ctx$seed; is_classif <- ctx$is_classif
record_packages(ctx)
.M["t_wf_start"] <- proc.time()[["elapsed"]]

# >>> workflow
specs <- list(glmnet = LEARNER_SPECS$glmnet,
              ranger = LEARNER_SPECS$random_forest,
              xgboost = LEARNER_SPECS$xgboost)
metric <- if (is_classif) "auc" else "rmse"
comparison <- compare(train, form, models = names(specs), specs = specs,
                      resampling = cv(v = CV_FOLDS, strata = TRUE, seed = seed),
                      metrics = metric, seed = seed)
cv <- comparison$results[comparison$results$metric == metric, ]
best <- cv$model[if (is_classif) which.max(cv$mean) else which.min(cv$mean)]
model <- fit(form, data = train, model = best, spec = specs[[best]], seed = seed)
pred <- if (is_classif) {
  predict(model, newdata = test, type = "prob")[, "Yes"]
} else {
  predict(model, newdata = test)
}
# <<< workflow

.M["t_wf_end"] <- proc.time()[["elapsed"]]
finish(ctx, best, pred, .M)
