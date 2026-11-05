# E5: funcml's ranger learner drops num.threads, so ranger uses every core
# unless R_RANGER_NUM_THREADS is set. mlr3 and tidymodels pass it through.
# Run from the paper root: Rscript experiment-biobench/issues/reprex/E5_ranger_threads.R
suppressPackageStartupMessages(library(funcml))
set.seed(1)
n <- 20000
d <- data.frame(matrix(rnorm(n * 10), n, 10))
d$y <- factor(ifelse(d$X1 + rnorm(n) > 0, "Yes", "No"))
cat("cores:", parallel::detectCores(), "| R_RANGER_NUM_THREADS:",
    Sys.getenv("R_RANGER_NUM_THREADS", "<unset>"), "\n")
t <- system.time(m <- fit(y ~ ., data = d, model = "ranger",
                          spec = list(num.trees = 200, num.threads = 1), seed = 1))
cat("funcml fit(spec = list(num.threads = 1)): CPU time / elapsed =", round((t[["user.self"]] + t[["sys.self"]]) / t[["elapsed"]], 2), "\n")
t2 <- system.time(r <- ranger::ranger(y ~ ., data = d, num.trees = 200, num.threads = 1,
                                      probability = TRUE))
cat("ranger::ranger(num.threads = 1) directly:  CPU time / elapsed =", round((t2[["user.self"]] + t2[["sys.self"]]) / t2[["elapsed"]], 2), "\n")
