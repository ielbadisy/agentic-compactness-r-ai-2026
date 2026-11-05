# ---------------------------------------------------------------------------
# make_dataset_table.R (biobench experiment)
#
# Describes the 10 biobench datasets as the harness prepares them (after the
# registry preparation and clean_predictors), plus the simulated scaling data.
# Writes results/table_datasets.csv, table_datasets.tex (paper, with source)
# and table_datasets_slide.tex (talk). Run from the paper root.
# ---------------------------------------------------------------------------

source("experiment-biobench/R/harness.R")

RES <- "experiment-biobench/results"
DS <- c("covidtest", "nhanes", "opt", "pima2", "glow500",
        "aidsdelay", "birthwt", "laryngoscope", "smartpill", "anorexia")
SOURCE <- c(covidtest = "medicaldata::covid\\_testing", nhanes = "aplore3::nhanes",
            opt = "medicaldata::opt", pima2 = "mlbench::PimaIndiansDiabetes2",
            glow500 = "aplore3::glow500", aidsdelay = "boot::aids",
            birthwt = "MASS::birthwt", laryngoscope = "medicaldata::laryngoscope",
            smartpill = "medicaldata::smartpill", anorexia = "MASS::anorexia")
OUTCOME <- c(covidtest = "SARS-CoV-2 test positive", nhanes = "Obesity (BMI > 35)",
             opt = "Pregnancy ended before 37 weeks", pima2 = "Diabetes",
             glow500 = "Fracture in first year", aidsdelay = "AIDS cases reported",
             birthwt = "Birth weight (g)", laryngoscope = "Intubation time (s)",
             smartpill = "Whole-gut transit time (h)", anorexia = "Weight after treatment (lb)")

runs <- read.csv(file.path(RES, "runs.csv"))
enc <- aggregate(p ~ data, runs[runs$arm == "datasets" & runs$scenario == "tune", ], max)

row <- function(name) {
  fr <- biobench_frame(name)
  d <- fr$data; y <- d[[fr$outcome]]; X <- d[setdiff(names(d), fr$outcome)]
  is_c <- fr$task == "classification"
  data.frame(
    dataset = name, task = fr$task, outcome = OUTCOME[[name]], source = SOURCE[[name]],
    n = nrow(d), predictors = ncol(X),
    encoded = enc$p[enc$data == paste0("biobench:", name)],
    factors = sum(vapply(X, is.factor, logical(1))),
    missing_pct = round(100 * mean(is.na(X)), 1),
    outcome_summary = if (is_c) sprintf("%.0f%% Yes", 100 * mean(y == "Yes"))
                      else sprintf("mean %.0f (SD %.0f)", mean(y), stats::sd(y)))
}
tab <- do.call(rbind, lapply(DS, row))
write.csv(tab, file.path(RES, "table_datasets.csv"), row.names = FALSE)

fmt <- function(x) formatC(x, format = "d", big.mark = ",")
task_lab <- c(classification = "Class.", regression = "Regr.")
pred_lab <- ifelse(tab$encoded == tab$predictors, tab$predictors,
                   sprintf("%d (%d)", tab$predictors, tab$encoded))
outcome_sum <- sub("%", "\\\\%", tab$outcome_summary)
miss_lab <- ifelse(tab$missing_pct == 0, "0", sprintf("%.1f", tab$missing_pct))

# Paper version: with source, factor count and missingness.
rows <- sprintf("%s & %s & %s & %s & %s & %s & %d & %s & %s \\\\",
                tab$dataset, task_lab[tab$task], tab$outcome, outcome_sum,
                fmt(tab$n), pred_lab, tab$factors, miss_lab, paste0("\\texttt{", tab$source, "}"))
writeLines(c(
  "\\begin{tabular}{lllrrrrrl}", "\\toprule",
  "Dataset & Task & Outcome & Outcome summary & $n$ & Predictors & Factors & Missing (\\%) & Source \\\\",
  "\\midrule", rows, "\\bottomrule", "\\end{tabular}"),
  file.path(RES, "table_datasets.tex"))

# Slide version: short.
rows <- sprintf("%s & %s & %s & %s & %s & %s \\\\",
                tab$dataset, task_lab[tab$task], tab$outcome, outcome_sum, fmt(tab$n), pred_lab)
writeLines(c(
  "\\begin{tabular}{lllrrr}", "\\toprule",
  "Dataset & Task & Outcome & Summary & $n$ & Predictors \\\\",
  "\\midrule", rows, "\\bottomrule", "\\end{tabular}"),
  file.path(RES, "table_datasets_slide.tex"))

print(tab[, c("dataset", "n", "predictors", "encoded", "factors", "missing_pct", "outcome_summary")])
