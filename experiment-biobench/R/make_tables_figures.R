# ---------------------------------------------------------------------------
# make_tables_figures.R (biobench experiment)
#
# Reads experiment-biobench/results/{compactness_static,runs,performance}.csv
# and the per-run packages.txt, writes LaTeX tables and PDF figures for the
# talk and the paper. Same palette and theme as experiment/R. Run from the
# paper root after run_experiment.R and measure_compactness.R.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

RES <- "experiment-biobench/results"
FIG <- file.path(RES, "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
FW <- c("funcml", "mlr3", "tidymodels")
SC <- c("compare", "preprocess", "tune")
SC_LAB <- c(compare = "Compare 3 learners", preprocess = "Impute + encode + compare",
            tune = "Tune xgboost (12-point grid)")
FW_COL <- c(funcml = "#2A78D6", mlr3 = "#EB6834", tidymodels = "#1BAF7A")
INK <- "#1F1F1F"; MUTED <- "#6B6A66"; RULE <- "#D6D5D0"; FONT <- "Inter"
tint <- function(hex, amount = 0.55) {
  m <- grDevices::col2rgb(hex) / 255
  grDevices::rgb(t(m + (1 - m) * amount))
}
theme_compact <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = FONT) +
    theme(text = element_text(colour = INK), axis.text = element_text(colour = MUTED),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = RULE, linewidth = 0.3),
          strip.text = element_text(face = "bold", hjust = 0, colour = INK),
          plot.title = element_text(face = "bold", size = rel(1.1)),
          plot.title.position = "plot", legend.position = "none")
}

static <- read.csv(file.path(RES, "compactness_static.csv"))
runs <- read.csv(file.path(RES, "runs.csv"))
perf <- read.csv(file.path(RES, "performance.csv"))
ds <- runs[runs$arm == "datasets", ]

# Packages attached by the framework: drop base packages and biobench, which
# the shared harness attaches in every dataset run.
DEFAULT_PKGS <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods")
fw_pkgs <- function(id) setdiff(readLines(file.path(RES, "runs", id, "packages.txt")),
                                c(DEFAULT_PKGS, "biobench"))
ds$packages <- vapply(ds$id, function(i) length(fw_pkgs(i)), integer(1))
pk <- aggregate(packages ~ scenario + framework, ds, max)
static <- merge(static, pk)

# Execution: medians over 10 datasets x 3 seeds per cell.
ex <- aggregate(cbind(library_seconds, workflow_seconds, total_seconds, peak_memory_mib) ~
                  scenario + framework, ds, median)
tab <- merge(static, ex)
tab <- tab[order(match(tab$scenario, SC), match(tab$framework, FW)), ]
write.csv(tab, file.path(RES, "table_glance.csv"), row.names = FALSE)

# --- Agreement of test performance across frameworks ------------------------
pm <- perf[perf$arm == "datasets" & perf$metric %in% c("auc", "rmse"), ]
gap <- aggregate(value ~ data + scenario + seed + metric, pm,
                 function(v) c(gap = diff(range(v)), mean = mean(v)))
gap <- cbind(gap[, 1:4], as.data.frame(gap$value))
gap$rel <- ifelse(gap$metric == "rmse", gap$gap / gap$mean, gap$gap)  # AUC: absolute
agree <- do.call(rbind, lapply(split(gap, gap$scenario), function(d) {
  data.frame(scenario = d$scenario[1], identical = sum(d$gap < 1e-9), n = nrow(d),
             auc_max = max(d$gap[d$metric == "auc"]),
             rmse_rel_max = max(d$rel[d$metric == "rmse"]))
}))
agree <- agree[match(SC, agree$scenario), ]
write.csv(agree, file.path(RES, "table_agreement.csv"), row.names = FALSE)

# Selected configuration / learner agreement.
sel <- aggregate(selected ~ data + scenario + seed, ds, function(s) length(unique(s)) == 1)
sel_agree <- aggregate(selected ~ scenario, sel, function(x) sprintf("%d/%d", sum(x), length(x)))
agree$same_selection <- sel_agree$selected[match(agree$scenario, sel_agree$scenario)]
write.csv(agree, file.path(RES, "table_agreement.csv"), row.names = FALSE)

# --- LaTeX: results at a glance --------------------------------------------
bold_min <- function(x, fmt) {
  s <- sprintf(fmt, x); s[x == min(x)] <- paste0("\\textbf{", s[x == min(x)], "}"); s
}
rows <- unlist(lapply(SC, function(sc) {
  d <- tab[tab$scenario == sc, ]
  cells <- cbind(paste0("\\texttt{", d$framework, "}"),
                 bold_min(d$workflow_loc, "%d"), bold_min(d$framework_calls, "%d"),
                 bold_min(d$distinct_framework_fns, "%d"), bold_min(d$framework_constructors, "%d"),
                 bold_min(d$packages, "%d"), bold_min(d$library_seconds, "%.2f"),
                 bold_min(d$total_seconds, "%.1f"), bold_min(d$peak_memory_mib, "%.0f"))
  c(sprintf("\\multicolumn{9}{l}{\\emph{%s}} \\\\", SC_LAB[[sc]]),
    apply(cells, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
    if (sc != tail(SC, 1)) "\\addlinespace")
}))
writeLines(c(
  "\\begin{tabular}{l rrrrr rrr}", "\\toprule",
  " & \\multicolumn{5}{c}{Code surface (counts)} & \\multicolumn{3}{c}{Execution (median of 30 runs)} \\\\",
  "\\cmidrule(lr){2-6}\\cmidrule(lr){7-9}",
  "Framework & Lines & Calls & Functions & Objects & Packages & Load, s & Total, s & Peak, MiB \\\\",
  "\\midrule", rows, "\\bottomrule", "\\end{tabular}"), file.path(RES, "table_glance.tex"))

# --- LaTeX: agreement --------------------------------------------------------
fmt_g <- function(x) if (x < 1e-9) "0" else sprintf("%.3f", x)
writeLines(c(
  "\\begin{tabular}{l rr rr}", "\\toprule",
  "Scenario & Same selection & Identical score & Max AUC gap & Max RMSE gap (rel.) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(agree)), function(i) with(agree[i, ], sprintf(
    "%s & %s & %d/%d & %s & %s \\\\", SC_LAB[[scenario]], same_selection, identical, n,
    fmt_g(auc_max), if (rmse_rel_max < 1e-9) "0" else sprintf("%.1f\\%%", 100 * rmse_rel_max))),
    character(1)),
  "\\bottomrule", "\\end{tabular}"), file.path(RES, "table_agreement.tex"))

# --- Figure: multiplier of the funcml value, per scenario --------------------
meas <- c(workflow_loc = "Lines of code", framework_calls = "Framework calls",
          distinct_framework_fns = "Distinct functions",
          framework_constructors = "Objects constructed", packages = "Packages attached",
          total_seconds = "Run time", peak_memory_mib = "Peak memory")
md <- do.call(rbind, lapply(SC, function(sc) do.call(rbind, lapply(names(meas), function(m) {
  d <- tab[tab$scenario == sc, ]; ref <- d[[m]][d$framework == "funcml"]
  data.frame(scenario = SC_LAB[[sc]], measure = meas[[m]], framework = c("mlr3", "tidymodels"),
             ratio = c(d[[m]][d$framework == "mlr3"], d[[m]][d$framework == "tidymodels"]) / ref)
}))))
md$scenario <- factor(md$scenario, levels = SC_LAB)
md$measure <- factor(md$measure, levels = rev(meas))
md$y <- as.numeric(md$measure) + ifelse(md$framework == "mlr3", 0.17, -0.17)
md$label <- ifelse(md$ratio >= 10, sprintf("%.0f×", md$ratio), sprintf("%.1f×", md$ratio))
p_mult <- ggplot(md) +
  geom_vline(xintercept = 1, colour = FW_COL[["funcml"]], linewidth = 0.8) +
  geom_point(aes(ratio, y, colour = framework), size = 2.6) +
  geom_text(aes(ratio, y, label = label), hjust = -0.3, size = 2.6, family = FONT, colour = INK) +
  facet_wrap(~scenario, nrow = 1) +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8, 16),
                     labels = c("0.5×", "1×", "2×", "4×", "8×", "16×"),
                     limits = c(0.5, 32)) +
  scale_y_continuous(breaks = seq_along(levels(md$measure)), labels = levels(md$measure),
                     expand = expansion(add = 0.5)) +
  scale_colour_manual(values = FW_COL) +
  labs(x = "Multiple of the funcml value (log scale; 1× = funcml)", y = NULL) +
  theme_compact() +
  theme(panel.grid.major.y = element_blank(), legend.position = "top",
        legend.justification = "left", legend.title = element_blank())
ggsave(file.path(FIG, "fig_multiplier.pdf"), p_mult, width = 9, height = 3.6, device = cairo_pdf)
ggsave(file.path(FIG, "fig_multiplier_slide.pdf"), p_mult, width = 8, height = 3.4, device = cairo_pdf)

# --- Figure: execution cost per scenario --------------------------------------
tl <- rbind(data.frame(ex[, 1:2], phase = "load", seconds = ex$library_seconds),
            data.frame(ex[, 1:2], phase = "workflow", seconds = ex$total_seconds - ex$library_seconds))
tl$fill <- ifelse(tl$phase == "load", vapply(FW_COL[tl$framework], tint, ""), FW_COL[tl$framework])
tl$framework <- factor(tl$framework, levels = rev(FW))
tl$phase <- factor(tl$phase, levels = c("workflow", "load"))
tl$scenario <- factor(SC_LAB[tl$scenario], levels = SC_LAB)
tot <- aggregate(seconds ~ framework + scenario, tl, sum)
p_time <- ggplot(tl, aes(seconds, framework)) +
  geom_col(aes(fill = fill), width = 0.62, colour = "white", linewidth = 0.6) +
  geom_text(data = tot, aes(seconds, framework, label = sprintf("%.1f s", seconds)),
            hjust = -0.2, size = 2.8, family = FONT, colour = INK) +
  facet_wrap(~scenario, nrow = 1) + scale_fill_identity() +
  scale_x_continuous(limits = c(0, max(tot$seconds) * 1.3), expand = expansion(mult = 0)) +
  labs(x = "Median seconds per run (lighter: package load)", y = NULL,
       title = "Wall-clock time") +
  theme_compact() + theme(panel.grid.major.y = element_blank())
mem <- ex; mem$framework <- factor(mem$framework, levels = rev(FW))
mem$scenario <- factor(SC_LAB[mem$scenario], levels = SC_LAB)
p_mem <- ggplot(mem, aes(peak_memory_mib, framework, fill = framework)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.0f MiB", peak_memory_mib)), hjust = -0.15, size = 2.8,
            family = FONT, colour = INK) +
  facet_wrap(~scenario, nrow = 1) + scale_fill_manual(values = FW_COL) +
  scale_x_continuous(limits = c(0, max(mem$peak_memory_mib) * 1.35), expand = expansion(mult = 0)) +
  labs(x = "Median peak resident memory, MiB", y = NULL, title = "Peak memory") +
  theme_compact() + theme(panel.grid.major.y = element_blank())
p_cost <- p_time / p_mem
ggsave(file.path(FIG, "fig_cost.pdf"), p_cost, width = 9, height = 4.4, device = cairo_pdf)
ggsave(file.path(FIG, "fig_cost_slide.pdf"), p_cost, width = 8, height = 3.8, device = cairo_pdf)

# --- Figure: scaling arm (compare scenario, simulated data) -------------------
sc <- runs[runs$arm == "scaling", ]
sc$n <- as.integer(sub("sim:(\\d+):.*", "\\1", sc$data))
sc$task <- sub(".*:", "", sc$data)
sca <- aggregate(total_seconds ~ framework + n + task, sc, median)
p_scale <- ggplot(sca, aes(n, total_seconds, colour = framework)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  facet_wrap(~task) + scale_colour_manual(values = FW_COL) +
  scale_x_log10(breaks = sort(unique(sca$n)), labels = scales::comma) + scale_y_log10() +
  labs(x = "Training + test rows (log scale)", y = "Median seconds per run (log scale)") +
  theme_compact() + theme(legend.position = "top", legend.justification = "left",
                          legend.title = element_blank())
ggsave(file.path(FIG, "fig_scaling.pdf"), p_scale, width = 8, height = 3.4, device = cairo_pdf)
ggsave(file.path(FIG, "fig_scaling_slide.pdf"), p_scale, width = 7, height = 3.4, device = cairo_pdf)
write.csv(sca, file.path(RES, "table_scaling.csv"), row.names = FALSE)

print(tab[, c("scenario", "framework", "workflow_loc", "framework_calls", "packages",
              "library_seconds", "total_seconds", "peak_memory_mib")], row.names = FALSE)
print(agree, row.names = FALSE)
print(sca, row.names = FALSE)
