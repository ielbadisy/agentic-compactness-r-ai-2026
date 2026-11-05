# ---------------------------------------------------------------------------
# make_code_listings.R (biobench experiment, tune scenario)
# Adapted from experiment/R/make_code_listings.R.
#
# Renders the analysis region of each workflow script as a LaTeX listing in
# which every framework-specific call counted by measure_compactness.R is
# highlighted. The highlight rule reuses the measurement rule, and the script
# stops if the highlighted count differs from compactness_static.csv, so the
# figure cannot drift from the reported numbers.
#
# Writes experiment/results/figures/code_<framework>.tex (listing bodies) and
# compiles experiment/results/figures/code_listing.pdf (three-column figure).
# ---------------------------------------------------------------------------

source(file.path("experiment", "R", "measure_compactness.R"))
FRAMEWORK_NS$mlr3 <- c(FRAMEWORK_NS$mlr3, "mlr3tuning", "mlr3pipelines", "bbotk")
FRAMEWORK_NS$tidymodels <- c(FRAMEWORK_NS$tidymodels, "tidymodels")
suppressPackageStartupMessages({
  library(funcml); library(mlr3); library(mlr3learners); library(mlr3tuning)
  library(mlr3pipelines); library(tidymodels)
})

SCENARIO <- "tune"
FIG <- file.path("experiment-biobench", "results", "figures")
static <- read.csv(file.path("experiment-biobench", "results", "compactness_static.csv"))
static <- static[static$scenario == SCENARIO, ]
SCRIPTS <- stats::setNames(
  file.path("experiment-biobench", "R", "workflows",
            sprintf("%s_%s.R", SCENARIO, c("funcml", "mlr3", "tidymodels"))),
  c("funcml", "mlr3", "tidymodels"))
file.copy(file.path("experiment", "results", "figures", "code_listing.tex"),
          file.path(FIG, "code_listing.tex"), overwrite = TRUE)

# Positions (line, col1, col2) of counted framework calls, same rule as
# measure_one(): exported framework functions, plus `$` method symbols for mlr3.
counted_tokens <- function(framework, path) {
  region <- extract_region(path)
  expr <- parse(text = paste(region, collapse = "\n"), keep.source = TRUE)
  pd <- utils::getParseData(expr)
  ns <- FRAMEWORK_NS[[framework]]
  exported <- unique(unlist(lapply(ns, function(n) {
    tryCatch(getNamespaceExports(n), error = function(e) character(0))
  })))
  fn <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% exported, ]
  hits <- fn[, c("line1", "col1", "col2", "text")]
  if (framework == "mlr3") {
    for (i in which(pd$token == "'$'")) {
      sibs <- pd[pd$parent == pd$parent[i], ]
      rhs <- sibs[sibs$token == "SYMBOL" & sibs$col1 > pd$col1[i], ]
      if (nrow(rhs)) hits <- rbind(hits, rhs[1, c("line1", "col1", "col2", "text")])
    }
  }
  list(region = region, hits = hits)
}

tex_escape <- function(s) {
  # Swap to placeholders first so the braces of the macros are not re-escaped.
  s <- gsub("\\", "\003", s, fixed = TRUE)
  s <- gsub("{", "\004", s, fixed = TRUE)
  s <- gsub("}", "\005", s, fixed = TRUE)
  s <- gsub("\003", "\\BS{}", s, fixed = TRUE)
  s <- gsub("\004", "\\LB{}", s, fixed = TRUE)
  gsub("\005", "\\RB{}", s, fixed = TRUE)
}

render_listing <- function(framework, path) {
  ct <- counted_tokens(framework, path)
  region <- ct$region
  hits <- ct$hits
  expected <- static$framework_calls[static$framework == framework]
  if (nrow(hits) != expected) {
    stop(sprintf("%s: %d highlighted calls but %d in compactness_static.csv",
                 framework, nrow(hits), expected))
  }
  keep <- vapply(region, is_code_line, logical(1))
  out <- character(0)
  for (ln in seq_along(region)) {
    if (!keep[ln]) next
    s <- region[ln]
    h <- hits[hits$line1 == ln, ]
    h <- h[order(h$col1, decreasing = TRUE), ]
    # Insert markers right to left so earlier columns stay valid.
    pieces <- s
    for (k in seq_len(nrow(h))) {
      a <- h$col1[k]; b <- h$col2[k]
      pieces <- paste0(substr(pieces, 1, a - 1), "\001", substr(pieces, a, b),
                       "\002", substr(pieces, b + 1, nchar(pieces)))
    }
    pieces <- tex_escape(pieces)
    pieces <- gsub("\001", "\\fw{", pieces, fixed = TRUE)
    pieces <- gsub("\002", "}", pieces, fixed = TRUE)
    out <- c(out, pieces)
  }
  writeLines(out, file.path(FIG, sprintf("code_%s.tex", framework)))
  invisible(length(out))
}

for (fw in names(SCRIPTS)) render_listing(fw, SCRIPTS[[fw]])

# Count macros for listing headers, e.g. \LOCfuncml, \CALLSmlrthree.
macros <- unlist(lapply(seq_len(nrow(static)), function(i) {
  fw <- gsub("3", "three", static$framework[i])  # TeX macro names cannot hold digits
  c(sprintf("\\newcommand{\\LOC%s}{%d}", fw, static$workflow_loc[i]),
    sprintf("\\newcommand{\\CALLS%s}{%d}", fw, static$framework_calls[i]))
}))
writeLines(macros, file.path(FIG, "code_counts.tex"))

# Compile the three-column figure.
old <- setwd(FIG)
on.exit(setwd(old), add = TRUE)
status <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error",
                                "code_listing.tex"), stdout = FALSE)
if (status != 0) stop("pdflatex failed on code_listing.tex")
unlink(c("code_listing.aux", "code_listing.log"))
cat("code listings written under", FIG, "\n")
