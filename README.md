# Agentic Compactness: Measuring the Cost of Machine Learning Workflows in R

Code for the R+AI 2026 talk (R Consortium, 10 November 2026).

The same supervised learning pipeline is written in three R frameworks
(funcml, mlr3, tidymodels) and run on 10 biobench datasets in three
scenarios (`compare`, `preprocess`, `tune`), plus a speed and memory test
on simulated data (500 to 50,000 rows). Pipelines are aligned so that
predictions agree across frameworks; the differences measured are
compactness (lines of code, framework calls, attached packages), load
time, run time and peak memory.

## Layout

```
slides/
  agentic-compactness-RAI2026-slides.pdf   R+AI 2026 talk slides (PDF)
experiment-biobench/
  R/
    run_experiment.R        runs every workflow in a fresh Rscript process
    harness.R               shared data split, folds and timing marks
    datasets.R              dataset inclusion rule on the biobench registry
    biobench_*.R            biobench loading and preparation
    measure_compactness.R   static counts (LOC, framework calls, packages)
    make_*.R                tables and figures from the run outputs
    workflows/              one script per scenario x framework
  issues/reprex/            minimal reproducible examples of issues met (E1-E5)
  results/                  final results: summary tables, figures, and per-run
                            metrics, timings, memory and attached packages
experiment/
  R/measure_compactness.R   counting rules shared with the code listing
```

## Running

From the repository root:

```sh
Rscript experiment-biobench/R/datasets.R
Rscript experiment-biobench/R/run_experiment.R            # all arms, 3 seeds
Rscript experiment-biobench/R/run_experiment.R --smoke    # quick check
Rscript experiment-biobench/R/make_tables_figures.R
```

Runs are resumable: a finished run is skipped when the command is repeated.
Each run is single-threaded (`OMP_NUM_THREADS=1`, `R_RANGER_NUM_THREADS=1`)
so timings are comparable; peak memory needs GNU `time`.

## Software

R packages used in the reported runs: funcml 0.9.6, mlr3 1.5.0,
mlr3learners 0.14.0, mlr3tuning 1.5.1, tidymodels 1.4.1, parsnip 1.6.1,
tune 2.1.0, yardstick 1.4.0, rsample 1.3.2, xgboost 3.2.1.1, ranger 0.18.0,
glmnet, biobench.

## Author

Imad El Badisy
