# Attitudes toward generative AI in mental health care

Mixed-effects analyses of attitudes and behavioural intentions toward
general-purpose versus bespoke generative AI tools for mental health, among
Australian adults and parents/carers.

This repository contains the analysis code only. It expects a de-identified,
scored dataset supplied separately (see [Data](#data) below) and produces
tidy result CSVs and Overleaf-ready LaTeX table fragments.

## Run

```bash
Rscript run_paper1.R
```

The power analysis (`09_power.R`) takes several minutes. To skip it while
iterating on the models:

```bash
RUN_POWER=0 Rscript run_paper1.R
```

### Running one step at a time

Each step script sets up the session itself if it has not been set up
already, so any of them can be run on its own from anywhere inside the
repository:

```r
source("R/04_model_a.R")
```

Steps that model the data call `paper1_require_data()`, which sources
`01_load_prepare.R` only if the analysis frames are not already in the
session.

## Data

Place a de-identified, scored CSV at `data/cleaned_scored_data.csv`. Expected
columns are documented by the item-scoring logic in `R/01_load_prepare.R`
and the questionnaire structure in `R/00_config.R`.

## Requirements

| Package | Minimum | Why |
|---|---|---|
| lme4 | 2.0 | `simr::makeLmer` calls `lme4::getReCovs`, absent in lme4 1.x |
| lmerTest | 3.1 | Satterthwaite degrees of freedom |
| emmeans | — | Estimated means and contrasts |
| simr | 1.0.10 | Power simulation |
| MASS | (base) | Ordinal model in Model D |
| ggplot2 | — | Figures (`10_figures.R`) |
| clubSandwich | — | Cluster-robust standard errors (`11_diagnostics.R`) |

```r
install.packages(c("lme4", "lmerTest", "emmeans", "simr", "ggplot2", "clubSandwich"))
```

## Pipeline steps

| Script | Produces |
|---|---|
| `01_load_prepare.R` | Analysis frames (`participants`, `long`) |
| `02_reliability.R` | Internal consistency of the ATAMHS subscales |
| `03_descriptives.R` | Demographics, subscale descriptives, correlations |
| `04_model_a.R` | Perceptions of general-purpose vs. bespoke AI |
| `05_model_b.R` | Predictors of attitudes and intentions |
| `06_model_c.R` | Demographic and familiarity predictors |
| `07_model_d.R` | Direct comfort comparison (ordinal model) |
| `08_preview.R` | Wraps every table fragment into one document for checking |
| `09_power.R` | A priori power simulation |
| `10_figures.R` | Figures |
| `11_diagnostics.R` | Residual diagnostics and cluster-robust standard errors |
| `12_supp_redundancy.R` | Supplementary redundancy checks |

## Outputs

Written to `outputs/paper1_r/`: tidy CSVs at the top level, LaTeX table
fragments in `tables/`, figures in `figures/`, and `preview_all_tables.tex`
for a one-shot compile check of every table.

## Optional: HTML table preview

```bash
python3 render_tables.py
```

Renders `outputs/paper1_r/tables/*.tex` as a single HTML page
(`outputs/paper1_r/tables_preview.html`) for viewing without a LaTeX
toolchain.
