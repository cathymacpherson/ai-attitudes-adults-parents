# Attitudes toward generative AI in mental health care

This repository provides the code to run mixed-effects analyses of attitudes and behavioural intentions toward general-purpose versus purpose-built generative AI tools for mental health, among Australian adults and parents/carers. It is designed to support the publication: *"Predicting Attitudes Toward the Use of Generative AI in Mental Health Care Among Australian Adults and Parents/Carers"*.

This repository contains the analysis code only. It expects a de-identified, scored dataset supplied separately (see [Data](#data) below).

## Run the analyses in full

```bash
Rscript run_analysis.R
```

The power analysis (`09_power.R`) takes several minutes. To skip it while
iterating on the models:

```bash
RUN_POWER=0 Rscript run_analysis.R
```

### Running one step at a time

Each step script sets up the session itself if it has not been set up already, so any of them can be run on its own from anywhere inside the repository:

```r
source("R/04_model_a.R")
```

Steps that model the data call `require_data()`, which sources
`01_load_prepare.R` only if the analysis frames are not already in the
session.

## Data

Place the de-identified data at `data/ai_attitudes_adults_parents.csv`, with its
codebook at `data/codebook.csv`. These can be downloaded from Mendeley data here: 

Expected columns are documented by the item-scoring logic in `R/01_load_prepare.R` and the questionnaire structure in `R/00_config.R`.

The dataset contains the analysed sample only (all participant exclusions are
applied before release) and only the variables needed to reproduce the
results. Variables used solely to describe the sample (household income,
cultural background, characteristics of the child reported on, completion
time) are not included, so the demographics tables presented in the publication are not
produced here. This decision was made to preserve data anonymity. 

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
| `03_descriptives.R` | Subscale descriptives, midpoint comparisons, correlations |
| `04_model_a.R` | Perceptions of general-purpose vs. purpose-built AI |
| `05_model_b.R` | Predictors of attitudes and intentions |
| `06_model_c.R` | Demographic and familiarity predictors |
| `07_model_d.R` | Direct comfort comparison (ordinal model) |
| `08_preview.R` | Wraps every table fragment into one document for checking |
| `09_power.R` | A priori power simulation |
| `10_figures.R` | Figures |
| `11_diagnostics.R` | Residual diagnostics and cluster-robust standard errors |
| `12_supp_redundancy.R` | Supplementary redundancy checks |

## Outputs

Written to `outputs/results/`: tidy CSVs at the top level, LaTeX table
fragments in `tables/`, figures in `figures/`, and `preview_all_tables.tex`
for a one-shot compile check of every table.

## Optional: HTML table preview

```bash
python3 render_tables.py
```

Renders `outputs/results/tables/*.tex` as a single HTML page
(`outputs/results/tables_preview.html`) for viewing without a LaTeX
toolchain.
