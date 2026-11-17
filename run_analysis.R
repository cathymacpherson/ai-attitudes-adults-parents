# Attitudes and behavioural intentions toward general-purpose versus purpose-built
# generative AI for mental health, among community adults and parents/carers.
#
# Reads a scored dataset from data/cleaned_scored_data.csv and writes tidy
# result CSVs plus Overleaf-ready LaTeX table fragments to outputs/results/.
#
# Run the whole pipeline from the repository root:
#   Rscript run_analysis.R
#
# Individual steps can also be run on their own, from any directory inside the
# repository, because each one sets up the session if it has not been set up
# already:
#   source("R/04_model_a.R")
#
# Requires lme4 >= 2.0, lmerTest, emmeans and simr (the power analysis needs
# lme4 2.x; simr's makeLmer fails on lme4 1.x).

options(warn = 1)

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  } else {
    normalizePath(".", winslash = "/")
  }
})

# Loads packages, configuration, helper functions and creates the output
# directories. The step scripts source this too, so it is written to be safe to
# run more than once.
source(file.path(script_dir, "R", "_setup.R"))

message("Analysis pipeline")
message("  root:    ", ROOT)
message("  outputs: ", OUT_DIR)
message("  sample:  community adults and parents/carers")
message("  concern scales: ",
        if (REVERSE_CONCERN_SCALES) "reverse-scored (pipeline convention)"
        else "natural direction (higher = more concern)")
message("")

STEPS <- c("01_load_prepare", "02_reliability", "03_descriptives",
           "04_model_a", "05_model_b", "06_model_c", "07_model_d",
           "09_power", "10_figures", "11_diagnostics", "12_supp_redundancy")

# The power analysis simulates tens of thousands of model fits and takes a few
# minutes. Set RUN_POWER=0 in the environment to skip it while iterating on the
# models; its tables are then left in place rather than treated as stale.
skip_power <- identical(Sys.getenv("RUN_POWER"), "0")
if (skip_power) {
  STEPS <- setdiff(STEPS, "09_power")
  message("  (skipping 09_power; RUN_POWER=0)")
}

for (step in STEPS) {
  source(file.path(script_dir, "R", paste0(step, ".R")))
  message("")
}

# Stale outputs are cleared before the preview is built, so the wrapper never
# ends up with an \input{} pointing at a file that is about to be deleted.
if (skip_power) {
  for (path in list.files(TEX_DIR, pattern = "^power_", full.names = TRUE)) {
    record_output(path)
  }
  for (path in list.files(OUT_DIR, pattern = "^09_.*\\.csv$", full.names = TRUE)) {
    record_output(path)
  }
}
remove_stale_outputs()
message("")

source(file.path(script_dir, "R", "08_preview.R"))
message("")

manifest <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  steps = STEPS,
  sample = list(
    min_plausible_parent_age = MIN_PLAUSIBLE_PARENT_AGE,
    reverse_concern_scales = REVERSE_CONCERN_SCALES,
    n_participants = nrow(participants),
    n_adults = sum(participants$respondent_type == "Adult"),
    n_parents = sum(participants$respondent_type == "Parent")
  ),
  csv_files = sort(basename(list.files(OUT_DIR, pattern = "\\.csv$"))),
  tex_files = sort(basename(list.files(TEX_DIR, pattern = "\\.tex$"))),
  fig_files = sort(basename(list.files(FIG_DIR, pattern = "\\.(pdf|png)$")))
)

writeLines(
  c("{",
    sprintf('  "generated_at": "%s",', manifest$generated_at),
    sprintf('  "r_version": "%s",', manifest$r_version),
    sprintf('  "steps": [%s],', paste0('"', STEPS, '"', collapse = ", ")),
    '  "sample": {',
    sprintf('    "reverse_concern_scales": %s,', tolower(as.character(REVERSE_CONCERN_SCALES))),
    sprintf('    "n_participants": %d,', manifest$sample$n_participants),
    sprintf('    "n_adults": %d,', manifest$sample$n_adults),
    sprintf('    "n_parents": %d', manifest$sample$n_parents),
    '  },',
    sprintf('  "csv_files": [%s],', paste0('"', manifest$csv_files, '"', collapse = ", ")),
    sprintf('  "tex_files": [%s],', paste0('"', manifest$tex_files, '"', collapse = ", ")),
    sprintf('  "fig_files": [%s]', paste0('"', manifest$fig_files, '"', collapse = ", ")),
    "}"),
  file.path(OUT_DIR, "manifest.json")
)

message("Done. ", length(manifest$tex_files), " LaTeX fragments in ", TEX_DIR)
