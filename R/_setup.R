# Shared session setup, so that any step script can be run on its own.
#
# run_analysis.R sources this once. Each step script also sources it if the
# session has not been set up yet, which is what makes
#
#   source("R/04_model_a.R")
#
# work directly from RStudio or VS Code without first running the whole
# pipeline. Sourcing it twice is harmless.

suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(emmeans)
})

# Directory this file lives in, resolved by walking up from the working
# directory. Works from the repository root or from R/.
R_DIR <- local({
  path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    candidate <- file.path(path, "R", "_setup.R")
    if (file.exists(candidate)) return(file.path(path, "R"))
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate R/. Set the working directory to somewhere ",
           "inside the repository.", call. = FALSE)
    }
    path <- parent
  }
})

source(file.path(R_DIR, "00_config.R"))
source(file.path(R_DIR, "utils.R"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TEX_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# Loads the analysis frames if they are not already in the session. Steps that
# model the data call this; the power analysis, which is entirely synthetic,
# does not.
require_data <- function() {
  if (!exists("long", envir = globalenv())) {
    source(file.path(R_DIR, "01_load_prepare.R"), local = globalenv())
  }
  invisible(TRUE)
}

SETUP_DONE <- TRUE
