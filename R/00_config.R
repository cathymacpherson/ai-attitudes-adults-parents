# Configuration ---------------------------------------------------------------
#
# Sample definition, scoring, covariate coding and inference settings for the
# analysis, in one place rather than scattered through the model scripts.

# ---- Project paths ----------------------------------------------------------

find_project_root <- function(start = getwd()) {
  marker <- file.path("R", "_setup.R")
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, marker))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the project root. Run this script from inside ",
           "the repository.", call. = FALSE)
    }
    path <- parent
  }
}

ROOT <- find_project_root()
DATA_FILE <- file.path(ROOT, "data", "ai_attitudes_adults_parents.csv")
OUT_DIR <- file.path(ROOT, "outputs", "results")
TEX_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR <- file.path(OUT_DIR, "figures")

# ---- Sample definition ------------------------------------------------------

# Community adults and parents/carers.

# A parent/carer age below 18 is implausible (most likely the child's age
# entered by mistake). The respondent is retained and only the age is set to
# missing, so they drop out of age-adjusted models via
# complete-case selection but stay in descriptives.
MIN_PLAUSIBLE_PARENT_AGE <- 18

# Adolescents aged 16-17 were eligible to self-report, so under-18 ages in the
# adult sample are plausible and retained; flagged here for reporting.
FLAG_UNDER18_ADULTS <- TRUE

# Respondents reporting "Other or undisclosed" gender are too few to estimate
# a coefficient for. 01_load_prepare.R builds gender_adj, a copy of
# gender with this level set to missing, so descriptives and reliability keep
# the full gender variable while ADJUSTMENT_COVARIATES (and so every adjusted
# model) uses gender_adj instead.
BLANK_OTHER_GENDER <- TRUE

# ---- Scoring direction ------------------------------------------------------

# Privacy, oversight and environmental concern are kept in their natural
# direction (higher = more concern) rather than reverse-scored: all six items
# are worded as concerns, so reversing them would turn every results sentence
# into a double negative.
REVERSE_CONCERN_SCALES <- FALSE

# ---- Measures ---------------------------------------------------------------

ATTITUDE_SCALE_MAX <- 7

# The questionnaire as administered, driving the reliability analysis.
# Departures from it for modelling are declared in SCALE_OVERRIDES below.
# `reverse` marks the scales flipped when REVERSE_CONCERN_SCALES is TRUE.
SUBSCALES <- list(
  pu        = list(items = 1:4,   label = "Perceived usefulness",             reverse = FALSE),
  peou      = list(items = 5:6,   label = "Perceived ease of use",            reverse = FALSE),
  att       = list(items = 7:8,   label = "Attitudes towards AI",             reverse = FALSE),
  intent    = list(items = 9:10,  label = "Behavioural intentions to use AI", reverse = FALSE),
  trust     = list(items = 11:12, label = "Trust in AI",                      reverse = FALSE),
  privacy   = list(items = 13:14, label = "Privacy concern",                  reverse = TRUE),
  oversight = list(items = 15:16, label = "Human oversight preferences",      reverse = TRUE),
  environ   = list(items = 17:18, label = "Environmental concerns",           reverse = TRUE)
)

# Model A: how are the two tool types perceived? Six perception outcomes.
MODEL_A_OUTCOMES <- c("pu", "peou", "trust", "privacy", "oversight", "environ")

# Model B: what predicts attitudes and intentions? These two are outcomes
# there, with the six Model A scales as predictors.
MODEL_B_OUTCOMES <- c("att", "intent")
MODEL_B_PREDICTORS <- MODEL_A_OUTCOMES

# ---- Display order and short labels ----------------------------------------

TABLE_VAR_ORDER <- c(MODEL_B_PREDICTORS, MODEL_B_OUTCOMES)
TABLE_VAR_LABELS <- c(
  pu        = "Perceived usefulness",
  peou      = "Perceived ease of use",
  trust     = "Trust",
  privacy   = "Privacy concerns",
  oversight = "Oversight preferences",
  environ   = "Environmental concerns",
  att       = "Attitudes",
  intent    = "Intentions"
)

# ---- Scale definitions as analysed ------------------------------------------

# Departures from the questionnaire as specified, applied only when scoring
# the analysis frame; the reliability analysis uses SUBSCALES as administered.
#
# Privacy: the reliability of the two-item composite is assessed in
# 02_reliability.R. Item 13 ("I would worry about privacy...") is kept as the privacy
# score; item 14 ("I would want strong safeguards...") is reported separately
# as a standalone descriptive item rather than combined with it.
SCALE_OVERRIDES <- list(
  privacy = list(items = 13L, label = "Privacy concerns")
)

# Items reported descriptively on their own, outside any subscale.
STANDALONE_ITEMS <- list(
  safeguards = list(item = 14L, label = "Preference for data safeguards (item 14)")
)

TOOLS <- list(gen = "General-purpose AI", besp = "Purpose-built AI")
REPORT_GROUPS <- list(s = "Adult", p = "Parent")

AI_TYPE_LEVELS <- c("General-purpose AI", "Purpose-built AI")
RESPONDENT_LEVELS <- c("Adult", "Parent")

# ---- Covariate coding -------------------------------------------------------

EDUCATION_SCORES <- c(
  "Primary school" = 1,
  "Year 10 or equivalent" = 2,
  "Year 12 or equivalent" = 3,
  "Vocational training / certificate / diploma" = 4,
  "Bachelor or Honours degree" = 5,
  "Postgraduate degree (e.g., Masters, PhD)" = 6
)

GENAI_FREQUENCY_SCORES <- c(
  "I have only tried it once or twice" = 1,
  "Less than once per week" = 2,
  "About once per week" = 3,
  "Several times per week" = 4,
  "About once per day" = 5,
  "Several times per day" = 6
)

CONFIDENCE_SCORES <- c(
  "Not at all confident" = 1,
  "Slightly confident" = 2,
  "Moderately confident" = 3,
  "Very confident" = 4,
  "Extremely confident" = 5
)

# Gender is kept as a three-level factor rather than collapsed to binary, so
# non-binary/undisclosed respondents aren't dropped from every model; the
# third level is too small to interpret and isn't reported as a focal effect.
GENDER_LEVELS <- c("Male", "Female", "Other or undisclosed")

ADJUSTMENT_COVARIATES <- c("age_c", "gender_adj", "education_c")

# ---- Derived: the scale definitions actually used for modelling --------------

ANALYSIS_SCALES <- SUBSCALES
for (key in names(SCALE_OVERRIDES)) {
  ANALYSIS_SCALES[[key]][names(SCALE_OVERRIDES[[key]])] <- SCALE_OVERRIDES[[key]]
}

# ---- Inference --------------------------------------------------------------

P_ADJUST_METHOD <- "BH"
ALPHA <- 0.05
