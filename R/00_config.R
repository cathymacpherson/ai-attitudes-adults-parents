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
DATA_FILE <- file.path(ROOT, "data", "cleaned_scored_data.csv")
OUT_DIR <- file.path(ROOT, "outputs", "results")
TEX_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR <- file.path(OUT_DIR, "figures")

# ---- Sample definition ------------------------------------------------------

# Community adults and parents/carers.

# Two parent/carer respondents reported an implausible age (below 18),
# apparently their child's age entered by mistake. They are retained and only
# the age is set to missing, so they drop out of age-adjusted models via
# complete-case selection but stay in descriptives and unadjusted models.
MIN_PLAUSIBLE_PARENT_AGE <- 18

# Adolescents aged 16-17 were eligible to self-report, so under-18 ages in the
# adult sample are plausible and retained; flagged here for reporting.
FLAG_UNDER18_ADULTS <- TRUE

# Respondents reporting "Other or undisclosed" gender (n = 3) are too few to
# estimate a coefficient for. 01_load_prepare.R builds gender_adj, a copy of
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

# ---- Shared display order/labels for the manuscript tables -----------------

# reliability.tex and the two correlations_*.tex tables present the same eight
# subscales side by side, sharing one order and one set of short labels for
# table headers/row labels. Model tables and figures use the fuller
# SUBSCALES/ANALYSIS_SCALES labels instead.
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
# Privacy: the two-item composite is unreliable in every cell (Spearman-Brown
# .48-.68). Item 13 ("I would worry about privacy...") is kept as the privacy
# score; item 14 ("I would want strong safeguards...") is reported separately
# as a standalone descriptive item rather than combined with it.
SCALE_OVERRIDES <- list(
  privacy = list(items = 13L, label = "Privacy concerns")
)

# Items reported descriptively on their own, outside any subscale.
STANDALONE_ITEMS <- list(
  safeguards = list(item = 14L, label = "Preference for data safeguards (item 14)")
)

TOOLS <- list(gen = "General-purpose AI", besp = "Bespoke AI")
REPORT_GROUPS <- list(s = "Adult", p = "Parent")

AI_TYPE_LEVELS <- c("General-purpose AI", "Bespoke AI")
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

# Follows the broad groups of the Australian Standard Classification of
# Cultural and Ethnic Groups, in that standard order; any unlisted response is
# appended alphabetically.
CULTURAL_GROUP_ORDER <- c(
  "Oceanian",
  "North-West European",
  "Southern and Eastern European",
  "North African and Middle Eastern",
  "South-East Asian",
  "North-East Asian",
  "Southern and Central Asian",
  "People of the Americas",
  "Sub-Saharan African"
)

# Household income. The raw item states a weekly and an annual range together;
# only the annual figure is kept. Named by the exact raw response text; the
# vector order fixes the table's row order (ascending, "Prefer not to say"
# last).
INCOME_LABELS <- c(
  "$0 - $599 per week ($0 - $31,199 per year)" = "$0 - $31,199",
  "$600 - $999 per week ($31,200 - $51,999 per year)" = "$31,200 - $51,999",
  "$1,000 - $1,599 per week ($52,000 - $83,199 per year)" = "$52,000 - $83,199",
  "$1,600 - $2,399 per week ($83,200 - $124,799 per year)" = "$83,200 - $124,799",
  "$2,400 - $3,199 per week ($124,800 - $166,399 per year)" = "$124,800 - $166,399",
  "$3,200 - $3,999 per week ($166,400 - $207,999 per year)" = "$166,400 - $207,999",
  "$4,000 - $5,199 per week ($208,000 - $268,399 per year)" = "$208,000 - $268,399",
  "$5,200 - $6,399 per week ($268,400 - $332,799 per year)" = "$268,400 - $332,799",
  "$6,400 - $7,999 per week ($332,800 - $415,999 per year)" = "$332,800 - $415,999",
  "$8,000+ per week ($416,000+ per year)" = "$416,000+",
  "Prefer not to say" = "Prefer not to say"
)

ADJUSTMENT_COVARIATES <- c("age_c", "gender_adj", "education_c")

# ---- Child characteristics (parent/carer sample) ----------------------------

# Eligibility was defined by two age bands, reported explicitly rather than
# folded into a summary range. Bounds are half-open (min <= age < below) so
# the bands tile the whole range with no gaps.
CHILD_AGE_BANDS <- list(
  list(label = "Under 4", min = -Inf, below = 4),
  list(label = "4--12", min = 4, below = 13),
  list(label = "13--17", min = 13, below = 18),
  list(label = "18 or older", min = 18, below = Inf)
)

# A child outside the recruited age range makes the respondent ineligible, so
# the parent/carer is excluded rather than having the value blanked.
CHILD_ELIGIBLE_AGE_MIN <- 4
CHILD_ELIGIBLE_AGE_MAX <- 17
EXCLUDE_INELIGIBLE_CHILD_AGE <- TRUE

# School grade has fifteen response options, collapsed to these groups for the
# table.
CHILD_GRADE_GROUPS <- c(
  "Not in school", "Kindergarten/Prep", "Years 1--6", "Years 7--12",
  "Prefer not to say"
)

# Categories sorted alphabetically but with these pushed to the end.
CATEGORY_LAST <- c("Other (please specify)", "Other", "Prefer not to say")

# Relationship to child: every non-biological arrangement is a single case, so
# only the modal category is reported separately.
CHILD_RELATION_KEEP <- "Biological parent"

# ---- Derived: the scale definitions actually used for modelling --------------

ANALYSIS_SCALES <- SUBSCALES
for (key in names(SCALE_OVERRIDES)) {
  ANALYSIS_SCALES[[key]][names(SCALE_OVERRIDES[[key]])] <- SCALE_OVERRIDES[[key]]
}

# ---- Inference --------------------------------------------------------------

# Model A tests six outcomes; p values are adjusted with Benjamini-Hochberg
# within each effect family (tool type, respondent type, interaction) rather
# than across all three families at once.
P_ADJUST_METHOD <- "BH"
ALPHA <- 0.05
