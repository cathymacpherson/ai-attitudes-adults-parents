# Build the analysis frames from the scored dataset.
#
# Produces two objects:
#   participants -- one row per participant (covariates, comfort item)
#   long         -- two rows per participant (one per AI tool type), with the
#                   eight subscale scores plus within/between decompositions

# Runnable on its own: sets up the session if it is not already set up.
if (!exists("SETUP_DONE")) {
  .p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(.p, "R", "_setup.R"))) {
    if (identical(dirname(.p), .p)) stop("Run from inside the repository.")
    .p <- dirname(.p)
  }
  source(file.path(.p, "R", "_setup.R"))
}

message("01_load_prepare: reading ", basename(DATA_FILE))
raw <- utils::read.csv(DATA_FILE, stringsAsFactors = FALSE, check.names = FALSE)

# ---- Participant-level frame ------------------------------------------------

participants <- data.frame(
  participant_id = as.character(raw$participant_id),
  participant_type = tolower(trimws(as.character(raw$participant_type))),
  stringsAsFactors = FALSE
)

participants$respondent_type <- ifelse(
  participants$participant_type == "self", "Adult",
  ifelse(participants$participant_type == "parent", "Parent", NA_character_)
)
participants$report_key <- ifelse(participants$respondent_type == "Adult", "s", "p")

# Age in years; blank if not reported.
participants$age <- suppressWarnings(as.numeric(
  sub("^.*?(-?[0-9]+(\\.[0-9]+)?).*$", "\\1", trimws(as.character(raw$demo_age)))
))

gender_raw <- trimws(as.character(raw$demo_gender))
participants$gender <- factor(
  ifelse(gender_raw == "Male", "Male",
         ifelse(gender_raw == "Female", "Female", "Other or undisclosed")),
  levels = GENDER_LEVELS
)

participants$education <- map_scores(raw$demo_education, EDUCATION_SCORES)

# GenAI use frequency: report-group specific column. Adults who answered "No"
# or "Not sure" to ever having used GenAI are scored 0 (they were not shown the
# frequency item); parents were not asked the equivalent gate question.
genai_freq <- rep(NA_real_, nrow(raw))
is_adult <- participants$report_key == "s"
genai_freq[is_adult] <- map_scores(raw$gai_s_freq[is_adult], GENAI_FREQUENCY_SCORES)
genai_freq[!is_adult] <- map_scores(raw$gai_p_freq[!is_adult], GENAI_FREQUENCY_SCORES)
if ("gai_ever" %in% names(raw)) {
  never <- trimws(as.character(raw$gai_ever)) %in% c("No", "Not sure")
  genai_freq[is_adult & never] <- 0
}
participants$genai_freq <- genai_freq

tech_conf <- rep(NA_real_, nrow(raw))
tech_conf[is_adult] <- map_scores(raw$tech_s_conf[is_adult], CONFIDENCE_SCORES)
tech_conf[!is_adult] <- map_scores(raw$tech_p_conf[!is_adult], CONFIDENCE_SCORES)
participants$tech_conf <- tech_conf

# Prior use of generative AI for mental health purposes (own mental health for
# adults, the child's for parents/carers). "Not sure" is grouped with "No". A
# blank response is missing, except for adults who answered "No" or "Not sure"
# to ever having used GenAI: they were not shown the item, so are scored "No",
# matching the frequency gate above.
mh_use_response <- rep(NA_character_, nrow(raw))
mh_use_response[is_adult] <- trimws(as.character(raw$gai_s_mh_use[is_adult]))
mh_use_response[!is_adult] <- trimws(as.character(raw$gai_p_mh_use[!is_adult]))
used_ai_mh <- ifelse(is.na(mh_use_response) | !nzchar(mh_use_response), NA,
                     mh_use_response == "Yes")
if ("gai_ever" %in% names(raw)) {
  used_ai_mh[is_adult & never] <- FALSE
}
participants$used_ai_mh <- used_ai_mh
# Factor form for modelling; the logical form above is used for descriptives.
participants$ai_mh_use <- factor(
  ifelse(is.na(used_ai_mh), NA_character_, ifelse(used_ai_mh, "Yes", "No")),
  levels = c("No", "Yes")
)

# Direct purpose-built-vs-general comfort item (Model D).
comfort <- rep(NA_real_, nrow(raw))
comfort[is_adult] <- parse_likert(raw$attn_comp_s[is_adult])
comfort[!is_adult] <- parse_likert(raw$attn_comp_p[!is_adult])
participants$comfort <- comfort

# ---- Sample selection, with an auditable flow -------------------------------

flow <- list()
add_step <- function(label, keep) {
  flow[[length(flow) + 1L]] <<- data.frame(
    step = label,
    n_total = sum(keep),
    n_adult = sum(keep & participants$respondent_type == "Adult", na.rm = TRUE),
    n_parent = sum(keep & participants$respondent_type == "Parent", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  keep
}

# The dataset contains the analysed sample only: every participant exclusion,
# including parents/carers whose child was outside the eligible age range, is
# applied before the data are released.
keep <- add_step("Cleaned and scored dataset", !is.na(participants$respondent_type))

# Implausible parent/carer ages are blanked rather than excluded: these
# respondents appear to have entered their child's age instead of their own, so
# the age value is unusable but the respondent is not. This changes no counts,
# and is recorded in the flow table so the decision is visible alongside the
# exclusions.
implausible_age <- participants$respondent_type == "Parent" &
  !is.na(participants$age) & participants$age < MIN_PLAUSIBLE_PARENT_AGE
n_implausible_age <- sum(implausible_age & keep)
participants$age[implausible_age] <- NA_real_
keep <- add_step(
  sprintf("Implausible parent/carer age set to missing (n = %d, retained)",
          n_implausible_age),
  keep
)

# Gender is an adjustment covariate in every model, and the "Other or
# undisclosed" cell is too small to estimate a coefficient for. gender_adj
# copies participants$gender with this level set to NA; participants$gender
# itself stays intact, so these respondents remain in descriptives,
# reliability, and any model that does not condition on gender (see
# BLANK_OTHER_GENDER in 00_config.R). Not part of the sample_flow table below:
# no one is added to or removed from the sample by this step.
participants$gender_adj <- participants$gender
n_other_gender <- 0L
if (BLANK_OTHER_GENDER) {
  other_gender <- participants$gender == "Other or undisclosed"
  n_other_gender <- sum(other_gender & keep)
  participants$gender_adj[other_gender] <- NA
}

sample_flow <- do.call(rbind, flow)
participants <- participants[keep, , drop = FALSE]

if (n_implausible_age > 0) {
  message("  NOTE: ", n_implausible_age, " parent/carer respondent(s) reported an ",
          "age below ", MIN_PLAUSIBLE_PARENT_AGE, "; age set to missing and the ",
          "respondent retained. They drop out of age-adjusted models only.")
}

if (BLANK_OTHER_GENDER && n_other_gender > 0) {
  message("  NOTE: ", n_other_gender, " respondent(s) reported other/undisclosed gender; ",
          "retained in the sample and in descriptives/reliability, but excluded (via ",
          "gender_adj) from Models A (adjusted), B, C and D, which all adjust for ",
          "gender (see BLANK_OTHER_GENDER in 00_config.R).")
}

under18_adults <- sum(participants$respondent_type == "Adult" &
                        !is.na(participants$age) & participants$age < 18)
if (FLAG_UNDER18_ADULTS && under18_adults > 0) {
  message("  NOTE: ", under18_adults, " retained adult respondent(s) are aged under 18. ",
          "Adolescents aged 16-17 were eligible to self-report, so these ages are ",
          "plausible; restricting the adult sample to 18+ would be a separate choice ",
          "(see FLAG_UNDER18_ADULTS in 00_config.R).")
}

participants$respondent_type <- factor(participants$respondent_type,
                                       levels = RESPONDENT_LEVELS)
participants$gender <- droplevels(participants$gender)
participants$gender_adj <- droplevels(participants$gender_adj)

# ---- Centred covariates -----------------------------------------------------
# Centred at the participant level, so the intercept is the sample-average
# respondent.

participants$age_c <- grand_center(participants$age)
participants$education_c <- grand_center(participants$education)
participants$genai_freq_c <- grand_center(participants$genai_freq)
participants$tech_conf_c <- grand_center(participants$tech_conf)

# ---- Long frame: one row per participant per AI tool type -------------------
# Adult and parent scores live in different columns (attn_gen_s_* vs
# attn_gen_p_*) and are coalesced into a single score column here, with
# respondent_type carrying the distinction.
#
# This pools the self- and parent-worded versions of the same 18 items, which
# assumes comparable measurement across report target. Measurement invariance
# cannot be tested meaningfully at this n; the assumption is declared as a
# limitation rather than treated as established.

raw_by_id <- raw[match(participants$participant_id, as.character(raw$participant_id)), , drop = FALSE]

blocks <- list()
for (tool_key in names(TOOLS)) {
  for (rg in names(REPORT_GROUPS)) {
    rows <- participants$report_key == rg
    if (!any(rows)) next
    prefix <- paste0("attn_", tool_key, "_", rg)
    block <- raw_by_id[rows, , drop = FALSE]

    # ANALYSIS_SCALES, not SUBSCALES: the privacy override is applied here, and
    # is justified by the reliability analysis in 02_reliability.R, which uses
    # the questionnaire as specified.
    scores <- lapply(ANALYSIS_SCALES, function(s) {
      score_items(block, prefix, s$items,
                  reverse = REVERSE_CONCERN_SCALES && s$reverse)
    })
    names(scores) <- names(ANALYSIS_SCALES)

    for (nm in names(STANDALONE_ITEMS)) {
      spec <- STANDALONE_ITEMS[[nm]]
      scores[[nm]] <- score_items(
        block, prefix, spec$item,
        reverse = REVERSE_CONCERN_SCALES && isTRUE(spec$reverse)
      )
    }

    blocks[[paste(tool_key, rg)]] <- cbind(
      participants[rows, , drop = FALSE],
      data.frame(ai_type = TOOLS[[tool_key]], stringsAsFactors = FALSE),
      as.data.frame(scores, stringsAsFactors = FALSE)
    )
  }
}

long <- do.call(rbind, blocks)
rownames(long) <- NULL
long$ai_type <- factor(long$ai_type, levels = AI_TYPE_LEVELS)
long <- long[order(long$participant_id, long$ai_type), , drop = FALSE]

# ---- Within/between decomposition of the Model B predictors -----------------

long <- add_within_between(long, MODEL_B_PREDICTORS)
long <- add_within_between(long, "att")  # needed as a predictor in the intention model

# ---- Report and save --------------------------------------------------------

message("  participants: ", nrow(participants),
        " (adults ", sum(participants$respondent_type == "Adult"),
        ", parents ", sum(participants$respondent_type == "Parent"), ")")
message("  long rows: ", nrow(long))

# Numbers are still computed and saved (00_sample_flow.csv) for reference;
# no sample_flow.tex table is written.
write_result_csv(sample_flow, "00_sample_flow")
write_result_csv(long, "01_analysis_long")
