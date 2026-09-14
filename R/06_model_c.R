# Model C: who holds positive attitudes and intentions?
#
#   outcome ~ ai_type + respondent_type + age_c + gender_adj + education_c
#             + genai_freq_c + tech_conf_c + ai_mh_use + (1 | participant_id)
#
# Demographic and AI-familiarity predictors are focal here, not covariates. This
# is reported before Model B because perceived usefulness correlates around .75
# with the outcomes: entered alongside it, demographic predictors look inert for
# reasons of measurement overlap rather than because background characteristics
# do not matter.
#
# All predictors vary between participants only, so no within/between
# decomposition applies. A second step tests whether they predict differently
# for the two tool types.

# Runnable on its own: sets up the session if it is not already set up.
if (!exists("SETUP_DONE")) {
  .p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(.p, "R", "_setup.R"))) {
    if (identical(dirname(.p), .p)) stop("Run from inside the repository.")
    .p <- dirname(.p)
  }
  source(file.path(.p, "R", "_setup.R"))
}
require_data()

message("06_model_c")

C_PREDICTORS <- c("age_c", "gender_adj", "education_c", "genai_freq_c",
                  "tech_conf_c", "ai_mh_use")
C_MAIN_TERMS <- c("ai_type", "respondent_type", C_PREDICTORS)
C_INTERACTIONS <- paste0("ai_type:", C_PREDICTORS)

c_step_rows <- list()
c_coef_rows <- list()

for (outcome in MODEL_B_OUTCOMES) {
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label
  vars <- unique(c(outcome, "participant_id", "ai_type", "respondent_type",
                   C_PREDICTORS))
  dat <- long[stats::complete.cases(long[, vars]), , drop = FALSE]

  forms <- list(
    C1 = stats::as.formula(paste(outcome, "~", paste(C_MAIN_TERMS, collapse = " + "),
                                 "+ (1 | participant_id)")),
    C2 = stats::as.formula(paste(outcome, "~",
                                 paste(c(C_MAIN_TERMS, C_INTERACTIONS),
                                       collapse = " + "),
                                 "+ (1 | participant_id)"))
  )

  ml <- lapply(forms, function(f) fit_lmer(f, dat, reml = FALSE)$fit)
  reml <- lapply(forms, function(f) fit_lmer(f, dat, reml = TRUE))

  message("  ", outcome_label, ": n = ", length(unique(dat$participant_id)),
          " participants, ", nrow(dat), " observations")

  for (step_key in names(forms)) {
    r2 <- r2_mixed(reml[[step_key]]$fit)
    lrt <- list(chisq = NA_real_, df = NA_real_, p = NA_real_)
    if (step_key == "C2" && !is.null(ml$C1) && !is.null(ml$C2)) {
      comparison <- suppressMessages(stats::anova(ml$C1, ml$C2))
      lrt <- list(chisq = comparison$Chisq[2], df = comparison$Df[2],
                  p = comparison$`Pr(>Chisq)`[2])
    }
    c_step_rows[[length(c_step_rows) + 1L]] <- data.frame(
      outcome = outcome_label, outcome_key = outcome, step = step_key,
      step_label = if (step_key == "C1") "1. Main effects"
                   else "2. + predictor $\\times$ tool type",
      r2_marginal = r2[["marginal"]], r2_conditional = r2[["conditional"]],
      aic = if (is.null(ml[[step_key]])) NA_real_ else stats::AIC(ml[[step_key]]),
      lrt_chisq = lrt$chisq, lrt_df = lrt$df, lrt_p = lrt$p,
      singular = is_singular(reml[[step_key]]$fit),
      messages = paste(unique(reml[[step_key]]$messages), collapse = "; "),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(reml$C1$fit)) {
    c_coef_rows[[length(c_coef_rows) + 1L]] <- coefficient_table(
      reml$C1$fit, outcome = outcome_label, outcome_key = outcome, step = "C1"
    )
  }
  if (!is.null(reml$C2$fit)) {
    ct <- coefficient_table(reml$C2$fit, outcome = outcome_label,
                            outcome_key = outcome, step = "C2")
    c_coef_rows[[length(c_coef_rows) + 1L]] <- ct[grepl(":", ct$term), , drop = FALSE]
  }
}

model_c_steps <- do.call(rbind, c_step_rows)
model_c_coefficients <- do.call(rbind, c_coef_rows)

# ---- Multiplicity -----------------------------------------------------------
# Benjamini-Hochberg within each outcome, across that outcome's C1 main-effect
# predictors (standard practice for a family of simultaneous tests, matching
# the correction already applied in Model A). The C2 interaction terms are a
# different family -- tested by the omnibus likelihood-ratio test reported in
# model_c_interaction_test.tex -- and are not part of this correction.
model_c_coefficients$p_fdr <- NA_real_
c1_mask <- model_c_coefficients$step == "C1" & model_c_coefficients$term != "(Intercept)"
for (oc in unique(model_c_coefficients$outcome[c1_mask])) {
  mask <- c1_mask & model_c_coefficients$outcome == oc
  model_c_coefficients$p_fdr[mask] <- stats::p.adjust(model_c_coefficients$p[mask],
                                                       method = P_ADJUST_METHOD)
}

write_result_csv(model_c_steps, "06_model_c_steps")
write_result_csv(model_c_coefficients, "06_model_c_coefficients")

# ---- Table: main-effects coefficients, both outcomes side by side ------------

C_TERM_LABELS <- c(
  "ai_typeBespoke AI" = "Bespoke AI (vs general-purpose)",
  "respondent_typeParent" = "Parent/carer (vs adult)",
  "age_c" = "Age (centred)",
  "gender_adjFemale" = "Female (vs male)",
  "gender_adjOther or undisclosed" = "Other or undisclosed gender (vs male)",
  "education_c" = "Education (centred)",
  "genai_freq_c" = "GenAI use frequency (centred)",
  "tech_conf_c" = "Digital technology confidence (centred)",
  "ai_mh_useYes" = "Has used GenAI for mental health (vs not)"
)

c_cell <- function(outcome, term) {
  ct <- model_c_coefficients[model_c_coefficients$outcome_key == outcome &
                               model_c_coefficients$step == "C1", , drop = FALSE]
  idx <- match(term, ct$term)
  if (is.na(idx)) return(c("--", "--", "--", "--", "--"))
  c(fmt_num(ct$estimate[idx], 2),
    fmt_ci(ct$ci95_low[idx], ct$ci95_high[idx], 2),
    fmt_num(ct$t[idx], 2),
    as.character(round(ct$df[idx])),
    fmt_p(ct$p_fdr[idx]))
}

c_rows <- list()
for (term in names(C_TERM_LABELS)) {
  att <- c_cell("att", term)
  intent <- c_cell("intent", term)
  c_rows[[length(c_rows) + 1L]] <- data.frame(
    Predictor = C_TERM_LABELS[[term]],
    b_att = att[1], ci_att = att[2], t_att = att[3], df_att = att[4], p_att = att[5],
    b_int = intent[1], ci_int = intent[2], t_int = intent[3], df_int = intent[4], p_int = intent[5],
    stringsAsFactors = FALSE
  )
}

r2_line <- function(outcome) {
  row <- model_c_steps[model_c_steps$outcome_key == outcome &
                         model_c_steps$step == "C1", , drop = FALSE]
  fmt_num(row$r2_marginal, 3)
}
c_rows[[length(c_rows) + 1L]] <- data.frame(
  Predictor = "$R^2_m$",
  b_att = r2_line("att"), ci_att = "", t_att = "", df_att = "", p_att = "",
  b_int = r2_line("intent"), ci_int = "", t_int = "", df_int = "", p_int = "",
  stringsAsFactors = FALSE
)

write_apa_table(
  do.call(rbind, c_rows),
  file = file.path(TEX_DIR, "model_c_coefficients.tex"),
  caption = paste0("Demographic and generative-AI familiarity predictors of ",
                   "attitudes and behavioural intentions"),
  label = "tab:model-c",
  align = "lrcrrrcrrrr",
  size = "\\small",
  header = paste(
    "Predictor &",
    "\\multicolumn{5}{c}{Attitudes} &",
    "\\multicolumn{5}{c}{Behavioural intentions} \\\\",
    "\\cmidrule(lr){2-6} \\cmidrule(lr){7-11}",
    " & $b$ & 95\\% CI & $t$ & $df$ & $p$ & $b$ & 95\\% CI & $t$ & $df$ & $p$"
  ),
  # Model specification is in the Data Analysis section. Retained here: the
  # unit, the symbol definition, and why the perception subscales are absent --
  # without which their omission reads as an oversight.
  note = paste0(
    "$b$ is in scale points (1--7). $p$ values are Benjamini-Hochberg adjusted ",
    "within each outcome, across that outcome's eight predictors. $R^2_m$ is ",
    "the marginal (fixed-effects) $R^2$. The perception subscales are excluded ",
    "here and enter as predictors in Tables \\ref{tab:model-b-coef-att} and ",
    "\\ref{tab:model-b-coef-intent}."
  )
)

# ---- Table: interaction block test ------------------------------------------

interaction_tests <- model_c_steps[model_c_steps$step == "C2", , drop = FALSE]
write_apa_table(
  data.frame(
    Outcome = interaction_tests$outcome,
    chisq = fmt_num(interaction_tests$lrt_chisq, 2),
    df = as.character(interaction_tests$lrt_df),
    p = fmt_p(interaction_tests$lrt_p),
    stringsAsFactors = FALSE
  ),
  file = file.path(TEX_DIR, "model_c_interaction_test.tex"),
  caption = paste0("Likelihood-ratio tests of whether demographic and ",
                   "familiarity predictors differ by AI tool type"),
  label = "tab:model-c-interactions",
  align = "lrrr",
  header = c("Outcome", "$\\chi^2$", "$df$", "$p$"),
  # The columns do not say what is being compared, so that stays. What a
  # non-significant test means is interpretation and belongs in the text.
  note = paste0(
    "Each test compares a model with all predictor $\\times$ tool type ",
    "interactions against the corresponding main-effects model."
  )
)

for (i in seq_len(nrow(interaction_tests))) {
  message("  ", interaction_tests$outcome[i], " interaction block: chi2(",
          interaction_tests$lrt_df[i], ") = ",
          fmt_num(interaction_tests$lrt_chisq[i], 2), ", p = ",
          fmt_p_plain(interaction_tests$lrt_p[i]))
}
