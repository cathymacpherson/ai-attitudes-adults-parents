# Residual diagnostics and a heteroscedasticity sensitivity check.
#
# Every model here fits a bounded 1-7 subscale mean with a random intercept,
# which is prone to two things at once: skewed residuals (ceiling effects
# compress the upper end of the scale) and heteroscedasticity (residual
# variance shrinks as the fitted value approaches the ceiling, since there is
# less room left to vary). Neither breaks the point estimates, but
# heteroscedasticity can bias the model-based standard errors, so every
# headline coefficient is re-estimated with cluster-robust (CR2) SEs,
# clustered on participant_id, and compared to the standard model-based
# result. clubSandwich, not sandwich: sandwich does not support merMod
# objects, and CR2 is the small-sample-appropriate cluster-robust estimator
# for a design with few observations per cluster (two, here).
#
# The "standard" p compared against is whatever the paper actually reports,
# not a raw p nobody sees: Model A's is FDR-adjusted within its "Tool type
# (overall)" family (04_model_a.R); Model C's is FDR-adjusted within each
# outcome's eight predictors (06_model_c.R, added for the same reason Model A
# already has it -- a family of simultaneous tests). Model B's single ai_type
# coefficient per outcome is not part of any such family and is left as the
# raw p, matching model_b_tool_effect.tex. Whichever correction the standard
# column carries, the robust column is put through the identical one, so a
# changed conclusion reflects the SE estimator and not a mismatched multiplicity
# adjustment on one side only.

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

if (!exists("model_a_effects", envir = globalenv())) {
  source(file.path(R_DIR, "04_model_a.R"), local = globalenv())
}
if (!exists("model_b_tool_effect", envir = globalenv())) {
  source(file.path(R_DIR, "05_model_b.R"), local = globalenv())
}
if (!exists("model_c_coefficients", envir = globalenv())) {
  source(file.path(R_DIR, "06_model_c.R"), local = globalenv())
}

suppressPackageStartupMessages(library(clubSandwich))

message("11_diagnostics")

skewness <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / stats::sd(x)^3
}
kurtosis_excess <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^4) / stats::sd(x)^4 - 3
}

# ---- Refit helpers ------------------------------------------------------
# Each model script reports coefficient/contrast tables but does not keep the
# fitted merMod object itself, so the headline specification is refit here,
# identically to how each model script builds it.

refit_model_a <- function(outcome) {
  vars <- c(outcome, "ai_type", "respondent_type", "participant_id", ADJUSTMENT_COVARIATES)
  dat <- long[stats::complete.cases(long[, vars]), , drop = FALSE]
  form <- stats::as.formula(paste(
    outcome, "~ ai_type * respondent_type +",
    paste(ADJUSTMENT_COVARIATES, collapse = " + "), "+ (1 | participant_id)"
  ))
  list(fit = lmerTest::lmer(form, data = dat, REML = TRUE), dat = dat)
}

refit_model_b <- function(outcome) {
  steps <- step_definitions(outcome)
  dat <- analysis_rows(outcome, steps[[length(steps)]]$terms)
  form <- model_formula(outcome, steps[["S3"]]$terms)
  list(fit = lmerTest::lmer(form, data = dat, REML = TRUE), dat = dat)
}

refit_model_c <- function(outcome) {
  vars <- unique(c(outcome, "participant_id", "ai_type", "respondent_type", C_PREDICTORS))
  dat <- long[stats::complete.cases(long[, vars]), , drop = FALSE]
  form <- stats::as.formula(paste(
    outcome, "~", paste(C_MAIN_TERMS, collapse = " + "), "+ (1 | participant_id)"
  ))
  list(fit = lmerTest::lmer(form, data = dat, REML = TRUE), dat = dat)
}

# ---- Residual diagnostics ------------------------------------------------

diagnose_residuals <- function(fit, dv, dat, model, outcome) {
  res <- stats::residuals(fit)
  ranef_vals <- lme4::ranef(fit)$participant_id[, 1]
  sw_resid <- stats::shapiro.test(if (length(res) > 5000) sample(res, 5000) else res)
  sw_ranef <- stats::shapiro.test(ranef_vals)
  het_p <- summary(stats::lm(abs(res) ~ stats::fitted(fit)))$coefficients[2, 4]
  data.frame(
    model = model, outcome = outcome, n_obs = length(res),
    dv_skew = skewness(dat[[dv]]), dv_kurtosis_excess = kurtosis_excess(dat[[dv]]),
    resid_skew = skewness(res), resid_kurtosis_excess = kurtosis_excess(res),
    shapiro_resid_p = sw_resid$p.value, shapiro_ranef_p = sw_ranef$p.value,
    heteroscedasticity_p = het_p, singular = lme4::isSingular(fit, tol = 1e-4),
    stringsAsFactors = FALSE
  )
}

residual_rows <- list()
robust_rows <- list()

# ---- Model A: the "Tool type (overall)" contrast, all six outcomes ---------
# Robust SE requires overriding emmeans' covariance matrix; Kenward-Roger
# cannot be used with a user-supplied covariance, so this falls back to
# Satterthwaite for both the standard and robust contrast, which keeps them
# comparable.
#
# The standard column uses model_a_effects$p_fdr -- the value 04_model_a.R
# actually reports -- not the raw contrast p, so this is a comparison against
# what the paper says rather than against an uncorrected number nobody sees.
# The robust column is put through the identical BH correction (same family:
# the "Tool type (overall)" effect, across the six outcomes), so both sides of
# the comparison carry the same multiplicity adjustment and differ only in
# whether the SE is model-based or cluster-robust.

model_a_rows <- list()
for (outcome in MODEL_A_OUTCOMES) {
  label <- ANALYSIS_SCALES[[outcome]]$label
  rf <- refit_model_a(outcome)
  residual_rows[[length(residual_rows) + 1L]] <-
    diagnose_residuals(rf$fit, outcome, rf$dat, "A (adjusted)", label)

  emm_std <- emmeans::emmeans(rf$fit, ~ai_type, weights = "proportional")
  con_std <- emmeans::contrast(emm_std, "revpairwise")
  ci_std <- as.data.frame(stats::confint(con_std))
  con_std <- as.data.frame(con_std)

  V <- clubSandwich::vcovCR(rf$fit, cluster = rf$dat$participant_id, type = "CR2")
  emm_rob <- suppressMessages(
    emmeans::emmeans(rf$fit, ~ai_type, weights = "proportional", vcov. = V)
  )
  con_rob <- emmeans::contrast(emm_rob, "revpairwise")
  ci_rob <- as.data.frame(stats::confint(con_rob))
  con_rob <- as.data.frame(con_rob)

  p_fdr_std <- model_a_effects$p_fdr[
    model_a_effects$model == "Adjusted" &
      model_a_effects$effect == "Tool type (overall)" &
      model_a_effects$outcome == label
  ]

  model_a_rows[[length(model_a_rows) + 1L]] <- data.frame(
    model = "A", outcome = label, term = "Bespoke - general (overall)",
    estimate = con_std$estimate,
    se_standard = con_std$SE, p_standard = p_fdr_std,
    ci_low_standard = ci_std$lower.CL, ci_high_standard = ci_std$upper.CL,
    se_robust = con_rob$SE, p_robust_raw = con_rob$p.value,
    ci_low_robust = ci_rob$lower.CL, ci_high_robust = ci_rob$upper.CL,
    stringsAsFactors = FALSE
  )
}
model_a_block <- do.call(rbind, model_a_rows)
model_a_block$p_robust <- stats::p.adjust(model_a_block$p_robust_raw, method = P_ADJUST_METHOD)
model_a_block$p_robust_raw <- NULL
robust_rows[[length(robust_rows) + 1L]] <- model_a_block

# ---- Model B: the direct-effect ai_type coefficient (step S3), both outcomes -

for (outcome in MODEL_B_OUTCOMES) {
  label <- ANALYSIS_SCALES[[outcome]]$label
  rf <- refit_model_b(outcome)
  residual_rows[[length(residual_rows) + 1L]] <-
    diagnose_residuals(rf$fit, outcome, rf$dat, "B (step S3)", label)

  ct_std <- coefficient_table(rf$fit)
  ct_rob <- clubSandwich::coef_test(rf$fit, vcov = "CR2", cluster = rf$dat$participant_id)

  term <- "ai_typeBespoke AI"
  std_row <- ct_std[ct_std$term == term, ]
  rob_row <- ct_rob[ct_rob$Coef == term, ]
  crit_rob <- stats::qt(0.975, rob_row$df_Satt)

  robust_rows[[length(robust_rows) + 1L]] <- data.frame(
    model = "B", outcome = label, term = "ai_type (direct effect)",
    estimate = std_row$estimate,
    se_standard = std_row$std_error, p_standard = std_row$p,
    ci_low_standard = std_row$ci95_low, ci_high_standard = std_row$ci95_high,
    se_robust = rob_row$SE, p_robust = rob_row$p_Satt,
    ci_low_robust = rob_row$beta - crit_rob * rob_row$SE,
    ci_high_robust = rob_row$beta + crit_rob * rob_row$SE,
    stringsAsFactors = FALSE
  )
}

# ---- Model C: every C1 main-effects coefficient, both outcomes -------------
# Same logic as Model A: the standard column is model_c_coefficients$p_fdr
# (06_model_c.R's own BH correction, within outcome across that outcome's
# eight predictors -- the correction requested for this model specifically),
# and the robust column is put through the identical per-outcome correction so
# the comparison isn't confounding "robust vs. not" with "corrected vs. not".

model_c_rows <- list()
for (outcome in MODEL_B_OUTCOMES) {
  label <- ANALYSIS_SCALES[[outcome]]$label
  rf <- refit_model_c(outcome)
  residual_rows[[length(residual_rows) + 1L]] <-
    diagnose_residuals(rf$fit, outcome, rf$dat, "C (C1)", label)

  ct_std <- coefficient_table(rf$fit)
  ct_rob <- clubSandwich::coef_test(rf$fit, vcov = "CR2", cluster = rf$dat$participant_id)

  for (term in setdiff(ct_std$term, "(Intercept)")) {
    std_row <- ct_std[ct_std$term == term, ]
    rob_row <- ct_rob[ct_rob$Coef == term, ]
    crit_rob <- stats::qt(0.975, rob_row$df_Satt)
    term_label <- if (term %in% names(C_TERM_LABELS)) C_TERM_LABELS[[term]] else term

    p_fdr_std <- model_c_coefficients$p_fdr[
      model_c_coefficients$outcome == label &
        model_c_coefficients$step == "C1" &
        model_c_coefficients$term == term
    ]

    model_c_rows[[length(model_c_rows) + 1L]] <- data.frame(
      model = "C", outcome = label, term = term_label,
      estimate = std_row$estimate,
      se_standard = std_row$std_error, p_standard = p_fdr_std,
      ci_low_standard = std_row$ci95_low, ci_high_standard = std_row$ci95_high,
      se_robust = rob_row$SE, p_robust_raw = rob_row$p_Satt,
      ci_low_robust = rob_row$beta - crit_rob * rob_row$SE,
      ci_high_robust = rob_row$beta + crit_rob * rob_row$SE,
      stringsAsFactors = FALSE
    )
  }
}
model_c_block <- do.call(rbind, model_c_rows)
model_c_block$p_robust <- NA_real_
for (oc in unique(model_c_block$outcome)) {
  mask <- model_c_block$outcome == oc
  model_c_block$p_robust[mask] <- stats::p.adjust(model_c_block$p_robust_raw[mask],
                                                   method = P_ADJUST_METHOD)
}
model_c_block$p_robust_raw <- NULL
robust_rows[[length(robust_rows) + 1L]] <- model_c_block

residual_diagnostics <- do.call(rbind, residual_rows)
robust_se_comparison <- do.call(rbind, robust_rows)
robust_se_comparison$sig_standard <- robust_se_comparison$p_standard < ALPHA
robust_se_comparison$sig_robust <- robust_se_comparison$p_robust < ALPHA
robust_se_comparison$conclusion_changes <-
  robust_se_comparison$sig_standard != robust_se_comparison$sig_robust

write_result_csv(residual_diagnostics, "11_residual_diagnostics")
write_result_csv(robust_se_comparison, "11_robust_se_comparison")

# Numbers are still computed and saved (11_robust_se_comparison.csv) for
# reference; no robust_se_comparison.tex table is written.

write_apa_table(
  data.frame(
    Model = residual_diagnostics$model,
    Outcome = residual_diagnostics$outcome,
    dv_skew = fmt_num(residual_diagnostics$dv_skew, 2),
    resid_skew = fmt_num(residual_diagnostics$resid_skew, 2),
    resid_kurt = fmt_num(residual_diagnostics$resid_kurtosis_excess, 2),
    shapiro_p = fmt_p(residual_diagnostics$shapiro_resid_p),
    het_p = fmt_p(residual_diagnostics$heteroscedasticity_p),
    stringsAsFactors = FALSE
  ),
  file = file.path(TEX_DIR, "residual_diagnostics.tex"),
  caption = "Residual diagnostics for every fitted model",
  label = "tab:residual-diagnostics",
  align = "llrrrrr",
  header = c("Model", "Outcome", "DV skew", "Resid. skew", "Resid. kurtosis",
             "Shapiro $p$", "Heterosc. $p$"),
  note = paste0(
    "DV skew is on the raw 1--7 composite; residual skew/kurtosis (excess) are ",
    "from the fitted model. Shapiro-Wilk on residuals is near-guaranteed to be ",
    "significant at this $n$ even for minor deviations, so it is reported for ",
    "completeness rather than as a pass/fail criterion; magnitude and the ",
    "heteroscedasticity test are more informative here."
  )
)

n_changed <- sum(robust_se_comparison$conclusion_changes)
message("  ", nrow(residual_diagnostics), " models checked for residual diagnostics")
message("  ", nrow(robust_se_comparison), " coefficients/contrasts re-estimated with cluster-robust (CR2) SEs")
message("  ", n_changed, " of ", nrow(robust_se_comparison),
        " change significance conclusion (p < .05) under cluster-robust SEs")
if (n_changed > 0) {
  changed <- robust_se_comparison[robust_se_comparison$conclusion_changes,
                                   c("model", "outcome", "term", "p_standard", "p_robust")]
  message("  changed: ", paste(
    sprintf("%s/%s/%s (p %.3f -> %.3f)", changed$model, changed$outcome, changed$term,
            changed$p_standard, changed$p_robust),
    collapse = "; "
  ))
}
