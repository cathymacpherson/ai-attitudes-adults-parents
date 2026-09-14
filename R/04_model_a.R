# Model A: how are the two AI tool types perceived?
#
#   outcome ~ ai_type * respondent_type + age_c + gender_adj + education_c
#             + (1 | participant_id)
#
# Six outcomes: perceived usefulness, ease of use, trust, privacy concern,
# human oversight preference, environmental concern. Attitudes and behavioural
# intentions are NOT outcomes here -- they are the outcomes of Model B, where
# these six enter as predictors.
#
# Random intercept only. With two observations per participant a random slope
# for ai_type is not identifiable and would return a singular fit.
#
# gender_adj has only two levels here (Male/Female): the three respondents
# with "Other or undisclosed" gender are excluded from this adjusted model via
# complete-case selection, not merely down-weighted -- see BLANK_OTHER_GENDER
# in 00_config.R. Estimated means and contrasts come from emmeans with
# proportional weighting, so averaging over gender_adj reflects the Male/
# Female sample composition rather than treating them as equally sized.

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

message("04_model_a")

# Satterthwaite is requested explicitly so that emmeans contrasts use the same
# denominator degrees of freedom as the reported coefficients (emmeans would
# otherwise switch to Kenward-Roger when pbkrtest is available).
emmeans::emm_options(lmer.df = "satterthwaite",
                     lmerTest.limit = 5000, pbkrtest.limit = 5000)

subscale_labels_a <- unname(
  vapply(ANALYSIS_SCALES[MODEL_A_OUTCOMES], function(s) s$label, character(1))
)

UNADJUSTED_RHS <- "ai_type * respondent_type + (1 | participant_id)"
ADJUSTED_RHS <- paste(
  "ai_type * respondent_type +",
  paste(ADJUSTMENT_COVARIATES, collapse = " + "),
  "+ (1 | participant_id)"
)

# Within-participant standardised effect size for the tool-type contrast,
# computed directly from the paired differences rather than from the model, so
# it is interpretable in the usual d_z sense.
cohens_dz <- function(outcome, subset_rows) {
  d <- long[subset_rows, c("participant_id", "ai_type", outcome)]
  gen <- d[d$ai_type == "General-purpose AI", c("participant_id", outcome)]
  besp <- d[d$ai_type == "Bespoke AI", c("participant_id", outcome)]
  paired <- merge(gen, besp, by = "participant_id", suffixes = c("_gen", "_besp"))
  diff <- paired[[paste0(outcome, "_besp")]] - paired[[paste0(outcome, "_gen")]]
  diff <- diff[!is.na(diff)]
  if (length(diff) < 3L) return(c(dz = NA_real_, n = length(diff)))
  c(dz = mean(diff) / stats::sd(diff), n = length(diff))
}

coef_rows <- list()
effect_rows <- list()
emmean_rows <- list()
fit_rows <- list()

for (outcome in MODEL_A_OUTCOMES) {
  label <- ANALYSIS_SCALES[[outcome]]$label

  for (model_name in c("Unadjusted", "Adjusted")) {
    rhs <- if (model_name == "Unadjusted") UNADJUSTED_RHS else ADJUSTED_RHS
    formula <- stats::as.formula(paste(outcome, "~", rhs))

    # Complete cases on the model's own variables, so the reported n matches
    # what was actually fitted.
    model_vars <- c(outcome, "ai_type", "respondent_type", "participant_id",
                    if (model_name == "Adjusted") ADJUSTMENT_COVARIATES)
    dat <- long[stats::complete.cases(long[, model_vars]), , drop = FALSE]

    res <- fit_lmer(formula, dat)
    fit <- res$fit

    fit_rows[[length(fit_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = model_name,
      formula = paste(outcome, "~", rhs),
      n_observations = nrow(dat),
      n_participants = length(unique(dat$participant_id)),
      n_adults = length(unique(dat$participant_id[dat$respondent_type == "Adult"])),
      n_parents = length(unique(dat$participant_id[dat$respondent_type == "Parent"])),
      singular = is_singular(fit),
      random_intercept_var = random_intercept_var(fit),
      residual_var = residual_var(fit),
      messages = paste(unique(res$messages), collapse = "; "),
      stringsAsFactors = FALSE
    )

    if (is.null(fit)) next

    # Fixed-effect coefficients with Satterthwaite df via lmerTest. CIs are
    # built from those same df rather than confint(), so every interval in the
    # output uses one consistent denominator.
    sm <- summary(fit)$coefficients
    crit <- stats::qt(0.975, sm[, "df"])
    coef_rows[[length(coef_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = model_name,
      term = rownames(sm),
      estimate = sm[, "Estimate"], std_error = sm[, "Std. Error"],
      df = sm[, "df"], t = sm[, "t value"], p = sm[, "Pr(>|t|)"],
      ci95_low = sm[, "Estimate"] - crit * sm[, "Std. Error"],
      ci95_high = sm[, "Estimate"] + crit * sm[, "Std. Error"],
      row.names = NULL, stringsAsFactors = FALSE
    )

    # --- Contrasts -----------------------------------------------------------
    # emmeans notes that marginal means "may be misleading due to involvement
    # in interactions". That is expected here and is the intended quantity: the
    # overall tool-type effect is the marginal main effect averaged over
    # respondent group, and the group-specific effects are reported alongside
    # it so the interaction is never hidden. The note is muffled rather than
    # left to print once per model.
    emm <- function(spec) {
      suppressMessages(emmeans::emmeans(fit, spec, weights = "proportional"))
    }
    emm_tool_by_group <- emm(~ ai_type | respondent_type)
    emm_group_by_tool <- emm(~ respondent_type | ai_type)
    emm_tool <- emm(~ ai_type)
    emm_group <- emm(~ respondent_type)

    grab <- function(obj, effect, level = "") {
      d <- as.data.frame(stats::confint(obj))
      s <- as.data.frame(obj)
      data.frame(
        outcome = label, outcome_key = outcome, model = model_name,
        effect = effect, level = level,
        contrast = as.character(s$contrast),
        estimate = s$estimate, std_error = s$SE, df = s$df,
        t = s$t.ratio, p = s$p.value,
        ci95_low = d$lower.CL, ci95_high = d$upper.CL,
        row.names = NULL, stringsAsFactors = FALSE
      )
    }

    # Bespoke minus general, overall and within each respondent group.
    tool_overall <- emmeans::contrast(emm_tool, "revpairwise")
    effect_rows[[length(effect_rows) + 1L]] <-
      grab(tool_overall, "Tool type (overall)")

    tool_by_group <- emmeans::contrast(emm_tool_by_group, "revpairwise")
    by_group <- as.data.frame(tool_by_group)
    for (i in seq_len(nrow(by_group))) {
      sub <- tool_by_group[i]
      effect_rows[[length(effect_rows) + 1L]] <-
        grab(sub, "Tool type (within group)",
             level = as.character(by_group$respondent_type[i]))
    }

    # Parent minus adult, overall and within each tool type.
    effect_rows[[length(effect_rows) + 1L]] <-
      grab(emmeans::contrast(emm_group, "revpairwise"), "Respondent type (overall)")
    group_by_tool <- emmeans::contrast(emm_group_by_tool, "revpairwise")
    gbt <- as.data.frame(group_by_tool)
    for (i in seq_len(nrow(gbt))) {
      effect_rows[[length(effect_rows) + 1L]] <-
        grab(group_by_tool[i], "Respondent type (within tool)",
             level = as.character(gbt$ai_type[i]))
    }

    # Interaction: does the tool-type gap differ between adults and parents?
    interaction <- emmeans::contrast(
      emm(~ ai_type * respondent_type),
      interaction = c("revpairwise", "revpairwise")
    )
    int_df <- as.data.frame(interaction)
    int_ci <- as.data.frame(stats::confint(interaction))
    effect_rows[[length(effect_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = model_name,
      effect = "Tool type x respondent type", level = "",
      contrast = paste(int_df[[1]], int_df[[2]], sep = " x "),
      estimate = int_df$estimate, std_error = int_df$SE, df = int_df$df,
      t = int_df$t.ratio, p = int_df$p.value,
      ci95_low = int_ci$lower.CL, ci95_high = int_ci$upper.CL,
      row.names = NULL, stringsAsFactors = FALSE
    )

    # Estimated means for the figure and appendix table, by respondent group.
    em <- as.data.frame(emm_tool_by_group)
    emmean_rows[[length(emmean_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = model_name,
      ai_type = as.character(em$ai_type),
      respondent_type = as.character(em$respondent_type),
      emmean = em$emmean, std_error = em$SE, df = em$df,
      ci95_low = em$lower.CL, ci95_high = em$upper.CL,
      row.names = NULL, stringsAsFactors = FALSE
    )

    # Pooled over respondent group, recorded as respondent_type "Overall".
    # These are the means quoted alongside the pooled contrast in the text: the
    # difference between the two reproduces b exactly, which the group-specific
    # means do not.
    em_pooled <- as.data.frame(emm_tool)
    emmean_rows[[length(emmean_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = model_name,
      ai_type = as.character(em_pooled$ai_type),
      respondent_type = "Overall",
      emmean = em_pooled$emmean, std_error = em_pooled$SE, df = em_pooled$df,
      ci95_low = em_pooled$lower.CL, ci95_high = em_pooled$upper.CL,
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  # Descriptive within-person effect sizes. "Overall" is computed on the
  # combined difference scores, so it matches the granularity of the pooled
  # model contrast reported alongside it; it is not the average of the two
  # group values. The group-specific values accompany the group-specific
  # estimated means.
  dz_groups <- c(
    list(list(label = "Overall", rows = rep(TRUE, nrow(long)))),
    lapply(levels(long$respondent_type), function(rt) {
      list(label = rt, rows = long$respondent_type == rt)
    })
  )
  for (spec in dz_groups) {
    dz <- cohens_dz(outcome, spec$rows)
    effect_rows[[length(effect_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = "Descriptive",
      effect = "Cohen's dz (bespoke - general)", level = spec$label,
      contrast = "Bespoke AI - General-purpose AI",
      estimate = unname(dz["dz"]), std_error = NA_real_, df = NA_real_,
      t = NA_real_, p = NA_real_, ci95_low = NA_real_, ci95_high = NA_real_,
      row.names = NULL, stringsAsFactors = FALSE
    )
  }
}

model_a_fits <- do.call(rbind, fit_rows)
model_a_coefficients <- do.call(rbind, coef_rows)
model_a_effects <- do.call(rbind, effect_rows)
model_a_emmeans <- do.call(rbind, emmean_rows)

# ---- Multiplicity -----------------------------------------------------------
# Benjamini-Hochberg across the six outcomes, applied within each effect family
# separately rather than across all families at once.

model_a_effects$p_fdr <- NA_real_
families <- unique(model_a_effects[, c("model", "effect", "level")])
for (i in seq_len(nrow(families))) {
  mask <- model_a_effects$model == families$model[i] &
    model_a_effects$effect == families$effect[i] &
    model_a_effects$level == families$level[i] &
    !is.na(model_a_effects$p)
  if (!any(mask)) next
  model_a_effects$p_fdr[mask] <- stats::p.adjust(model_a_effects$p[mask],
                                                 method = P_ADJUST_METHOD)
}

write_result_csv(model_a_fits, "04_model_a_fit_summary")
write_result_csv(model_a_coefficients, "04_model_a_coefficients")
write_result_csv(model_a_effects, "04_model_a_effects")
write_result_csv(model_a_emmeans, "04_model_a_estimated_means")

# ---- Main table -------------------------------------------------------------

effect_cell <- function(effect, level, model = "Adjusted") {
  idx <- match(paste(subscale_labels_a, effect, level, model),
               paste(model_a_effects$outcome, model_a_effects$effect,
                     model_a_effects$level, model_a_effects$model))
  list(
    b = fmt_num(model_a_effects$estimate[idx], 2),
    ci = fmt_ci(model_a_effects$ci95_low[idx], model_a_effects$ci95_high[idx], 2),
    t = fmt_num(model_a_effects$t[idx], 2),
    df = as.character(round(model_a_effects$df[idx])),
    p = fmt_p(model_a_effects$p_fdr[idx])
  )
}

# Within-participant effect sizes are descriptive, so they do not vary between
# the adjusted and unadjusted models. "Overall" matches the pooled model
# contrast; the group-specific values accompany the estimated means.
dz_cell <- function(level) {
  idx <- match(paste(subscale_labels_a, "Cohen's dz (bespoke - general)", level),
               paste(model_a_effects$outcome, model_a_effects$effect,
                     model_a_effects$level))
  fmt_num(model_a_effects$estimate[idx], 2)
}

for (model_name in c("Unadjusted", "Adjusted")) {
  tool <- effect_cell("Tool type (overall)", "", model_name)
  group <- effect_cell("Respondent type (overall)", "", model_name)
  inter <- effect_cell("Tool type x respondent type", "", model_name)

  tab <- data.frame(
    Outcome = subscale_labels_a,
    tool_b = tool$b, tool_ci = tool$ci, tool_t = tool$t, tool_df = tool$df,
    tool_p = tool$p, tool_dz = dz_cell("Overall"),
    group_b = group$b, group_ci = group$ci, group_t = group$t,
    group_df = group$df, group_p = group$p,
    int_b = inter$b, int_t = inter$t, int_df = inter$df, int_p = inter$p,
    stringsAsFactors = FALSE
  )

  suffix <- tolower(model_name)
  write_apa_table(
    tab,
    file = file.path(TEX_DIR, paste0("model_a_", suffix, ".tex")),
    caption = sprintf(
      "%s mixed-effects estimates of AI tool type and respondent group on ATAMHS subscales",
      model_name
    ),
    label = paste0("tab:model-a-", suffix),
    align = "lrcrrrrrcrrrrrrr",
    size = "\\small",
    header = paste(
      "Outcome &",
      "\\multicolumn{6}{c}{Bespoke $-$ general} &",
      "\\multicolumn{5}{c}{Parent $-$ adult} &",
      "\\multicolumn{4}{c}{Interaction} \\\\",
      "\\cmidrule(lr){2-7} \\cmidrule(lr){8-12} \\cmidrule(lr){13-16}",
      " & $b$ & 95\\% CI & $t$ & $df$ & $p$ & $d_z$",
      "& $b$ & 95\\% CI & $t$ & $df$ & $p$",
      "& $b$ & $t$ & $df$ & $p$"
    ),
    # The model specification, degrees of freedom and multiplicity correction
    # are described in the Data Analysis section. The note carries only what a
    # reader cannot infer from the column headers or the text: the units, the
    # weighting used when averaging over factors, and the basis of $d_z$, which
    # is why it matches the pooled contrast rather than averaging the two
    # respondent groups.
    note = paste0(
      "$b$ is in scale points (1--7). Estimates average over respondent group ",
      "and gender using proportional weights. $d_z$ is the ",
      "within-participant standardised mean difference, computed on the ",
      "combined difference scores."
    )
  )
}

# ---- Estimated means table --------------------------------------------------

emm_cell <- function(rt, at) {
  idx <- match(paste(subscale_labels_a, rt, at, "Adjusted"),
               paste(model_a_emmeans$outcome, model_a_emmeans$respondent_type,
                     model_a_emmeans$ai_type, model_a_emmeans$model))
  sprintf("%s [%s, %s]",
          fmt_num(model_a_emmeans$emmean[idx], 2),
          fmt_num(model_a_emmeans$ci95_low[idx], 2),
          fmt_num(model_a_emmeans$ci95_high[idx], 2))
}

dz_cell <- function(rt) {
  idx <- match(paste(subscale_labels_a, "Cohen's dz (bespoke - general)", rt),
               paste(model_a_effects$outcome, model_a_effects$effect,
                     model_a_effects$level))
  fmt_num(model_a_effects$estimate[idx], 2)
}

write_apa_table(
  data.frame(
    Outcome = subscale_labels_a,
    ag = emm_cell("Adult", "General-purpose AI"),
    ab = emm_cell("Adult", "Bespoke AI"),
    adz = dz_cell("Adult"),
    pg = emm_cell("Parent", "General-purpose AI"),
    pb = emm_cell("Parent", "Bespoke AI"),
    pdz = dz_cell("Parent"),
    stringsAsFactors = FALSE
  ),
  file = file.path(TEX_DIR, "model_a_estimated_means.tex"),
  caption = "Estimated marginal means for ATAMHS subscales by AI tool type and respondent group",
  label = "tab:model-a-means",
  align = "lccrccr",
  size = "\\small",
  header = paste(
    "Outcome &",
    "\\multicolumn{3}{c}{Adults} & \\multicolumn{3}{c}{Parents/carers} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    " & General & Bespoke & $d_z$ & General & Bespoke & $d_z$"
  ),
  note = paste0(
    "Estimated marginal means [95\\% CI] from the adjusted models, on the ",
    "1--7 response scale. $d_z$ is the within-participant standardised mean ",
    "difference (bespoke $-$ general) computed from the paired differences."
  )
)

message("  fitted ", nrow(model_a_fits), " models across ",
        length(MODEL_A_OUTCOMES), " outcomes")
if (any(model_a_fits$singular, na.rm = TRUE)) {
  message("  NOTE: singular fit in: ",
          paste(unique(model_a_fits$outcome[which(model_a_fits$singular)]),
                collapse = ", "))
}
