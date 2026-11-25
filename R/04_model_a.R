# Model A: how are the two AI tool types perceived?
#
#   outcome ~ ai_type * respondent_type + age_c + gender_adj + education_c
#             + (1 | participant_id)


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
  besp <- d[d$ai_type == "Purpose-built AI", c("participant_id", outcome)]
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

  for (model_name in c("Adjusted")) {
    rhs <- ADJUSTED_RHS
    formula <- stats::as.formula(paste(outcome, "~", rhs))

    # Complete cases on the model's own variables, so the reported n matches
    # what was actually fitted.
    model_vars <- c(outcome, "ai_type", "respondent_type", "participant_id",
                    ADJUSTMENT_COVARIATES)
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

    # Purpose-built minus general, overall and within each respondent group.
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

    # Estimated means by respondent group.
    em <- as.data.frame(emm_tool_by_group)
    emmean_rows[[length(emmean_rows) + 1L]] <- data.frame(
      outcome = label, outcome_key = outcome, model = model_name,
      ai_type = as.character(em$ai_type),
      respondent_type = as.character(em$respondent_type),
      emmean = em$emmean, std_error = em$SE, df = em$df,
      ci95_low = em$lower.CL, ci95_high = em$upper.CL,
      row.names = NULL, stringsAsFactors = FALSE
    )

    # Pooled over respondent group, recorded as respondent_type "Overall". The
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
  # model contrast; it is not the average of the two
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
      effect = "Cohen's dz (purpose-built - general)", level = spec$label,
      contrast = "Purpose-built AI - General-purpose AI",
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

# ---- Table: tool-type and respondent-group effects --------------------------

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

# Within-participant effect sizes are descriptive. "Overall" matches the
# pooled model contrast; the group-specific values accompany the estimated
# means.
dz_cell <- function(level) {
  idx <- match(paste(subscale_labels_a, "Cohen's dz (purpose-built - general)", level),
               paste(model_a_effects$outcome, model_a_effects$effect,
                     model_a_effects$level))
  fmt_num(model_a_effects$estimate[idx], 2)
}

# $d_z$ only applies to the tool-type contrast, so it is blank ("--") for the
# other two effect blocks rather than recomputed for them.
effect_block <- function(effect, group_label, dz = NULL) {
  cells <- effect_cell(effect, "")
  data.frame(
    Outcome = c(group_label, paste0("\\quad ", subscale_labels_a)),
    b = c("", cells$b), ci = c("", cells$ci), t = c("", cells$t),
    df = c("", cells$df), p = c("", cells$p),
    dz = c("", if (is.null(dz)) rep("--", length(subscale_labels_a)) else dz),
    stringsAsFactors = FALSE
  )
}

write_apa_table(
  rbind(
    effect_block("Tool type (overall)", "Purpose-built $-$ general", dz = dz_cell("Overall")),
    effect_block("Respondent type (overall)", "Parent $-$ adult"),
    effect_block("Tool type x respondent type", "Interaction")
  ),
  file = file.path(TEX_DIR, "model_a_adjusted.tex"),
  caption = "Mixed-effects estimates of AI tool type and respondent group on ATAMHS subscales",
  label = "tab:model-a-adjusted",
  align = "lrcrrrr",
  size = "\\small",
  header = c("Outcome", "$b$", "95\\% CI", "$t$", "$df$", "$p$", "$d_z$"),
  note = paste0(
    "$b$ is in scale points (1--7). Estimates average over respondent group ",
    "and gender using proportional weights. $d_z$, the within-participant ",
    "standardised mean difference computed on the combined difference ",
    "scores, applies only to the tool-type contrast."
  )
)

# ---- Table: estimated means -------------------------------------------------

emm_cell <- function(rt, at) {
  idx <- match(paste(subscale_labels_a, rt, at, "Adjusted"),
               paste(model_a_emmeans$outcome, model_a_emmeans$respondent_type,
                     model_a_emmeans$ai_type, model_a_emmeans$model))
  sprintf("%s [%s, %s]",
          fmt_num(model_a_emmeans$emmean[idx], 2),
          fmt_num(model_a_emmeans$ci95_low[idx], 2),
          fmt_num(model_a_emmeans$ci95_high[idx], 2))
}

means_block <- function(rt, group_label) {
  data.frame(
    Outcome = c(group_label, paste0("\\quad ", subscale_labels_a)),
    general = c("", emm_cell(rt, "General-purpose AI")),
    purpose_built = c("", emm_cell(rt, "Purpose-built AI")),
    dz = c("", dz_cell(rt)),
    stringsAsFactors = FALSE
  )
}

write_apa_table(
  rbind(means_block("Adult", "Adults"), means_block("Parent", "Parents/carers")),
  file = file.path(TEX_DIR, "model_a_estimated_means.tex"),
  caption = "Estimated marginal means for ATAMHS subscales by AI tool type and respondent group",
  label = "tab:model-a-means",
  align = "lccc",
  size = "\\small",
  header = c("Outcome", "General", "Purpose-built", "$d_z$"),
  note = paste0(
    "Estimated marginal means [95\\% CI] from the adjusted models, on the ",
    "1--7 response scale. $d_z$ is the within-participant standardised mean ",
    "difference (purpose-built $-$ general) computed from the paired differences."
  )
)

message("  fitted ", nrow(model_a_fits), " models across ",
        length(MODEL_A_OUTCOMES), " outcomes")
if (any(model_a_fits$singular, na.rm = TRUE)) {
  message("  NOTE: singular fit in: ",
          paste(unique(model_a_fits$outcome[which(model_a_fits$singular)]),
                collapse = ", "))
}
