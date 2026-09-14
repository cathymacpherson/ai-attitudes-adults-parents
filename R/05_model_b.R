# Model B: what predicts attitudes and behavioural intentions?
#
# Each perception predictor is measured once per AI tool type, so it carries two
# distinct pieces of information, entered separately:
#
#   _btw  the participant's mean across the two tools, grand-centred. Compares
#         different people: "someone who finds AI more useful than someone else
#         holds more positive attitudes." Open to person-level confounding.
#
#   _wth  the deviation from that participant's own mean. Compares one person
#         with themselves: "the tool this person found more useful is the tool
#         they were more positive about." Stable person characteristics cannot
#         produce this, which is the reason for using a mixed model rather than
#         four separate regressions.
#
# Steps are fitted with ML for the likelihood-ratio tests (a REML likelihood is
# not comparable across different fixed-effect structures) and refitted with
# REML for coefficient inference.
#
# ai_type is retained at every step. Its coefficient changes meaning as
# predictors enter: at step 1 it is the total bespoke-versus-general difference,
# and at step 3 it is the difference remaining once perceptions are accounted
# for. Both are reported, because the attenuation between them is the finding.

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

message("05_model_b")

BASE_TERMS <- c("ai_type", "respondent_type", ADJUSTMENT_COVARIATES)
CORE_TAM <- c("pu_btw", "pu_wth", "peou_btw", "peou_wth")
EXTENDED <- as.vector(rbind(
  paste0(c("trust", "privacy", "oversight", "environ"), "_btw"),
  paste0(c("trust", "privacy", "oversight", "environ"), "_wth")
))
# Whether the predictor weights differ by tool type. Restricted to the two
# theoretically central predictors rather than the full block, which this n
# will not support.
TOOL_INTERACTIONS <- c("ai_type:pu_wth", "ai_type:trust_wth")
ATTITUDE_TERMS <- c("att_btw", "att_wth")

# Each step adds one block to the ones before it. Steps are assembled from the
# blocks listed in MODEL_B_STEPS, so a block can be dropped without leaving its
# terms behind in later steps and without the step numbering going stale.
MODEL_B_BLOCKS <- list(
  S1 = list(label = "Tool type, group, demographics", terms = BASE_TERMS),
  S2 = list(label = "+ core TAM", terms = CORE_TAM),
  S3 = list(label = "+ trust, privacy, oversight, environment",
            terms = EXTENDED),
  S4 = list(label = "+ predictor $\\times$ tool type",
            terms = TOOL_INTERACTIONS),
  S5 = list(label = "+ attitudes towards AI", terms = ATTITUDE_TERMS,
            outcomes = "intent")
)

# S4 is omitted: the interaction block was non-significant for both outcomes and
# added no explanatory power, and the same reassurance is available from the
# similarity of the perception effects across tool types.
MODEL_B_STEPS <- c("S1", "S2", "S3", "S5")

step_definitions <- function(outcome) {
  keys <- intersect(MODEL_B_STEPS, names(MODEL_B_BLOCKS))
  keys <- Filter(function(key) {
    limit <- MODEL_B_BLOCKS[[key]]$outcomes
    is.null(limit) || outcome %in% limit
  }, keys)

  steps <- list()
  terms <- character(0)
  for (i in seq_along(keys)) {
    key <- keys[i]
    terms <- c(terms, MODEL_B_BLOCKS[[key]]$terms)
    steps[[key]] <- list(
      label = paste0(i, ". ", MODEL_B_BLOCKS[[key]]$label),
      terms = terms
    )
  }
  steps
}

# The step reported as the main-effects model: the full predictor set, without
# interactions and without attitude, so one table answers one question.
MAIN_STEP <- "S3"

model_formula <- function(outcome, terms) {
  stats::as.formula(paste(outcome, "~", paste(terms, collapse = " + "),
                          "+ (1 | participant_id)"))
}

# Every step must be fitted on identical rows, or the likelihood-ratio tests
# compare models on different data.
analysis_rows <- function(outcome, all_terms) {
  vars <- unique(c(outcome, "participant_id",
                   unlist(strsplit(all_terms, ":", fixed = TRUE))))
  vars <- vars[vars %in% names(long)]
  long[stats::complete.cases(long[, vars]), , drop = FALSE]
}

step_rows <- list()
coef_rows <- list()
tool_rows <- list()
vif_rows <- list()
attitude_path_rows <- list()

for (outcome in MODEL_B_OUTCOMES) {
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label
  steps <- step_definitions(outcome)
  dat <- analysis_rows(outcome, steps[[length(steps)]]$terms)

  message("  ", outcome_label, ": n = ", length(unique(dat$participant_id)),
          " participants, ", nrow(dat), " observations")

  ml_fits <- list()
  reml_fits <- list()

  for (step_key in names(steps)) {
    form <- model_formula(outcome, steps[[step_key]]$terms)
    ml <- fit_lmer(form, dat, reml = FALSE)
    reml <- fit_lmer(form, dat, reml = TRUE)
    ml_fits[[step_key]] <- ml$fit
    reml_fits[[step_key]] <- reml$fit

    r2 <- r2_mixed(reml$fit)
    previous <- if (step_key == "S1") NULL else ml_fits[[which(names(steps) == step_key) - 1L]]

    lrt <- list(chisq = NA_real_, df = NA_real_, p = NA_real_)
    if (!is.null(previous) && !is.null(ml$fit)) {
      comparison <- suppressMessages(stats::anova(previous, ml$fit))
      lrt$chisq <- comparison$Chisq[2]
      lrt$df <- comparison$Df[2]
      lrt$p <- comparison$`Pr(>Chisq)`[2]
    }

    delta_r2 <- if (step_key == "S1") NA_real_ else
      r2[["marginal"]] - step_rows[[length(step_rows)]]$r2_marginal

    step_rows[[length(step_rows) + 1L]] <- data.frame(
      outcome = outcome_label, outcome_key = outcome,
      step = step_key, step_label = steps[[step_key]]$label,
      n_terms = length(steps[[step_key]]$terms),
      r2_marginal = r2[["marginal"]], r2_conditional = r2[["conditional"]],
      delta_r2_marginal = delta_r2,
      aic = if (is.null(ml$fit)) NA_real_ else stats::AIC(ml$fit),
      lrt_chisq = lrt$chisq, lrt_df = lrt$df, lrt_p = lrt$p,
      singular = is_singular(reml$fit),
      messages = paste(unique(reml$messages), collapse = "; "),
      stringsAsFactors = FALSE
    )

    if (is.null(reml$fit)) next

    # Tool-type coefficient at this step: total effect at S1, direct effect once
    # perceptions are partialled out at S3 onwards.
    ct <- coefficient_table(reml$fit)
    tool_term <- ct[grepl("^ai_type", ct$term) & !grepl(":", ct$term), , drop = FALSE]
    if (nrow(tool_term)) {
      tool_rows[[length(tool_rows) + 1L]] <- data.frame(
        outcome = outcome_label, outcome_key = outcome,
        step = step_key, step_label = steps[[step_key]]$label,
        tool_term[, c("estimate", "std_error", "df", "t", "p",
                      "ci95_low", "ci95_high")],
        row.names = NULL, stringsAsFactors = FALSE
      )
    }
  }

  # Robustness check on the attenuation. The block order gives the established
  # TAM constructs all variance they share with the extended ones, which makes
  # the extension's increment conservative but leaves open whether the
  # tool-type effect would disappear under any set of predictors. This refits
  # the base model with the extended constructs but WITHOUT perceived
  # usefulness and ease of use: if the tool-type effect survives here and not
  # in Block 2, the attenuation is specific to the core constructs rather than
  # a general consequence of adding predictors.
  if (all(c("S2", "S3") %in% MODEL_B_STEPS)) {
    alt <- fit_lmer(model_formula(outcome, c(BASE_TERMS, EXTENDED)), dat)
    if (!is.null(alt$fit)) {
      ct <- coefficient_table(alt$fit)
      tool_term <- ct[grepl("^ai_type", ct$term) & !grepl(":", ct$term), ,
                      drop = FALSE]
      tool_rows[[length(tool_rows) + 1L]] <- data.frame(
        outcome = outcome_label, outcome_key = outcome,
        step = "Alt",
        step_label = "Extended constructs only, without core TAM",
        tool_term[, c("estimate", "std_error", "df", "t", "p",
                      "ci95_low", "ci95_high")],
        row.names = NULL, stringsAsFactors = FALSE
      )
    }
  }

  # Coefficients for the main-effects model.
  main_fit <- reml_fits[[MAIN_STEP]]
  if (!is.null(main_fit)) {
    coef_rows[[length(coef_rows) + 1L]] <- coefficient_table(
      main_fit, outcome = outcome_label, outcome_key = outcome, step = MAIN_STEP
    )
    v <- vif_fixed(main_fit)
    vif_rows[[length(vif_rows) + 1L]] <- data.frame(
      outcome = outcome_label, outcome_key = outcome, step = MAIN_STEP,
      term = names(v), vif = unname(v),
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  # Interaction and attitude steps are reported on their own, since adding them
  # changes the question rather than refining the same one.
  for (extra_step in intersect(c("S4", "S5"), names(steps))) {
    fit <- reml_fits[[extra_step]]
    if (is.null(fit)) next
    ct <- coefficient_table(fit, outcome = outcome_label,
                            outcome_key = outcome, step = extra_step)
    keep <- grepl(":", ct$term) | ct$term %in% ATTITUDE_TERMS
    if (any(keep)) {
      attitude_path_rows[[length(attitude_path_rows) + 1L]] <- ct[keep, , drop = FALSE]
    }
  }
}

model_b_steps <- do.call(rbind, step_rows)
model_b_coefficients <- do.call(rbind, coef_rows)
model_b_tool_effect <- do.call(rbind, tool_rows)
model_b_vif <- do.call(rbind, vif_rows)
model_b_extra_terms <- do.call(rbind, attitude_path_rows)

write_result_csv(model_b_steps, "05_model_b_steps")
write_result_csv(model_b_coefficients, "05_model_b_coefficients")
write_result_csv(model_b_tool_effect, "05_model_b_tool_effect_by_step")
write_result_csv(model_b_vif, "05_model_b_vif")
write_result_csv(model_b_extra_terms, "05_model_b_interaction_and_attitude_terms")

# ---- Table: hierarchical steps ----------------------------------------------

for (outcome in MODEL_B_OUTCOMES) {
  rows <- model_b_steps[model_b_steps$outcome_key == outcome, , drop = FALSE]
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label

  write_apa_table(
    data.frame(
      Step = rows$step_label,
      r2m = fmt_num(rows$r2_marginal, 3),
      dr2 = fmt_num(rows$delta_r2_marginal, 3),
      chisq = fmt_num(rows$lrt_chisq, 2),
      df = ifelse(is.na(rows$lrt_df), "--", as.character(rows$lrt_df)),
      p = fmt_p(rows$lrt_p),
      aic = fmt_num(rows$aic, 1),
      stringsAsFactors = FALSE
    ),
    file = file.path(TEX_DIR, paste0("model_b_steps_", outcome, ".tex")),
    caption = sprintf("Hierarchical mixed-effects models predicting %s",
                      decapitalise(outcome_label)),
    label = paste0("tab:model-b-steps-", outcome),
    align = "lrrrrrr",
    header = c("Block", "$R^2_m$", "$\\Delta R^2_m$", "$\\chi^2$", "$df$",
               "$p$", "AIC"),
    # Estimation and the block structure are described in the Data Analysis
    # section; retained here are the two symbol definitions and the fact that
    # the blocks share a sample, without which the increments are not
    # comparable.
    note = paste0(
      "$R^2_m$ is the marginal (fixed-effects) $R^2$; $\\Delta R^2_m$ is the ",
      "increment over the preceding block. All blocks are fitted on identical ",
      "cases."
    )
  )
}

# ---- Table: main-effects coefficients, between and within -------------------

PREDICTOR_LABELS <- c(
  pu = "Perceived usefulness", peou = "Perceived ease of use",
  trust = "Trust in AI", privacy = "Privacy concerns",
  oversight = "Human oversight preferences", environ = "Environmental concerns"
)

cell <- function(ct, term) {
  idx <- match(term, ct$term)
  if (is.na(idx)) return(c("--", "--", "--", "--", "--"))
  c(fmt_num(ct$estimate[idx], 2),
    fmt_ci(ct$ci95_low[idx], ct$ci95_high[idx], 2),
    fmt_num(ct$t[idx], 2),
    as.character(round(ct$df[idx])),
    fmt_p(ct$p[idx]))
}

for (outcome in MODEL_B_OUTCOMES) {
  ct <- model_b_coefficients[model_b_coefficients$outcome_key == outcome, , drop = FALSE]
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label

  rows <- list()
  for (key in names(PREDICTOR_LABELS)) {
    btw <- cell(ct, paste0(key, "_btw"))
    wth <- cell(ct, paste0(key, "_wth"))
    rows[[length(rows) + 1L]] <- data.frame(
      Predictor = PREDICTOR_LABELS[[key]],
      b_btw = btw[1], ci_btw = btw[2], t_btw = btw[3], df_btw = btw[4], p_btw = btw[5],
      b_wth = wth[1], ci_wth = wth[2], t_wth = wth[3], df_wth = wth[4], p_wth = wth[5],
      stringsAsFactors = FALSE
    )
  }
  predictors <- do.call(rbind, rows)

  # Terms that exist only once (not decomposed) occupy the between columns.
  other_terms <- ct$term[!grepl("_btw$|_wth$", ct$term) & ct$term != "(Intercept)"]
  other_labels <- c(
    "respondent_typeParent" = "Parent/carer (vs adult)",
    "age_c" = "Age (centred)",
    "gender_adjFemale" = "Female (vs male)",
    "gender_adjOther or undisclosed" = "Other or undisclosed gender (vs male)",
    "education_c" = "Education (centred)"
  )
  other_rows <- list()
  for (term in other_terms) {
    if (grepl("^ai_type", term)) next  # reported in the attenuation table
    value <- cell(ct, term)
    label <- if (term %in% names(other_labels)) other_labels[[term]] else term
    other_rows[[length(other_rows) + 1L]] <- data.frame(
      Predictor = label,
      b_btw = value[1], ci_btw = value[2], t_btw = value[3], df_btw = value[4], p_btw = value[5],
      b_wth = "--", ci_wth = "--", t_wth = "--", df_wth = "--", p_wth = "--",
      stringsAsFactors = FALSE
    )
  }

  tab <- rbind(predictors, do.call(rbind, other_rows))

  write_apa_table(
    tab,
    file = file.path(TEX_DIR, paste0("model_b_coefficients_", outcome, ".tex")),
    caption = sprintf(
      "Between- and within-person predictors of %s (step 3)",
      decapitalise(outcome_label)
    ),
    label = paste0("tab:model-b-coef-", outcome),
    align = "lrcrrrrcrrrr",
    size = "\\small",
    header = paste(
      "Predictor &",
      "\\multicolumn{5}{c}{Between-person} & \\multicolumn{5}{c}{Within-person} \\\\",
      "\\cmidrule(lr){2-6} \\cmidrule(lr){7-11}",
      " & $b$ & 95\\% CI & $t$ & $df$ & $p$ & $b$ & 95\\% CI & $t$ & $df$ & $p$"
    ),
    # The decomposition itself is explained in the Data Analysis section. What
    # remains here is the unit, why some rows have no within-person entry, and
    # where the tool-type coefficient went, since its absence from this table
    # would otherwise look like an omission.
    note = paste0(
      "$b$ is in scale points (1--7). Demographic covariates and respondent ",
      "group vary between participants only, so have no within-person ",
      "component. The tool-type coefficient is reported in ",
      "Table \\ref{tab:model-b-tool}."
    )
  )
}

# ---- Table: tool-type effect across steps (total to direct) -----------------

# The attenuation table shows the tool-type coefficient as perceptions enter, so
# it stops at the main model rather than continuing into any later block. The
# final row is the robustness check that reverses which constructs are omitted.
step_keys <- intersect(MODEL_B_STEPS, c("S1", "S2", "S3"))
if (all(c("S2", "S3") %in% MODEL_B_STEPS)) step_keys <- c(step_keys, "Alt")
tool_tab_rows <- list()
for (outcome in MODEL_B_OUTCOMES) {
  rows <- model_b_tool_effect[model_b_tool_effect$outcome_key == outcome, , drop = FALSE]
  idx <- match(step_keys, rows$step)
  tool_tab_rows[[length(tool_tab_rows) + 1L]] <- data.frame(
    Outcome = c(ANALYSIS_SCALES[[outcome]]$label,
                rep("", length(step_keys) - 1L)),
    Step = c(
      S1 = "Before perceptions (total effect)",
      S2 = "Adjusting for core TAM",
      S3 = "Adjusting for all perceptions (direct effect)",
      Alt = "Adjusting for extended constructs only"
    )[step_keys],
    b = fmt_num(rows$estimate[idx], 2),
    ci = fmt_ci(rows$ci95_low[idx], rows$ci95_high[idx], 2),
    t = fmt_num(rows$t[idx], 2),
    df = as.character(round(rows$df[idx])),
    p = fmt_p(rows$p[idx]),
    stringsAsFactors = FALSE
  )
}

write_apa_table(
  do.call(rbind, tool_tab_rows),
  file = file.path(TEX_DIR, "model_b_tool_effect.tex"),
  caption = "Bespoke versus general-purpose AI before and after adjusting for perceptions",
  label = "tab:model-b-tool",
  align = "llrcrrr",
  header = c("Outcome", "Model", "$b$", "95\\% CI", "$t$", "$df$", "$p$"),
  # What each row adjusts for is stated in its own label, and the attenuation
  # is interpreted in the text, so only the unit needs stating here.
  note = "$b$ is the bespoke-minus-general difference in scale points."
)

message("  step models: ", nrow(model_b_steps))
if (any(model_b_steps$singular, na.rm = TRUE)) {
  message("  NOTE: singular fit at ",
          paste(unique(model_b_steps$step[which(model_b_steps$singular)]),
                collapse = ", "))
}
max_vif <- max(model_b_vif$vif, na.rm = TRUE)
message("  maximum VIF in the step-3 models: ", fmt_num(max_vif, 1),
        " (", model_b_vif$term[which.max(model_b_vif$vif)], ")")
