# Subscale descriptives, midpoint comparisons and correlations. The sample-
# characteristics tables are produced separately, because they use variables
# that are not in the released dataset.

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

message("03_descriptives")

# ---- Subscale descriptives --------------------------------------------------

# Descriptives follow the scales as analysed, plus any items reported on their
# own.
descriptive_specs <- c(
  lapply(ANALYSIS_SCALES, function(s) list(label = s$label)),
  lapply(STANDALONE_ITEMS, function(s) list(label = s$label))
)

desc_rows <- list()
for (key in names(descriptive_specs)) {
  for (rt in levels(long$respondent_type)) {
    for (at in levels(long$ai_type)) {
      v <- long[[key]][long$respondent_type == rt & long$ai_type == at]
      desc_rows[[length(desc_rows) + 1L]] <- data.frame(
        subscale = descriptive_specs[[key]]$label,
        subscale_key = key,
        respondent_type = rt,
        ai_type = at,
        n = sum(!is.na(v)),
        mean = mean(v, na.rm = TRUE),
        sd = stats::sd(v, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
}
subscale_descriptives <- do.call(rbind, desc_rows)
write_result_csv(subscale_descriptives, "03_subscale_descriptives")

subscale_labels <- unname(vapply(descriptive_specs, function(s) s$label,
                                 character(1)))

msd_cell <- function(rt, at) {
  idx <- match(paste(subscale_labels, rt, at),
               paste(subscale_descriptives$subscale,
                     subscale_descriptives$respondent_type,
                     subscale_descriptives$ai_type))
  sprintf("%s (%s)", fmt_num(subscale_descriptives$mean[idx], 2),
          fmt_num(subscale_descriptives$sd[idx], 2))
}

desc_tab <- data.frame(
  Subscale = subscale_labels,
  adult_gen = msd_cell("Adult", "General-purpose AI"),
  adult_besp = msd_cell("Adult", "Purpose-built AI"),
  parent_gen = msd_cell("Parent", "General-purpose AI"),
  parent_besp = msd_cell("Parent", "Purpose-built AI"),
  stringsAsFactors = FALSE
)

write_apa_table(
  desc_tab,
  file = file.path(TEX_DIR, "subscale_descriptives.tex"),
  caption = "ATAMHS subscale means and standard deviations by AI tool type and respondent group",
  label = "tab:subscale-descriptives",
  align = "lcccc",
  header = paste(
    "Subscale &",
    "\\multicolumn{2}{c}{Adults} & \\multicolumn{2}{c}{Parents/carers} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    " & General & Purpose-built & General & Purpose-built"
  ),
  note = "Cells are $M$ ($SD$); all subscales are scored 1--7."
)

# ---- Comparison against the scale midpoint ----------------------------------
# Are attitudes and intentions favourable at all, irrespective of tool type?
# Each outcome is centred on the scale midpoint (4, on the 1--7 ATAMHS scale)
# and fit as an intercept-only mixed model with a by-participant random
# intercept, so the two ratings per participant (general-purpose, purpose-built) are
# not treated as independent. The intercept of the centred outcome is then
# exactly the test of the mean against the midpoint, with the same
# Satterthwaite df used in the other models.

SCALE_MIDPOINT <- 4

midpoint_rows <- list()
for (outcome in MODEL_B_OUTCOMES) {
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label
  dat <- long[!is.na(long[[outcome]]), c(outcome, "participant_id"), drop = FALSE]
  dat$centred <- dat[[outcome]] - SCALE_MIDPOINT

  res <- fit_lmer(stats::as.formula("centred ~ 1 + (1 | participant_id)"), dat)
  fit <- res$fit
  if (is.null(fit)) next

  ct <- coefficient_table(fit)
  sd_total <- sqrt(random_intercept_var(fit) + residual_var(fit))

  midpoint_rows[[length(midpoint_rows) + 1L]] <- data.frame(
    outcome = outcome_label, outcome_key = outcome,
    n_participants = length(unique(dat$participant_id)),
    n_observations = nrow(dat),
    mean = ct$estimate + SCALE_MIDPOINT,
    ci95_low = ct$ci95_low + SCALE_MIDPOINT,
    ci95_high = ct$ci95_high + SCALE_MIDPOINT,
    t = ct$t, df = ct$df, p = ct$p,
    d = ct$estimate / sd_total,
    stringsAsFactors = FALSE
  )
}
midpoint_comparison <- do.call(rbind, midpoint_rows)
write_result_csv(midpoint_comparison, "03_midpoint_comparison")

write_apa_table(
  data.frame(
    Outcome = midpoint_comparison$outcome,
    n = midpoint_comparison$n_participants,
    M = fmt_num(midpoint_comparison$mean, 2),
    ci = fmt_ci(midpoint_comparison$ci95_low, midpoint_comparison$ci95_high, 2),
    t = fmt_num(midpoint_comparison$t, 2),
    df = as.character(round(midpoint_comparison$df)),
    p = fmt_p(midpoint_comparison$p),
    d = fmt_num(midpoint_comparison$d, 2),
    stringsAsFactors = FALSE
  ),
  file = file.path(TEX_DIR, "midpoint_comparison.tex"),
  caption = paste0(
    "Attitudes and intentions relative to the scale midpoint, irrespective ",
    "of AI tool type"
  ),
  label = "tab:midpoint-comparison",
  align = "lrrcrrrr",
  header = c("Outcome", "$n$", "$M$", "95\\% CI", "$t$", "$df$", "$p$", "$d$"),
  note = paste0(
    "$M$ and 95\\% CI are estimated from an intercept-only mixed-effects model ",
    "with a by-participant random intercept, on the 1--7 scale (midpoint = 4), ",
    "pooling ratings of both AI tool types. $d$ is the standardised mean ",
    "difference from the midpoint, using the model's total (random-intercept ",
    "plus residual) variance."
  )
)

message("  midpoint comparison: ", paste(
  sprintf("%s M=%s d=%s", midpoint_comparison$outcome,
          fmt_num(midpoint_comparison$mean, 2), fmt_num(midpoint_comparison$d, 2)),
  collapse = "; "
))

# ---- Correlations among the Model B variables -------------------------------

CORRELATION_VARS <- TABLE_VAR_ORDER

correlation_labels <- unname(TABLE_VAR_LABELS[CORRELATION_VARS])

for (at in levels(long$ai_type)) {
  suffix <- if (at == "Purpose-built AI") "purpose_built" else "general"
  block <- long[long$ai_type == at, CORRELATION_VARS]
  R <- stats::cor(block, use = "pairwise.complete.obs")
  write_result_csv(
    data.frame(variable = rownames(R), R, check.names = FALSE),
    paste0("03_correlations_", suffix)
  )

  # Benjamini-Hochberg across all pairs in the table: 28 simultaneous tests is
  # the same kind of family corrected for in Models A and C.
  n_vars <- length(CORRELATION_VARS)
  pair_i <- integer(0); pair_j <- integer(0); pair_r <- numeric(0); pair_p <- numeric(0)
  for (i in seq_len(n_vars)) {
    for (j in seq_len(n_vars)) {
      if (j >= i) next
      test <- stats::cor.test(block[[i]], block[[j]])
      pair_i <- c(pair_i, i); pair_j <- c(pair_j, j)
      pair_r <- c(pair_r, unname(test$estimate)); pair_p <- c(pair_p, test$p.value)
    }
  }
  pair_p_fdr <- stats::p.adjust(pair_p, method = P_ADJUST_METHOD)

  cells <- matrix(NA_character_, n_vars, n_vars)
  for (k in seq_along(pair_i)) {
    cells[pair_i[k], pair_j[k]] <- paste0(fmt_r(pair_r[k], 2), star(pair_p_fdr[k]))
  }
  tab <- data.frame(
    Variable = sprintf("%d. %s", seq_len(n_vars), correlation_labels),
    cells[, -n_vars, drop = FALSE],
    stringsAsFactors = FALSE
  )
  names(tab)[-1] <- as.character(seq_len(n_vars - 1))

  write_apa_table(
    tab,
    file = file.path(TEX_DIR, paste0("correlations_", suffix, ".tex")),
    caption = sprintf(
      "Correlations among ATAMHS subscales (%s)",
      if (at == "Purpose-built AI") "purpose-built AI" else "general-purpose AI"
    ),
    label = paste0("tab:correlations-", gsub("_", "-", suffix, fixed = TRUE)),
    align = paste0("l", strrep("r", n_vars - 1)),
    header = c("Variable", as.character(seq_len(n_vars - 1))),
    escape = TRUE,
    size = "\\small",
    tabcolsep = "3pt",
    note = paste0(
      "Column numbers correspond to the numbered rows. $p$ values are ",
      "Benjamini-Hochberg adjusted across all pairs in the table. ",
      "$^*p < .05$, $^{**}p < .01$, $^{***}p < .001$."
    )
  )
}

message("  wrote demographics, subscale descriptives and correlation matrices")
