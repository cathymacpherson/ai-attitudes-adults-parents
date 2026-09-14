# Sample characteristics and subscale descriptives.

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

# ---- Demographics -----------------------------------------------------------

# Each demographic characteristic is described by a function of a participant
# subset returning a single formatted cell, so the adult and parent columns are
# built from exactly the same definitions.
count_pct <- function(x, total) {
  sprintf("%d (%s\\%%)", sum(x, na.rm = TRUE),
          formatC(100 * sum(x, na.rm = TRUE) / total, format = "f", digits = 1))
}

# mean_sd is shared (r/R/utils.R): identical in both papers' own
# 03_descriptives.R, so it moved there rather than staying duplicated.

DEMOGRAPHIC_SPEC <- c(
  list(
    list(label = "$n$", fn = function(p) as.character(nrow(p))),
    list(label = "Age, $M$ ($SD$)", fn = function(p) mean_sd(p$age, 1)),
    list(label = "\\quad Range", fn = function(p) {
      sprintf("%s--%s", fmt_num(min(p$age, na.rm = TRUE), 0),
              fmt_num(max(p$age, na.rm = TRUE), 0))
    })
  ),
  lapply(GENDER_LEVELS, function(lv) {
    list(label = paste0("\\quad ", lv),
         fn = function(p) count_pct(p$gender == lv, nrow(p)),
         group = "Gender, $n$ (\\%)")
  }),
  lapply(levels(participants$cultural_group), function(lv) {
    list(label = paste0("\\quad ", lv),
         fn = function(p) count_pct(p$cultural_group == lv, nrow(p)),
         group = "Cultural background, $n$ (\\%)")
  }),
  lapply(names(EDUCATION_SCORES), function(lv) {
    score <- EDUCATION_SCORES[[lv]]
    list(label = paste0("\\quad ", lv),
         fn = function(p) count_pct(p$education == score, nrow(p)),
         group = "Education, $n$ (\\%)")
  }),
  list(
    # Not part of EDUCATION_SCORES (see 01_load_prepare.R): tracked via the raw
    # string so these respondents show up as a category instead of vanishing
    # into education's NAs alongside genuinely missing responses.
    list(label = "\\quad Prefer not to say",
         fn = function(p) count_pct(p$education_raw == "Prefer not to say", nrow(p)),
         group = "Education, $n$ (\\%)")
  ),
  lapply(levels(participants$income), function(lv) {
    # Income brackets contain literal "$", which must be escaped for LaTeX;
    # this table is not written with escape = TRUE (that would also mangle
    # the "\quad" indentation already baked into every label here).
    list(label = paste0("\\quad ", gsub("$", "\\$", lv, fixed = TRUE)),
         fn = function(p) count_pct(p$income == lv, nrow(p)),
         group = "Household income (annual), $n$ (\\%)")
  }),
  list(
    list(label = "GenAI use frequency, $M$ ($SD$)",
         fn = function(p) mean_sd(p$genai_freq, 2)),
    list(label = "Digital technology confidence, $M$ ($SD$)",
         fn = function(p) mean_sd(p$tech_conf, 2)),
    # The referent differs by group: adults were asked about their own mental
    # health, parents/carers about their child's. Neither group was asked the
    # other version, so the two columns are not directly comparable.
    list(label = "Used GenAI for mental health\\textsuperscript{a}, $n$ (\\%)",
         fn = function(p) count_pct(p$used_ai_mh, nrow(p)))
  )
)

adults <- participants[participants$respondent_type == "Adult", , drop = FALSE]
parents <- participants[participants$respondent_type == "Parent", , drop = FALSE]

demo_rows <- list()
last_group <- ""
for (spec in DEMOGRAPHIC_SPEC) {
  group <- if (is.null(spec$group)) "" else spec$group
  if (nzchar(group) && !identical(group, last_group)) {
    demo_rows[[length(demo_rows) + 1L]] <- data.frame(
      Characteristic = group, Adults = "", Parents = "", stringsAsFactors = FALSE
    )
    last_group <- group
  }
  demo_rows[[length(demo_rows) + 1L]] <- data.frame(
    Characteristic = spec$label,
    Adults = spec$fn(adults),
    Parents = spec$fn(parents),
    stringsAsFactors = FALSE
  )
}
demographics <- do.call(rbind, demo_rows)

# Education/income levels absent from both samples add nothing to the table.
empty <- grepl("^\\\\quad ", demographics$Characteristic) &
  grepl("^0 \\(0\\.0", demographics$Adults) & grepl("^0 \\(0\\.0", demographics$Parents)
demographics <- demographics[!empty, , drop = FALSE]

write_result_csv(demographics, "03_demographics")
write_apa_table(
  demographics,
  file = file.path(TEX_DIR, "demographics.tex"),
  caption = "Sample characteristics by respondent group",
  label = "tab:demographics",
  align = "lrr",
  header = c("Characteristic", "Adults", "Parents/carers"),
  note = paste0(
    "GenAI use frequency ranged from 0 (has not used) to 6 (several times per ",
    "day); digital technology confidence ranged from 1 (not at all confident) ",
    "to 5 (extremely confident). Household income is the respondent's reported ",
    "annual household income bracket. \\textsuperscript{a}Adults were asked ",
    "whether they had used a generative AI tool for questions about their own ",
    "mental health, parents/carers whether they had done so for questions ",
    "about their child's mental health."
  )
)

# ---- Child characteristics (parent/carer sample) ----------------------------
# A separate table, because these describe the children reported on rather than
# the respondents, and apply to only one of the two samples.

child_block <- function(variable, group_label) {
  levels_present <- levels(droplevels(parents[[variable]]))
  lapply(levels_present, function(lv) {
    list(label = paste0("\\quad ", lv),
         fn = function(p) count_pct(p[[variable]] == lv, sum(!is.na(p[[variable]]))),
         group = group_label)
  })
}

CHILD_SPEC <- c(
  list(
    list(label = "Age, $M$ ($SD$)", fn = function(p) mean_sd(p$child_age, 1)),
    list(label = "\\quad Range", fn = function(p) {
      sprintf("%s--%s", fmt_num(min(p$child_age, na.rm = TRUE), 0),
              fmt_num(max(p$child_age, na.rm = TRUE), 0))
    })
  ),
  # Age bands are omitted because the analyses collapse across them, and school
  # grade because it carries essentially the same information as age.
  child_block("child_gender", "Gender, $n$ (\\%)"),
  child_block("child_relation", "Respondent's relationship to child, $n$ (\\%)"),
  child_block("child_family_setting", "Family setting, $n$ (\\%)")
)

child_rows <- list()
last_group <- ""
for (spec in CHILD_SPEC) {
  group <- if (is.null(spec$group)) "" else spec$group
  if (nzchar(group) && !identical(group, last_group)) {
    child_rows[[length(child_rows) + 1L]] <- data.frame(
      Characteristic = group, Value = "", stringsAsFactors = FALSE
    )
    last_group <- group
  }
  child_rows[[length(child_rows) + 1L]] <- data.frame(
    Characteristic = spec$label, Value = spec$fn(parents),
    stringsAsFactors = FALSE
  )
}
child_demographics <- do.call(rbind, child_rows)

# Categories nobody fell into add nothing to the table. This matters most for
# the out-of-range age bands, which are empty once ineligible respondents are
# excluded, but applies to any empty category.
child_empty <- grepl("^\\\\quad ", child_demographics$Characteristic) &
  grepl("^0 \\(0\\.0", child_demographics$Value)
child_demographics <- child_demographics[!child_empty, , drop = FALSE]

write_result_csv(child_demographics, "03_child_demographics")
write_apa_table(
  child_demographics,
  file = file.path(TEX_DIR, "child_demographics.tex"),
  caption = "Characteristics of the children reported on by parents and carers",
  label = "tab:child-demographics",
  align = "lr",
  header = c("Characteristic", "Children")
  # No note: the sampling frame and item wording are described in the text.
)

# ---- Subscale descriptives --------------------------------------------------

# Descriptives follow the scales AS ANALYSED, plus any items reported on their
# own, so this table sits consistently alongside the model tables.
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
  adult_besp = msd_cell("Adult", "Bespoke AI"),
  parent_gen = msd_cell("Parent", "General-purpose AI"),
  parent_besp = msd_cell("Parent", "Bespoke AI"),
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
    " & General & Bespoke & General & Bespoke"
  ),
  note = paste0(
    "Cells are $M$ ($SD$); all subscales are scored 1--7. Privacy concern, human ",
    "oversight preferences and environmental concerns are scored in their natural ",
    "direction, so higher values indicate greater concern or a stronger preference ",
    "for human oversight."
  )
)

# ---- Comparison against the scale midpoint ----------------------------------
# Are attitudes and intentions favourable at all, irrespective of tool type?
# Each outcome is centred on the scale midpoint (4, on the 1--7 ATAMHS scale)
# and fit as an intercept-only mixed model with a by-participant random
# intercept, so the two ratings per participant (general-purpose, bespoke) are
# not treated as independent. The intercept of the centred outcome is then
# exactly the test of the mean against the midpoint, with the same
# Satterthwaite df used throughout the rest of the paper.

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
# Short labels shared with the reliability table (TABLE_VAR_LABELS in
# 00_config.R), so both tables present the same eight variables the same way.
correlation_labels <- unname(TABLE_VAR_LABELS[CORRELATION_VARS])

for (at in levels(long$ai_type)) {
  suffix <- if (at == "Bespoke AI") "bespoke" else "general"
  block <- long[long$ai_type == at, CORRELATION_VARS]
  R <- stats::cor(block, use = "pairwise.complete.obs")
  write_result_csv(
    data.frame(variable = rownames(R), R, check.names = FALSE),
    paste0("03_correlations_", suffix)
  )

  # Lower triangle only, each pair shown once. Cells carry significance stars
  # from pairwise cor.test, since a table this size is otherwise unreadable
  # without some way to tell a trivial r from a meaningful one at a glance.
  # Benjamini-Hochberg across all pairs in the table: 28 simultaneous tests is
  # the same kind of family already corrected for in Models A and C, and these
  # values are now cited directly in the text rather than shown only for
  # descriptive context.
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
    cells[pair_i[k], pair_j[k]] <- paste0(fmt_num(pair_r[k], 2), star(pair_p_fdr[k]))
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
      if (at == "Bespoke AI") "bespoke AI" else "general-purpose AI"
    ),
    label = paste0("tab:correlations-", suffix),
    align = paste0("l", strrep("r", n_vars - 1)),
    header = c("Variable", as.character(seq_len(n_vars - 1))),
    escape = TRUE,
    # \small keeps 8 columns of short row labels (e.g. "Oversight
    # preferences") inside the APA portrait text width, matching the other
    # manuscript tables -- landscape isn't needed once the labels are short.
    # tabcolsep trims the default 6pt gap either side of each of the 8
    # columns down to 3pt, saving about 0.4in of width with no change to
    # font size or content.
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
