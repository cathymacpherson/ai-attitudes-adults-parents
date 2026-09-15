# Model D: the direct bespoke-versus-general comfort item.
#
# "Compared with general-purpose AI tools (such as ChatGPT), how comfortable
# would you feel using AI tools specifically designed for mental health care?"
# rated 1 (much less comfortable) to 5 (much more comfortable).
#
# One response per participant, so this is a between-person model with no random
# effect. The item is ordinal with five categories and a clear floor of
# responses, so it is modelled with proportional-odds ordinal logistic
# regression (MASS::polr) rather than as a continuous outcome.
#
# This corroborates Model A using a different question format: Model A infers a
# preference from separately rated blocks, whereas this asks for the comparison
# directly.

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

message("07_model_d")

COMFORT_LABELS <- c("1 Much less comfortable", "2 Slightly less comfortable",
                    "3 About the same", "4 Slightly more comfortable",
                    "5 Much more comfortable")

comfort_data <- participants[!is.na(participants$comfort), , drop = FALSE]
comfort_data$comfort_f <- factor(comfort_data$comfort, levels = 1:5,
                                 labels = COMFORT_LABELS, ordered = TRUE)

# ---- Distribution by respondent group ---------------------------------------

dist_rows <- list()
for (rt in levels(participants$respondent_type)) {
  sub <- comfort_data[comfort_data$respondent_type == rt, , drop = FALSE]
  for (lv in COMFORT_LABELS) {
    n <- sum(sub$comfort_f == lv)
    dist_rows[[length(dist_rows) + 1L]] <- data.frame(
      respondent_type = rt, response = lv, n = n,
      denominator_n = nrow(sub),
      percent = 100 * n / nrow(sub),
      stringsAsFactors = FALSE
    )
  }
}
comfort_distribution <- do.call(rbind, dist_rows)
write_result_csv(comfort_distribution, "07_comfort_distribution")

pct_cell <- function(rt) {
  rows <- comfort_distribution[comfort_distribution$respondent_type == rt, , drop = FALSE]
  idx <- match(COMFORT_LABELS, rows$response)
  sprintf("%d (%s\\%%)", rows$n[idx], fmt_num(rows$percent[idx], 1))
}

# "Less/More comfortable" collapse categories 1-2 and 4-5 respectively, which
# are the summaries the result is usually stated as. Computed from the raw
# counts (not by summing the already-rounded per-category percentages), so
# they match the text exactly.
collapsed <- function(rt, labels) {
  rows <- comfort_distribution[comfort_distribution$respondent_type == rt, , drop = FALSE]
  n <- sum(rows$n[rows$response %in% labels])
  sprintf("%d (%s\\%%)", n, fmt_num(100 * n / rows$denominator_n[1], 1))
}
less_comfortable <- function(rt) collapsed(rt, COMFORT_LABELS[1:2])
more_comfortable <- function(rt) collapsed(rt, COMFORT_LABELS[4:5])

comfort_tab <- data.frame(
  Response = c(COMFORT_LABELS, "\\emph{Less comfortable (1 or 2)}",
              "\\emph{More comfortable (4 or 5)}"),
  Adults = c(pct_cell("Adult"), less_comfortable("Adult"), more_comfortable("Adult")),
  Parents = c(pct_cell("Parent"), less_comfortable("Parent"), more_comfortable("Parent")),
  stringsAsFactors = FALSE
)

write_apa_table(
  comfort_tab,
  file = file.path(TEX_DIR, "comfort_distribution.tex"),
  caption = paste0("Comfort using bespoke mental health AI compared with ",
                   "general-purpose AI tools"),
  label = "tab:comfort",
  align = "lrr",
  header = c("Response", "Adults", "Parents/carers"),
  # The denominator rule and the referent both change how the percentages are
  # read, and neither is visible from the column headers.
  note = paste0(
    "Cells are $n$ (\\%) of respondents answering the item; those who selected ",
    "\\emph{prefer not to say} are excluded. Parents and carers answered with ",
    "reference to their child."
  )
)

# ---- Proportional-odds model ------------------------------------------------

# The same adjustment set as Model A, so that the respondent-group contrast here
# is like-for-like with the one reported there. This analysis exists to
# corroborate that comparison using a different question format, which it can
# only do if both are adjusted the same way.
D_PREDICTORS <- c("respondent_type", ADJUSTMENT_COVARIATES)
d_vars <- c("comfort_f", D_PREDICTORS)
d_data <- comfort_data[stats::complete.cases(comfort_data[, d_vars]), , drop = FALSE]

d_formula <- stats::as.formula(paste("comfort_f ~",
                                     paste(D_PREDICTORS, collapse = " + ")))
polr_fit <- suppressWarnings(
  MASS::polr(d_formula, data = d_data, Hess = TRUE, method = "logistic")
)

sm <- summary(polr_fit)$coefficients
slopes <- rownames(sm)[!rownames(sm) %in% names(polr_fit$zeta)]
z <- sm[slopes, "Value"] / sm[slopes, "Std. Error"]

model_d_coefficients <- data.frame(
  term = slopes,
  estimate_logit = sm[slopes, "Value"],
  std_error = sm[slopes, "Std. Error"],
  odds_ratio = exp(sm[slopes, "Value"]),
  or_ci95_low = exp(sm[slopes, "Value"] - 1.96 * sm[slopes, "Std. Error"]),
  or_ci95_high = exp(sm[slopes, "Value"] + 1.96 * sm[slopes, "Std. Error"]),
  z = z,
  p = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
  row.names = NULL, stringsAsFactors = FALSE
)
write_result_csv(model_d_coefficients, "07_model_d_coefficients")

D_TERM_LABELS <- c(
  "respondent_typeParent" = "Parent/carer (vs adult)",
  "age_c" = "Age (centred)",
  "gender_adjFemale" = "Female (vs male)",
  "education_c" = "Education (centred)",
  "genai_freq_c" = "GenAI use frequency (centred)",
  "tech_conf_c" = "Digital technology confidence (centred)"
)

d_tab <- data.frame(
  Predictor = ifelse(model_d_coefficients$term %in% names(D_TERM_LABELS),
                     D_TERM_LABELS[model_d_coefficients$term],
                     model_d_coefficients$term),
  or = fmt_num(model_d_coefficients$odds_ratio, 2),
  ci = fmt_ci(model_d_coefficients$or_ci95_low,
              model_d_coefficients$or_ci95_high, 2),
  z = fmt_num(model_d_coefficients$z, 2),
  p = fmt_p(model_d_coefficients$p),
  stringsAsFactors = FALSE
)

write_apa_table(
  d_tab,
  file = file.path(TEX_DIR, "model_d_comfort.tex"),
  caption = paste0("Proportional-odds model of comfort using bespoke rather ",
                   "than general-purpose AI for mental health"),
  label = "tab:model-d",
  align = "lrcrr",
  header = c("Predictor", "$OR$", "95\\% CI", "$z$", "$p$"),
  # The model and its covariates are described in the text. Retained here: the
  # sample size, which differs from the other models; the direction of the odds
  # ratio, without which the sign cannot be read; and the interval method.
  note = paste0(
    "$n = ", nrow(d_data), "$. An odds ratio above 1 indicates greater relative ",
    "comfort with bespoke tools. Confidence intervals and $p$ values use the ",
    "Wald approximation."
  )
)

# ---- Convergent validity against the Model A attitude difference ------------
# If the direct comparison item measures what the separately rated blocks
# measure, respondents who rate bespoke tools more favourably should also report
# greater comfort with them.

att_wide <- long[, c("participant_id", "ai_type", "att")]
gen <- att_wide[att_wide$ai_type == "General-purpose AI", c("participant_id", "att")]
besp <- att_wide[att_wide$ai_type == "Bespoke AI", c("participant_id", "att")]
paired <- merge(gen, besp, by = "participant_id", suffixes = c("_gen", "_besp"))
paired$att_difference <- paired$att_besp - paired$att_gen
paired <- merge(paired, comfort_data[, c("participant_id", "comfort")],
                by = "participant_id")
paired <- paired[stats::complete.cases(paired[, c("att_difference", "comfort")]), ]

validity <- stats::cor.test(paired$att_difference, paired$comfort,
                            method = "spearman", exact = FALSE)
model_d_validity <- data.frame(
  comparison = "Attitude difference (bespoke - general) vs comfort item",
  n = nrow(paired),
  spearman_rho = unname(validity$estimate),
  p = validity$p.value,
  stringsAsFactors = FALSE
)
write_result_csv(model_d_validity, "07_model_d_convergent_validity")

message("  comfort item: n = ", nrow(d_data), " in the model")
message("  parent vs adult OR = ",
        fmt_num(model_d_coefficients$odds_ratio[
          model_d_coefficients$term == "respondent_typeParent"], 2),
        ", p = ", fmt_p_plain(model_d_coefficients$p[
          model_d_coefficients$term == "respondent_typeParent"]))
message("  convergent validity with the attitude difference: rho = ",
        fmt_num(model_d_validity$spearman_rho, 2),
        ", p = ", fmt_p_plain(model_d_validity$p), " (n = ", model_d_validity$n, ")")
