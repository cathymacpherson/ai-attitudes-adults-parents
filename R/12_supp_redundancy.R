# Supplementary analysis: does trust or perceived usefulness account for
# environmental concern's and oversight preference's null association with
# attitudes/intentions in the Model B full (Block 3) models?
#
# Both constructs correlate moderately with trust and perceived usefulness
# (Tables 4-5 in the manuscript), so their null Block 3 coefficients could
# reflect either a genuine absence of association or variance shared with
# those two stronger predictors. This is tested directly: for each construct,
# the same base model is fit three times -- the construct alone, the
# construct plus trust, and the construct plus perceived usefulness -- and
# the construct's own coefficient is compared across the three. If adding a
# single other predictor drives the construct's coefficient to
# non-significance, its Block 3 null is attributable to overlap with that
# predictor rather than to a genuine absence of association.
#
# Requires 05_model_b.R to have been sourced first, for BASE_TERMS,
# model_formula() and analysis_rows().

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

if (!exists("model_b_coefficients", envir = globalenv())) {
  source(file.path(R_DIR, "05_model_b.R"), local = globalenv())
}

message("12_supp_redundancy")

REDUNDANCY_TARGETS <- c(environ = "Environmental concerns",
                        oversight = "Human oversight preferences")
REDUNDANCY_ABSORBERS <- c(none = "Alone (no absorber)",
                          trust = "+ Trust", pu = "+ Perceived usefulness")

redundancy_rows <- list()

for (outcome in MODEL_B_OUTCOMES) {
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label

  for (target in names(REDUNDANCY_TARGETS)) {
    target_terms <- paste0(target, c("_btw", "_wth"))

    # One shared dataset across all three fits for this target/outcome, so the
    # three models are compared on identical rows and any change in the
    # target's coefficient reflects the added predictor, not a sample shift.
    all_terms <- c(BASE_TERMS, target_terms,
                   paste0("trust", c("_btw", "_wth")),
                   paste0("pu", c("_btw", "_wth")))
    dat <- analysis_rows(outcome, all_terms)

    for (absorber in names(REDUNDANCY_ABSORBERS)) {
      terms <- c(BASE_TERMS, target_terms)
      if (absorber != "none") {
        terms <- c(terms, paste0(absorber, c("_btw", "_wth")))
      }
      res <- fit_lmer(model_formula(outcome, terms), dat)
      if (is.null(res$fit)) next

      ct <- coefficient_table(res$fit)
      target_ct <- ct[ct$term %in% target_terms, , drop = FALSE]

      redundancy_rows[[length(redundancy_rows) + 1L]] <- data.frame(
        outcome = outcome_label, outcome_key = outcome,
        target = REDUNDANCY_TARGETS[[target]], target_key = target,
        absorber = REDUNDANCY_ABSORBERS[[absorber]], absorber_key = absorber,
        component = ifelse(grepl("_btw$", target_ct$term), "Between", "Within"),
        estimate = target_ct$estimate, std_error = target_ct$std_error,
        df = target_ct$df, t = target_ct$t, p = target_ct$p,
        ci95_low = target_ct$ci95_low, ci95_high = target_ct$ci95_high,
        stringsAsFactors = FALSE
      )
    }
  }
}

supp_redundancy <- do.call(rbind, redundancy_rows)
supp_redundancy$sig <- supp_redundancy$p < ALPHA
write_result_csv(supp_redundancy, "12_supp_redundancy")

# ---- Table -------------------------------------------------------------

cell <- function(rows, target_label, component, absorber_label) {
  idx <- which(rows$target == target_label & rows$component == component &
               rows$absorber == absorber_label)
  if (!length(idx)) return(c("--", "--", "--"))
  c(fmt_num(rows$estimate[idx], 2),
    fmt_p(rows$p[idx]),
    fmt_ci(rows$ci95_low[idx], rows$ci95_high[idx], 2))
}

for (outcome in MODEL_B_OUTCOMES) {
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label
  rows <- supp_redundancy[supp_redundancy$outcome_key == outcome, , drop = FALSE]

  tab_rows <- list()
  for (target_key in names(REDUNDANCY_TARGETS)) {
    target_label <- REDUNDANCY_TARGETS[[target_key]]
    for (component in c("Between", "Within")) {
      none <- cell(rows, target_label, component, "Alone (no absorber)")
      trust <- cell(rows, target_label, component, "+ Trust")
      pu <- cell(rows, target_label, component, "+ Perceived usefulness")
      tab_rows[[length(tab_rows) + 1L]] <- data.frame(
        Construct = paste0(target_label, " (", tolower(component), ")"),
        b_none = none[1], p_none = none[2],
        b_trust = trust[1], p_trust = trust[2],
        b_pu = pu[1], p_pu = pu[2],
        stringsAsFactors = FALSE
      )
    }
  }
  tab <- do.call(rbind, tab_rows)

  write_apa_table(
    tab,
    file = file.path(TEX_DIR, paste0("supp_redundancy_", outcome, ".tex")),
    caption = sprintf(
      paste0(
        "Environmental concern and oversight preference coefficients predicting ",
        "%s, alone versus alongside trust or perceived usefulness"
      ),
      decapitalise(outcome_label)
    ),
    label = paste0("tab:supp-redundancy-", outcome),
    align = "lrcrcrc",
    size = "\\small",
    header = paste(
      "Construct &",
      "\\multicolumn{2}{c}{Alone} & \\multicolumn{2}{c}{+ Trust} &",
      "\\multicolumn{2}{c}{+ Usefulness} \\\\",
      "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
      " & $b$ & $p$ & $b$ & $p$ & $b$ & $p$"
    ),
    note = paste0(
      "Each construct is entered with tool type, respondent group, and ",
      "demographics, plus only the named absorber (not the full Block 3 ",
      "model); all columns share the same rows. Losing significance once an ",
      "absorber is added indicates shared variance with it, not a genuine null."
    )
  )
}

message("  redundancy models: ", nrow(supp_redundancy), " coefficients across ",
        length(REDUNDANCY_TARGETS), " constructs x ", length(MODEL_B_OUTCOMES),
        " outcomes x ", length(REDUNDANCY_ABSORBERS), " conditions")
for (outcome in MODEL_B_OUTCOMES) {
  for (target_key in names(REDUNDANCY_TARGETS)) {
    rows <- supp_redundancy[supp_redundancy$outcome_key == outcome &
                             supp_redundancy$target_key == target_key, ]
    message("  ", ANALYSIS_SCALES[[outcome]]$label, " / ",
            REDUNDANCY_TARGETS[[target_key]], ": ", paste(
      sprintf("%s/%s b=%s p=%s", rows$absorber_key, rows$component,
              fmt_num(rows$estimate, 2), fmt_p_plain(rows$p)),
      collapse = "; "
    ))
  }
}
