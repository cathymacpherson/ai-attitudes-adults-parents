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
# Blocks are fitted with ML for the likelihood-ratio tests (a REML likelihood is
# not comparable across different fixed-effect structures) and refitted with
# REML for coefficient inference.
#
# ai_type is retained in every block. Its coefficient changes meaning as
# predictors enter: in Block 1 it is the total bespoke-versus-general difference,
# and in Block 3 it is the difference remaining once perceptions are accounted
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

# Each block adds its terms to those of the blocks before it. Keys B1--B4 match
# the Block numbering in the paper. Models are assembled from the blocks listed
# in MODEL_B_INCLUDED, so a block can be dropped without leaving its terms
# behind in later blocks; displayed labels are numbered by position.
MODEL_B_BLOCKS <- list(
  B1 = list(label = "Tool type, group, demographics", terms = BASE_TERMS),
  B2 = list(label = "+ core TAM", terms = CORE_TAM),
  B3 = list(label = "+ trust, privacy, oversight, environment",
            terms = EXTENDED),
  interactions = list(label = "+ predictor $\\times$ tool type",
                      terms = TOOL_INTERACTIONS),
  B4 = list(label = "+ attitudes towards AI", terms = ATTITUDE_TERMS,
            outcomes = "intent")
)

# The interactions block is not part of the paper's block sequence and is
# omitted: it was non-significant for both outcomes and added no explanatory
# power, and the same reassurance is available from the similarity of the
# perception effects across tool types. Adding it here would renumber the
# displayed labels of any block that follows it.
MODEL_B_INCLUDED <- c("B1", "B2", "B3", "B4")

block_definitions <- function(outcome) {
  keys <- intersect(MODEL_B_INCLUDED, names(MODEL_B_BLOCKS))
  keys <- Filter(function(key) {
    limit <- MODEL_B_BLOCKS[[key]]$outcomes
    is.null(limit) || outcome %in% limit
  }, keys)

  blocks <- list()
  terms <- character(0)
  for (i in seq_along(keys)) {
    key <- keys[i]
    terms <- c(terms, MODEL_B_BLOCKS[[key]]$terms)
    blocks[[key]] <- list(
      label = paste0(i, ". ", MODEL_B_BLOCKS[[key]]$label),
      terms = terms
    )
  }
  blocks
}

# The block reported as the main-effects model: the full predictor set, without
# interactions and without attitude, so one table answers one question.
MAIN_BLOCK <- "B3"

model_formula <- function(outcome, terms) {
  stats::as.formula(paste(outcome, "~", paste(terms, collapse = " + "),
                          "+ (1 | participant_id)"))
}

# Every block must be fitted on identical rows, or the likelihood-ratio tests
# compare models on different data.
analysis_rows <- function(outcome, all_terms) {
  vars <- unique(c(outcome, "participant_id",
                   unlist(strsplit(all_terms, ":", fixed = TRUE))))
  vars <- vars[vars %in% names(long)]
  long[stats::complete.cases(long[, vars]), , drop = FALSE]
}

block_rows <- list()
coef_rows <- list()
tool_rows <- list()
vif_rows <- list()
attitude_path_rows <- list()
attitude_block_rows <- list()

for (outcome in MODEL_B_OUTCOMES) {
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label
  blocks <- block_definitions(outcome)
  dat <- analysis_rows(outcome, blocks[[length(blocks)]]$terms)

  message("  ", outcome_label, ": n = ", length(unique(dat$participant_id)),
          " participants, ", nrow(dat), " observations")

  ml_fits <- list()
  reml_fits <- list()

  for (block_key in names(blocks)) {
    form <- model_formula(outcome, blocks[[block_key]]$terms)
    ml <- fit_lmer(form, dat, reml = FALSE)
    reml <- fit_lmer(form, dat, reml = TRUE)
    ml_fits[[block_key]] <- ml$fit
    reml_fits[[block_key]] <- reml$fit

    r2 <- r2_mixed(reml$fit)
    previous <- if (block_key == "B1") NULL else ml_fits[[which(names(blocks) == block_key) - 1L]]

    lrt <- list(chisq = NA_real_, df = NA_real_, p = NA_real_)
    if (!is.null(previous) && !is.null(ml$fit)) {
      comparison <- suppressMessages(stats::anova(previous, ml$fit))
      lrt$chisq <- comparison$Chisq[2]
      lrt$df <- comparison$Df[2]
      lrt$p <- comparison$`Pr(>Chisq)`[2]
    }

    delta_r2 <- if (block_key == "B1") NA_real_ else
      r2[["marginal"]] - block_rows[[length(block_rows)]]$r2_marginal

    block_rows[[length(block_rows) + 1L]] <- data.frame(
      outcome = outcome_label, outcome_key = outcome,
      block = block_key, block_label = blocks[[block_key]]$label,
      n_terms = length(blocks[[block_key]]$terms),
      r2_marginal = r2[["marginal"]], r2_conditional = r2[["conditional"]],
      delta_r2_marginal = delta_r2,
      aic = if (is.null(ml$fit)) NA_real_ else stats::AIC(ml$fit),
      lrt_chisq = lrt$chisq, lrt_df = lrt$df, lrt_p = lrt$p,
      singular = is_singular(reml$fit),
      messages = paste(unique(reml$messages), collapse = "; "),
      stringsAsFactors = FALSE
    )

    if (is.null(reml$fit)) next

    # Tool-type coefficient in this block: total effect in Block 1, direct effect
    # once perceptions are partialled out from Block 3 onwards.
    ct <- coefficient_table(reml$fit)
    tool_term <- ct[grepl("^ai_type", ct$term) & !grepl(":", ct$term), , drop = FALSE]
    if (nrow(tool_term)) {
      tool_rows[[length(tool_rows) + 1L]] <- data.frame(
        outcome = outcome_label, outcome_key = outcome,
        block = block_key, block_label = blocks[[block_key]]$label,
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
  if (all(c("B2", "B3") %in% MODEL_B_INCLUDED)) {
    alt <- fit_lmer(model_formula(outcome, c(BASE_TERMS, EXTENDED)), dat)
    if (!is.null(alt$fit)) {
      ct <- coefficient_table(alt$fit)
      tool_term <- ct[grepl("^ai_type", ct$term) & !grepl(":", ct$term), ,
                      drop = FALSE]
      tool_rows[[length(tool_rows) + 1L]] <- data.frame(
        outcome = outcome_label, outcome_key = outcome,
        block = "Alt",
        block_label = "Extended constructs only, without core TAM",
        tool_term[, c("estimate", "std_error", "df", "t", "p",
                      "ci95_low", "ci95_high")],
        row.names = NULL, stringsAsFactors = FALSE
      )
    }
  }

  # Coefficients for the main-effects model.
  main_fit <- reml_fits[[MAIN_BLOCK]]
  if (!is.null(main_fit)) {
    coef_rows[[length(coef_rows) + 1L]] <- coefficient_table(
      main_fit, outcome = outcome_label, outcome_key = outcome, block = MAIN_BLOCK
    )
    v <- vif_fixed(main_fit)
    vif_rows[[length(vif_rows) + 1L]] <- data.frame(
      outcome = outcome_label, outcome_key = outcome, block = MAIN_BLOCK,
      term = names(v), vif = unname(v),
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  # Interaction and attitude blocks are reported on their own, since adding them
  # changes the question rather than refining the same one.
  for (extra_block in intersect(c("interactions", "B4"), names(blocks))) {
    fit <- reml_fits[[extra_block]]
    if (is.null(fit)) next
    ct <- coefficient_table(fit, outcome = outcome_label,
                            outcome_key = outcome, block = extra_block)
    keep <- grepl(":", ct$term) | ct$term %in% ATTITUDE_TERMS
    if (any(keep)) {
      attitude_path_rows[[length(attitude_path_rows) + 1L]] <- ct[keep, , drop = FALSE]
    }
    # The full attitude-block model is kept as well: adding attitudes changes
    # the other perception coefficients (perceived usefulness no longer
    # predicts intentions once attitudes are in), so those need a table of
    # their own rather than only the two attitude terms.
    if (extra_block == "B4") {
      attitude_block_rows[[length(attitude_block_rows) + 1L]] <- ct
    }
  }
}

model_b_blocks <- do.call(rbind, block_rows)
model_b_coefficients <- do.call(rbind, coef_rows)
model_b_tool_effect <- do.call(rbind, tool_rows)
model_b_vif <- do.call(rbind, vif_rows)
model_b_extra_terms <- do.call(rbind, attitude_path_rows)
model_b_attitude_block <- do.call(rbind, attitude_block_rows)

write_result_csv(model_b_blocks, "05_model_b_blocks")
write_result_csv(model_b_coefficients, "05_model_b_coefficients")
write_result_csv(model_b_tool_effect, "05_model_b_tool_effect_by_block")
write_result_csv(model_b_vif, "05_model_b_vif")
write_result_csv(model_b_extra_terms, "05_model_b_interaction_and_attitude_terms")
if (!is.null(model_b_attitude_block)) {
  write_result_csv(model_b_attitude_block, "05_model_b_coefficients_attitude_block")
}

# ---- Table: hierarchical blocks ---------------------------------------------
# Both outcomes in one table, as a row-block each rather than two separate
# tables: the blocks being compared are the same sequence, so reading the
# increments across outcomes matters more than keeping them apart. Intentions
# carry one block the attitudes model does not (attitudes enter as a
# predictor), so the two row-blocks are deliberately uneven.

block_table_rows <- function(outcome, group_label) {
  rows <- model_b_blocks[model_b_blocks$outcome_key == outcome, , drop = FALSE]
  rbind(
    data.frame(Block = group_label, r2m = "", dr2 = "", chisq = "", df = "",
               p = "", aic = "", stringsAsFactors = FALSE),
    data.frame(
      Block = paste0("\\quad ", rows$block_label),
      r2m = fmt_r(rows$r2_marginal, 3),
      dr2 = fmt_r(rows$delta_r2_marginal, 3),
      chisq = fmt_num(rows$lrt_chisq, 2),
      df = ifelse(is.na(rows$lrt_df), "--", as.character(rows$lrt_df)),
      p = fmt_p(rows$lrt_p),
      aic = fmt_num(rows$aic, 1),
      stringsAsFactors = FALSE
    )
  )
}

write_apa_table(
  rbind(
    block_table_rows("att", "Attitudes"),
    block_table_rows("intent", "Behavioural intentions")
  ),
  file = file.path(TEX_DIR, "model_b_blocks.tex"),
  caption = paste0("Hierarchical mixed-effects models predicting attitudes ",
                   "and behavioural intentions"),
  label = "tab:model-b-blocks",
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
    "cases within each outcome."
  )
)

# ---- Table: main-effects coefficients, between and within -------------------
# Long format: a between-person row-block followed by a within-person
# row-block, rather than the two side by side, so the table stays narrow
# enough to fit a document page. Demographic covariates have no within-person
# component (they vary between participants only), so they are omitted from
# the within-person block entirely rather than shown as a placeholder.

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

row_from_cell <- function(label, values) {
  data.frame(
    Predictor = paste0("\\quad ", label),
    b = values[1], ci = values[2], t = values[3], df = values[4], p = values[5],
    stringsAsFactors = FALSE
  )
}

# Builds the between/within table from one model's coefficients. The main
# (Block 3) tables omit the tool-type coefficient because the attenuation
# table reports it; the attitude-block table has no such home for it, so it
# is shown there, in the within-person block since tool type varies within
# participants.
coefficient_tab <- function(ct, labels, show_tool_type = FALSE) {
  btw_rows <- list()
  wth_rows <- list()
  for (key in names(labels)) {
    btw_rows[[length(btw_rows) + 1L]] <-
      row_from_cell(labels[[key]], cell(ct, paste0(key, "_btw")))
    wth_rows[[length(wth_rows) + 1L]] <-
      row_from_cell(labels[[key]], cell(ct, paste0(key, "_wth")))
  }

  # Terms that exist only once (not decomposed) occupy the between-person block.
  other_terms <- ct$term[!grepl("_btw$|_wth$", ct$term) & ct$term != "(Intercept)"]
  other_labels <- c(
    "respondent_typeParent" = "Parent/carer (vs adult)",
    "age_c" = "Age (centred)",
    "gender_adjFemale" = "Female (vs male)",
    "education_c" = "Education (centred)"
  )
  for (term in other_terms) {
    if (grepl("^ai_type", term)) {
      if (show_tool_type) {
        wth_rows[[length(wth_rows) + 1L]] <-
          row_from_cell("Bespoke AI (vs general-purpose)", cell(ct, term))
      }
      next
    }
    label <- if (term %in% names(other_labels)) other_labels[[term]] else term
    btw_rows[[length(btw_rows) + 1L]] <- row_from_cell(label, cell(ct, term))
  }

  group_row <- function(label) {
    data.frame(Predictor = label, b = "", ci = "", t = "", df = "", p = "",
               stringsAsFactors = FALSE)
  }
  rbind(
    group_row("Between-person"), do.call(rbind, btw_rows),
    group_row("Within-person"), do.call(rbind, wth_rows)
  )
}

for (outcome in MODEL_B_OUTCOMES) {
  ct <- model_b_coefficients[model_b_coefficients$outcome_key == outcome, , drop = FALSE]
  outcome_label <- ANALYSIS_SCALES[[outcome]]$label
  tab <- coefficient_tab(ct, PREDICTOR_LABELS)

  write_apa_table(
    tab,
    file = file.path(TEX_DIR, paste0("model_b_coefficients_", outcome, ".tex")),
    caption = sprintf(
      "Between- and within-person predictors of %s",
      decapitalise(outcome_label)
    ),
    label = paste0("tab:model-b-coef-", outcome),
    align = "lrcrrr",
    size = "\\small",
    header = c("Predictor", "$b$", "95\\% CI", "$t$", "$df$", "$p$"),
    # The decomposition itself is explained in the Data Analysis section. What
    # remains here is the unit, why demographic covariates are absent from the
    # within-person block, and that the tool-type coefficient is reported
    # elsewhere, since its absence from this table would otherwise look like
    # an omission.
    note = paste0(
      "$b$ is in scale points (1--7). Demographic covariates and respondent ",
      "group vary between participants only, so are omitted from the ",
      "within-person block. The tool-type coefficient is reported separately."
    )
  )
}

# ---- Table: intentions model with attitudes added (Block 4) -----------------
# Same layout as the main intentions table, with attitudes as a further
# decomposed predictor.

if (!is.null(model_b_attitude_block)) {
  write_apa_table(
    coefficient_tab(
      model_b_attitude_block,
      c(PREDICTOR_LABELS, att = "Attitudes towards AI"),
      show_tool_type = TRUE
    ),
    file = file.path(TEX_DIR, "model_b_coefficients_intent_block4.tex"),
    caption = paste0("Between- and within-person predictors of behavioural ",
                     "intentions to use AI, with attitudes towards AI added"),
    label = "tab:model-b-coef-intent-block4",
    align = "lrcrrr",
    size = "\\small",
    header = c("Predictor", "$b$", "95\\% CI", "$t$", "$df$", "$p$"),
    note = paste0(
      "$b$ is in scale points (1--7). Demographic covariates and respondent ",
      "group vary between participants only, so are omitted from the ",
      "within-person block."
    )
  )
}

# ---- Table: tool-type effect across blocks (total to direct) ----------------

# The attenuation table shows the tool-type coefficient as perceptions enter, so
# it stops at the main model rather than continuing into any later block. The
# final row is the robustness check that reverses which constructs are omitted.
block_keys <- intersect(MODEL_B_INCLUDED, c("B1", "B2", "B3"))
if (all(c("B2", "B3") %in% MODEL_B_INCLUDED)) block_keys <- c(block_keys, "Alt")
# The outcome is a group-header row rather than its own column, matching
# model_c_coefficients and the block table above.
TOOL_EFFECT_OUTCOME_LABELS <- c(att = "Attitudes",
                                intent = "Behavioural intentions")
TOOL_EFFECT_BLOCK_LABELS <- c(
  B1 = "Before perceptions (total effect)",
  B2 = "Adjusting for core TAM",
  B3 = "Adjusting for all perceptions (direct effect)",
  Alt = "Adjusting for extended constructs only"
)
tool_tab_rows <- list()
for (outcome in MODEL_B_OUTCOMES) {
  rows <- model_b_tool_effect[model_b_tool_effect$outcome_key == outcome, , drop = FALSE]
  idx <- match(block_keys, rows$block)
  tool_tab_rows[[length(tool_tab_rows) + 1L]] <- rbind(
    data.frame(Block = TOOL_EFFECT_OUTCOME_LABELS[[outcome]],
               b = "", ci = "", t = "", df = "", p = "",
               stringsAsFactors = FALSE),
    data.frame(
      Block = paste0("\\quad ", TOOL_EFFECT_BLOCK_LABELS[block_keys]),
      b = fmt_num(rows$estimate[idx], 2),
      ci = fmt_ci(rows$ci95_low[idx], rows$ci95_high[idx], 2),
      t = fmt_num(rows$t[idx], 2),
      df = as.character(round(rows$df[idx])),
      p = fmt_p(rows$p[idx]),
      stringsAsFactors = FALSE
    )
  )
}

write_apa_table(
  do.call(rbind, tool_tab_rows),
  file = file.path(TEX_DIR, "model_b_tool_effect.tex"),
  caption = "Bespoke versus general-purpose AI before and after adjusting for perceptions",
  label = "tab:model-b-tool",
  align = "lrcrrr",
  header = c("Model", "$b$", "95\\% CI", "$t$", "$df$", "$p$"),
  # What each row adjusts for is stated in its own label, and the attenuation
  # is interpreted in the text, so only the unit needs stating here.
  note = "$b$ is the bespoke-minus-general difference in scale points."
)

message("  block models: ", nrow(model_b_blocks))
if (any(model_b_blocks$singular, na.rm = TRUE)) {
  message("  NOTE: singular fit at ",
          paste(unique(model_b_blocks$block[which(model_b_blocks$singular)]),
                collapse = ", "))
}
max_vif <- max(model_b_vif$vif, na.rm = TRUE)
message("  maximum VIF in the Block 3 models: ", fmt_num(max_vif, 1),
        " (", model_b_vif$term[which.max(model_b_vif$vif)], ")")
