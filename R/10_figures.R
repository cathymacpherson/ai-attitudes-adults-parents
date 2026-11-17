# One figure per model, showing its main finding as a dot-and-whisker plot
# (estimate + 95% CI) -- the natural form for every quantity here, since each
# model's headline result already IS an estimate with an interval.
#
# Model D has no figure: its comfort-distribution result reads better as the
# table already in the manuscript than as a plot.
#
# Significance in Figures A and C is encoded by shape alone (filled vs. open
# point), both plain black, so it survives greyscale print. Figure B instead
# uses colour (navy vs. orange) for identity -- between- vs. within-person --
# since there significance is not what is being distinguished; shape there
# marks significance as in A and C.

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

# Each model's results are needed, not just the scored data. Sourced only if
# this is a fresh session, exactly like require_data() above.
if (!exists("model_a_effects", envir = globalenv())) {
  source(file.path(R_DIR, "04_model_a.R"), local = globalenv())
}
if (!exists("model_b_tool_effect", envir = globalenv())) {
  source(file.path(R_DIR, "05_model_b.R"), local = globalenv())
}
if (!exists("model_c_coefficients", envir = globalenv())) {
  source(file.path(R_DIR, "06_model_c.R"), local = globalenv())
}

suppressPackageStartupMessages(library(ggplot2))

message("10_figures")

# ---- Shared style -------------------------------------------------------
# Figures A and C are plain black -- significance is carried by shape (filled
# vs. open point) alone, not by colour.
# Figure B's navy/orange mark between- vs. within-person identity rather than
# significance. Navy is the darkest step (700) of the palette's sequential
# blue ramp, paired with the categorical orange slot; the pairing has not
# been run through the project's CVD validator (no Node.js in this
# environment when it was chosen), but the large lightness gap between the
# two adds a strong luminance cue on top of hue, which should be at least as
# distinguishable as the validated blue/orange adjacent pair it replaces.
# Re-validate if that becomes possible.

COLOUR_SIG <- "#000000"
COLOUR_NS <- "#000000"
COLOUR_OUTCOME_1 <- "#0D366B"
COLOUR_OUTCOME_2 <- "#EB6834"

# "Times", not "Times New Roman": the base pdf() device (Cairo is not
# functional in this R install) only recognises the four standard PDF font
# aliases and errors with "invalid font type" on anything else. "Times" maps
# to Times-Roman, the same typeface family, and renders correctly in both the
# PDF and PNG output.
fig_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "Times") +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(family = "Times", face = "bold"),
    # APA style expects drawn axis lines and tick marks; with the gridlines
    # removed entirely, these are the only structure left on the panel.
    axis.line = ggplot2::element_line(colour = "black", linewidth = 0.4),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4)
  )

# Saves both a vector copy (Overleaf/print) and a PNG (viewing without a PDF
# reader), and registers both with the stale-output tracker so a figure that
# stops being generated is removed rather than left behind.
save_figure <- function(plot, name, width, height) {
  for (ext in c("pdf", "png")) {
    path <- file.path(FIG_DIR, paste0(name, ".", ext))
    ggplot2::ggsave(path, plot, width = width, height = height, units = "in", dpi = 300)
    record_output(path)
    message("  wrote ", sub(paste0("^", normalizePath(ROOT, winslash = "/"), "/"), "",
                            normalizePath(path, winslash = "/", mustWork = FALSE)))
  }
  invisible(NULL)
}

# ---- Figure A: Model A's tool-type effect across the six ATAMHS subscales ---
# The adjusted "purpose-built - general" contrast, overall (averaged over respondent
# group), is the quantity model_a_adjusted.tex reports as its headline column.

model_a_main <- model_a_effects[
  model_a_effects$model == "Adjusted" &
    model_a_effects$effect == "Tool type (overall)",
  , drop = FALSE
]
# "Trust" rather than "Trust in AI" on this axis only -- the table
# (model_a_adjusted.tex etc.) keeps the fuller subscale name.
fig_a_outcome_labels <- sub("^Trust in AI$", "Trust", subscale_labels_a)
model_a_main$outcome <- sub("^Trust in AI$", "Trust", model_a_main$outcome)
model_a_main$outcome <- factor(model_a_main$outcome, levels = rev(fig_a_outcome_labels))
model_a_main$sig <- factor(
  ifelse(!is.na(model_a_main$p_fdr) & model_a_main$p_fdr < ALPHA,
         "p < .05", "n.s."),
  levels = c("p < .05", "n.s.")
)

fig_a <- ggplot2::ggplot(
  model_a_main,
  ggplot2::aes(x = estimate, y = outcome, colour = sig, shape = sig)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = ci95_low, xmax = ci95_high),
    width = 0.15, linewidth = 0.6
  ) +
  ggplot2::geom_point(size = 3, fill = "white", stroke = 1) +
  ggplot2::scale_colour_manual(
    values = c("p < .05" = COLOUR_SIG, "n.s." = COLOUR_NS)
  ) +
  ggplot2::scale_shape_manual(
    values = c("p < .05" = 16, "n.s." = 21)
  ) +
  ggplot2::labs(
    x = expression(paste("Coefficient (", italic(b), "): purpose-built AI - general-purpose AI")),
    y = NULL, colour = NULL, shape = NULL
  ) +
  fig_theme

save_figure(fig_a, "model_a_tool_effect", width = 7, height = 4)

outcome_labels_b <- vapply(ANALYSIS_SCALES[MODEL_B_OUTCOMES], function(s) s$label, character(1))

# ---- Figure B: Model B's between/within predictor coefficients -------------
# The step-by-step tool-type attenuation (Blocks 1-3, plus the extended-only
# robustness check) is easily read off the numbers already in the text, so it
# has no figure. What benefits from one is the full Block 3 coefficient set:
# six predictors x between/within x two outcomes is dense in a table, and the
# text is built entirely around contrasts between them (privacy predicting
# only within-participant, ease of use predicting intentions only between) --
# exactly what a forest plot surfaces at a glance.

model_b_coef_main <- model_b_coefficients[model_b_coefficients$block == "B3", , drop = FALSE]
level_suffix <- c("Between-person" = "btw", "Within-person" = "wth")
model_b_predictor_rows <- list()
for (key in names(PREDICTOR_LABELS)) {
  for (level_label in names(level_suffix)) {
    term <- paste0(key, "_", level_suffix[[level_label]])
    ct <- model_b_coef_main[model_b_coef_main$term == term, , drop = FALSE]
    if (!nrow(ct)) next
    # "Trust" rather than "Trust in AI" on this axis only -- the table
    # (model_b_coefficients_*.tex) keeps the fuller predictor name.
    model_b_predictor_rows[[length(model_b_predictor_rows) + 1L]] <- data.frame(
      outcome = vapply(ct$outcome_key, function(k) ANALYSIS_SCALES[[k]]$label, character(1)),
      predictor = sub("^Trust in AI$", "Trust", PREDICTOR_LABELS[[key]]), level = level_label,
      estimate = ct$estimate, ci_low = ct$ci95_low, ci_high = ct$ci95_high, p = ct$p,
      stringsAsFactors = FALSE
    )
  }
}
model_b_main <- do.call(rbind, model_b_predictor_rows)
fig_b_predictor_labels <- sub("^Trust in AI$", "Trust", unname(PREDICTOR_LABELS))
model_b_main$predictor <- factor(model_b_main$predictor, levels = rev(fig_b_predictor_labels))
model_b_main$outcome <- factor(model_b_main$outcome, levels = outcome_labels_b)
model_b_main$level <- factor(model_b_main$level, levels = names(level_suffix))
model_b_main$sig <- factor(
  ifelse(!is.na(model_b_main$p) & model_b_main$p < ALPHA, "p < .05", "n.s."),
  levels = c("p < .05", "n.s.")
)

fig_b <- ggplot2::ggplot(
  model_b_main,
  ggplot2::aes(x = estimate, y = predictor, colour = level, shape = sig, group = level)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = ci_low, xmax = ci_high),
    width = 0.15, linewidth = 0.6, position = ggplot2::position_dodge(width = 0.5)
  ) +
  ggplot2::geom_point(
    size = 3, fill = "white", stroke = 1, position = ggplot2::position_dodge(width = 0.5)
  ) +
  ggplot2::scale_colour_manual(
    values = c("Between-person" = COLOUR_OUTCOME_1, "Within-person" = COLOUR_OUTCOME_2)
  ) +
  ggplot2::scale_shape_manual(values = c("p < .05" = 16, "n.s." = 21)) +
  ggplot2::facet_wrap(~outcome) +
  ggplot2::labs(
    x = expression(paste("Coefficient (", italic(b), ")")), y = NULL, colour = NULL, shape = NULL
  ) +
  fig_theme

save_figure(fig_b, "model_b_predictors", width = 9, height = 5)

# ---- Figure C: Model C's demographic/familiarity predictors -----------------
# "Who holds positive attitudes and intentions" is the main-effects (C1)
# coefficient set already reported in model_c_coefficients.tex, faceted here
# by outcome instead of laid out in table columns.

model_c_main <- model_c_coefficients[
  model_c_coefficients$step == "C1" & model_c_coefficients$term != "(Intercept)",
  , drop = FALSE
]
model_c_main$predictor <- ifelse(
  model_c_main$term %in% names(C_TERM_LABELS),
  C_TERM_LABELS[model_c_main$term],
  model_c_main$term
)
# "(centred)" is precise for the manuscript table (model_c_coefficients.tex)
# but redundant clutter on a plot axis, so it is dropped here only.
model_c_main$predictor <- sub(" \\(centred\\)", "", model_c_main$predictor)
predictor_order <- sub(
  " \\(centred\\)", "",
  unname(C_TERM_LABELS[names(C_TERM_LABELS) %in% model_c_main$term])
)
model_c_main$predictor <- factor(model_c_main$predictor, levels = rev(predictor_order))
model_c_main$outcome <- factor(model_c_main$outcome, levels = outcome_labels_b)
model_c_main$sig <- factor(
  ifelse(!is.na(model_c_main$p_fdr) & model_c_main$p_fdr < ALPHA, "p < .05", "n.s."),
  levels = c("p < .05", "n.s.")
)

fig_c <- ggplot2::ggplot(
  model_c_main,
  ggplot2::aes(x = estimate, y = predictor, colour = sig, shape = sig)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = ci95_low, xmax = ci95_high),
    width = 0.15, linewidth = 0.6
  ) +
  ggplot2::geom_point(size = 3, fill = "white", stroke = 1) +
  ggplot2::scale_colour_manual(values = c("p < .05" = COLOUR_SIG, "n.s." = COLOUR_NS)) +
  ggplot2::scale_shape_manual(values = c("p < .05" = 16, "n.s." = 21)) +
  ggplot2::facet_wrap(~outcome) +
  ggplot2::labs(
    x = expression(paste("Coefficient (", italic(b), ")")), y = NULL, colour = NULL, shape = NULL
  ) +
  fig_theme

save_figure(fig_c, "model_c_predictors", width = 9, height = 5)

message("  wrote 3 figures (pdf + png) to ", sub(
  paste0("^", normalizePath(ROOT, winslash = "/"), "/"), "",
  normalizePath(FIG_DIR, winslash = "/", mustWork = FALSE)
))
