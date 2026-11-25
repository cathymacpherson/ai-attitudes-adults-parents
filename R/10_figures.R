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
# significance. The large lightness gap between dark navy and orange adds a
# luminance cue on top of hue, so the pair stays distinguishable for readers
# with colour-vision deficiencies and in greyscale.

COLOUR_SIG <- "#000000"
COLOUR_NS <- "#000000"
COLOUR_OUTCOME_1 <- "#0D366B"
COLOUR_OUTCOME_2 <- "#EB6834"

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

model_a_main <- model_a_effects[
  model_a_effects$model == "Adjusted" &
    model_a_effects$effect == "Tool type (overall)",
  , drop = FALSE
]
# Shortened to "Trust" on this axis.
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

model_b_coef_main <- model_b_coefficients[model_b_coefficients$block == "B3", , drop = FALSE]
level_suffix <- c("Between-person" = "btw", "Within-person" = "wth")
model_b_predictor_rows <- list()
for (key in names(PREDICTOR_LABELS)) {
  for (level_label in names(level_suffix)) {
    term <- paste0(key, "_", level_suffix[[level_label]])
    ct <- model_b_coef_main[model_b_coef_main$term == term, , drop = FALSE]
    if (!nrow(ct)) next
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

model_c_main <- model_c_coefficients[
  model_c_coefficients$step == "C1" & model_c_coefficients$term != "(Intercept)",
  , drop = FALSE
]
model_c_main$predictor <- ifelse(
  model_c_main$term %in% names(C_TERM_LABELS),
  C_TERM_LABELS[model_c_main$term],
  model_c_main$term
)
# "(centred)" is dropped from the plot axis labels.
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
