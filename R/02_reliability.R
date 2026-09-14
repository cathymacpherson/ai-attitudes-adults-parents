# Internal consistency of the Attitudes Towards AI in Mental Health Services
# Questionnaire (ATAMHS), separately for each AI tool type and respondent group.
#
# Cronbach's alpha is reported for the four-item usefulness scale. The remaining
# seven subscales have two items each, where alpha is unstable and difficult to
# interpret; Spearman-Brown is the appropriate coefficient and is reported
# alongside the inter-item correlation, which for a two-item scale is the whole
# of the evidence.

# Runnable on its own: sets up the session if it is not already set up.
if (!exists("PAPER1_SETUP")) {
  .p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(.p, "R", "_setup.R"))) {
    if (identical(dirname(.p), .p)) stop("Run from inside the repository.")
    .p <- dirname(.p)
  }
  source(file.path(.p, "R", "_setup.R"))
}
paper1_require_data()

message("02_reliability")

rows <- list()
for (tool_key in names(TOOLS)) {
  for (rg in names(REPORT_GROUPS)) {
    in_group <- participants$report_key == rg
    if (!any(in_group)) next
    dat <- raw_by_id[in_group, , drop = FALSE]
    prefix <- paste0("attn_", tool_key, "_", rg)

    for (key in names(SUBSCALES)) {
      spec <- SUBSCALES[[key]]
      m <- item_matrix(dat, prefix, spec$items,
                       reverse = REVERSE_CONCERN_SCALES && spec$reverse)
      two_item <- ncol(m) == 2L

      rows[[length(rows) + 1L]] <- data.frame(
        subscale = spec$label,
        subscale_key = key,
        ai_type = TOOLS[[tool_key]],
        respondent_type = REPORT_GROUPS[[rg]],
        n_items = ncol(m),
        n = sum(stats::complete.cases(m)),
        coefficient = if (two_item) "Spearman-Brown" else "Cronbach's alpha",
        estimate = if (two_item) spearman_brown(m) else cronbach_alpha(m),
        inter_item_r = mean_inter_item_r(m),
        stringsAsFactors = FALSE
      )
    }
  }
}

reliability <- do.call(rbind, rows)
write_result_csv(reliability, "02_reliability")

# ---- Table: reliability by tool type and respondent group -------------------

# Rows follow TABLE_VAR_ORDER (00_config.R) rather than SUBSCALES' own order,
# so this table lists the same eight variables in the same order as the
# correlations_general/correlations_bespoke tables.
subscale_labels <- unname(vapply(SUBSCALES[TABLE_VAR_ORDER], function(s) s$label, character(1)))
n_items <- unname(vapply(SUBSCALES[TABLE_VAR_ORDER], function(s) length(s$items), integer(1)))

lookup <- function(respondent, tool) {
  idx <- match(
    paste(subscale_labels, respondent, tool),
    paste(reliability$subscale, reliability$respondent_type, reliability$ai_type)
  )
  fmt_num(reliability$estimate[idx], 2)
}

# Short row labels shared with the correlation tables (TABLE_VAR_LABELS in
# 00_config.R). `subscale_labels` above still carries the full SUBSCALES
# labels used to look up `reliability`, so this substitution can't break
# that match.
display_labels <- unname(TABLE_VAR_LABELS[TABLE_VAR_ORDER])

tab <- data.frame(
  Subscale = display_labels,
  Items = n_items,
  Coefficient = ifelse(n_items == 2L, "$r_{SB}$", "$\\alpha$"),
  adult_gen = lookup("Adult", "General-purpose AI"),
  adult_besp = lookup("Adult", "Bespoke AI"),
  parent_gen = lookup("Parent", "General-purpose AI"),
  parent_besp = lookup("Parent", "Bespoke AI"),
  stringsAsFactors = FALSE
)

# Two-row header with spanned group columns, built in one string so the table
# writer stays a dumb printer.
spanned_header <- paste(
  "Subscale & Items & Coef. &",
  "\\multicolumn{2}{c}{Adults} & \\multicolumn{2}{c}{Parents/carers} \\\\",
  "\\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  " &  &  & General & Bespoke & General & Bespoke"
)

write_apa_table(
  tab,
  file = file.path(TEX_DIR, "reliability.tex"),
  caption = "Internal consistency of the ATAMHS subscales by AI tool type and respondent group",
  label = "tab:reliability",
  align = "llccccc",
  header = spanned_header,
  note = paste0(
    "$\\alpha$ = Cronbach's alpha; $r_{SB}$ = Spearman-Brown coefficient, ",
    "reported for two-item subscales where $\\alpha$ is unstable. ",
    "General = general-purpose AI tools; Bespoke = AI tools designed specifically ",
    "for mental health assessment or treatment."
  )
)

# ---- Scale definitions: as specified versus as analysed ---------------------
# Documents any departure from the administered questionnaire, so the reliability
# evidence above and the scoring used in the models can be read together.

definition_rows <- lapply(names(SUBSCALES), function(key) {
  specified <- SUBSCALES[[key]]$items
  analysed <- ANALYSIS_SCALES[[key]]$items
  data.frame(
    subscale_key = key,
    subscale = SUBSCALES[[key]]$label,
    items_specified = paste(specified, collapse = ", "),
    items_analysed = paste(analysed, collapse = ", "),
    changed = !identical(as.integer(specified), as.integer(analysed)),
    stringsAsFactors = FALSE
  )
})
scale_definitions <- do.call(rbind, definition_rows)
write_result_csv(scale_definitions, "02_scale_definitions")

changed <- scale_definitions[scale_definitions$changed, , drop = FALSE]
if (nrow(changed)) {
  message("  scale changes applied for modelling: ",
          paste(changed$subscale, collapse = ", "))
}

message("  estimates range ",
        paste(fmt_num(range(reliability$estimate, na.rm = TRUE), 2), collapse = " to "))
low <- reliability[!is.na(reliability$estimate) & reliability$estimate < 0.70, ]
if (nrow(low)) {
  message("  below .70 in ", nrow(low), " of ", nrow(reliability), " cells:")
  for (i in seq_len(nrow(low))) {
    message("    ", low$subscale[i], " -- ", low$respondent_type[i], " / ",
            low$ai_type[i], ": ", fmt_num(low$estimate[i], 2),
            " (inter-item r = ", fmt_num(low$inter_item_r[i], 2), ")")
  }
  if (any(low$subscale_key == "privacy")) {
    message("    -> Expected: item 13 asks about worry, item 14 about wanting ",
            "safeguards. This is the evidence for the override applied at ",
            "SCALE_OVERRIDES in R/00_config.R; the models use item 13 alone.")
  }
}
