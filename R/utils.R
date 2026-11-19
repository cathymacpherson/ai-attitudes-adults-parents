# Shared helpers: item parsing, scoring, reliability, formatting, LaTeX output.

# ---- Item parsing and scoring ------------------------------------------------

# Qualtrics label exports look like "5 - Slightly agree". Non-numeric responses
# ("Prefer not to say") become NA.
parse_likert <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  numeric_lead <- grepl("^[0-9]+", x)
  out[numeric_lead] <- as.numeric(sub("^([0-9]+).*$", "\\1", x[numeric_lead]))
  out
}

map_scores <- function(x, mapping) {
  as.numeric(mapping[trimws(as.character(x))])
}

# Numeric item-by-participant matrix for one subscale.
item_matrix <- function(data, prefix, items, reverse = FALSE,
                        scale_max = ATTITUDE_SCALE_MAX) {
  cols <- paste0(prefix, "_", items)
  cols <- cols[cols %in% names(data)]
  if (!length(cols)) return(matrix(numeric(0), nrow = nrow(data), ncol = 0))
  m <- matrix(
    unlist(lapply(cols, function(cl) parse_likert(data[[cl]])), use.names = FALSE),
    nrow = nrow(data),
    dimnames = list(NULL, cols)
  )
  if (reverse) m <- scale_max + 1 - m
  m
}

# Subscale score = mean of available items, NA unless at least half the
# subscale's items were answered. For a 2-item subscale this is the same as
# "not all missing" (1 of 2 already clears 50%); for the 4-item Perceived
# usefulness subscale it is stricter, requiring 2 of 4 rather than just 1.
score_items <- function(data, prefix, items, reverse = FALSE,
                        scale_max = ATTITUDE_SCALE_MAX, min_prop = 0.5) {
  m <- item_matrix(data, prefix, items, reverse = reverse, scale_max = scale_max)
  if (!ncol(m)) return(rep(NA_real_, nrow(data)))
  n_answered <- rowSums(!is.na(m))
  out <- rowMeans(m, na.rm = TRUE)
  out[n_answered < ceiling(min_prop * ncol(m))] <- NA_real_
  out
}

# ---- Reliability ------------------------------------------------------------

# Pairwise-complete rather than listwise deletion: some item sets (BACE in
# particular) include a legitimate "Not applicable" response for items that
# don't apply to every respondent (e.g. childcare, workplace concerns), so
# requiring every item in a scale to be answered can throw away most of the
# sample even though each item individually has plenty of data. Listwise and
# pairwise deletion agree exactly for a 2-item scale, so this is a no-op for
# Spearman-Brown subscales.
cronbach_alpha <- function(m) {
  k <- ncol(m)
  if (k < 2L || sum(rowSums(!is.na(m)) > 0) < 3L) return(NA_real_)
  v <- stats::cov(m, use = "pairwise.complete.obs")
  if (anyNA(v)) return(NA_real_)
  total_var <- sum(v)
  if (!is.finite(total_var) || total_var <= 0) return(NA_real_)
  (k / (k - 1)) * (1 - sum(diag(v)) / total_var)
}

# Cronbach's alpha is unstable and hard to interpret for two-item scales;
# Spearman-Brown is the appropriate coefficient there.
spearman_brown <- function(m) {
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (ncol(m) != 2L || nrow(m) < 3L) return(NA_real_)
  r <- stats::cor(m[, 1], m[, 2])
  if (!is.finite(r)) return(NA_real_)
  (2 * r) / (1 + r)
}

mean_inter_item_r <- function(m) {
  if (ncol(m) < 2L || sum(rowSums(!is.na(m)) > 0) < 3L) return(NA_real_)
  R <- stats::cor(m, use = "pairwise.complete.obs")
  if (anyNA(R)) return(NA_real_)
  mean(R[upper.tri(R)])
}

# ---- Centering and within/between decomposition ------------------------------

grand_center <- function(x) x - mean(x, na.rm = TRUE)

# Splits a tool-specific predictor into a between-person component (the
# participant's mean across the two tools, grand-centred) and a within-person
# component (deviation from their own mean).
#
# Both components are set to NA unless BOTH tool scores are present. With only
# two observations per participant, imputing a person mean from a single
# observation would force the within-person component to exactly zero, which
# fabricates "no within-person difference" rather than recording missingness.
add_within_between <- function(long, vars, id = "participant_id") {
  ids <- as.character(long[[id]])
  for (v in vars) {
    person_mean <- tapply(long[[v]], ids, function(z) {
      if (length(z) < 2L || anyNA(z)) NA_real_ else mean(z)
    })
    pm <- as.numeric(person_mean[ids])
    long[[paste0(v, "_btw")]] <- pm - mean(person_mean, na.rm = TRUE)
    long[[paste0(v, "_wth")]] <- long[[v]] - pm
  }
  long
}

# ---- Model fitting helpers --------------------------------------------------

# With two observations per participant a random slope for ai_type is not
# identifiable, so every model here uses a random intercept only. That is
# equivalent to a compound-symmetric residual structure. Warnings are captured
# rather than printed so that singular fits can be reported in a column.
#
# reml = TRUE  -> lmerTest, for coefficient inference with Satterthwaite df.
# reml = FALSE -> lme4 with ML, which is what likelihood-ratio tests of nested
#                 fixed effects require.
fit_lmer <- function(formula, data, reml = TRUE) {
  messages <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      if (reml) {
        lmerTest::lmer(formula, data = data, REML = TRUE)
      } else {
        lme4::lmer(formula, data = data, REML = FALSE)
      },
      error = function(e) {
        messages <<- c(messages, paste("error:", conditionMessage(e)))
        NULL
      }
    ),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, messages = messages)
}

# Nakagawa marginal and conditional R-squared. For a Gaussian random-intercept
# model these have closed forms, so they are computed directly rather than via
# an extra package dependency.
r2_mixed <- function(fit) {
  if (is.null(fit)) return(c(marginal = NA_real_, conditional = NA_real_))
  fixed_part <- as.vector(lme4::getME(fit, "X") %*% lme4::fixef(fit))
  var_fixed <- stats::var(fixed_part)
  var_random <- random_intercept_var(fit)
  var_resid <- residual_var(fit)
  total <- var_fixed + var_random + var_resid
  c(marginal = var_fixed / total,
    conditional = (var_fixed + var_random) / total)
}

# Variance inflation factors for the fixed effects, from the model's own design
# matrix. Reported because the between-person predictors are strongly
# intercorrelated, and because ai_type is correlated with the within-person
# components by construction.
vif_fixed <- function(fit) {
  if (is.null(fit)) return(numeric(0))
  X <- lme4::getME(fit, "X")
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(X) < 2L) return(numeric(0))
  vifs <- vapply(seq_len(ncol(X)), function(j) {
    others <- X[, -j, drop = FALSE]
    r2 <- summary(stats::lm(X[, j] ~ others))$r.squared
    if (!is.finite(r2) || r2 >= 1) NA_real_ else 1 / (1 - r2)
  }, numeric(1))
  stats::setNames(vifs, colnames(X))
}

# Fixed-effect table with Satterthwaite df and matching t-based intervals.
coefficient_table <- function(fit, ...) {
  sm <- summary(fit)$coefficients
  crit <- stats::qt(0.975, sm[, "df"])
  data.frame(
    ...,
    term = rownames(sm),
    estimate = sm[, "Estimate"], std_error = sm[, "Std. Error"],
    df = sm[, "df"], t = sm[, "t value"], p = sm[, "Pr(>|t|)"],
    ci95_low = sm[, "Estimate"] - crit * sm[, "Std. Error"],
    ci95_high = sm[, "Estimate"] + crit * sm[, "Std. Error"],
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Effect size from a t-statistic and its df: r = t/sqrt(t^2+df) (Rosenthal &
# Rosnow's t-to-r conversion). Applies uniformly to any t-based term (a
# continuous slope, a between-groups contrast, an interaction), unlike
# Cohen's dz, which only has a defined meaning for a paired within-participant
# contrast.
effect_r <- function(t, df) t / sqrt(t^2 + df)

is_singular <- function(fit) {
  if (is.null(fit)) return(NA)
  isTRUE(lme4::isSingular(fit, tol = 1e-4))
}

random_intercept_var <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  vc <- as.data.frame(lme4::VarCorr(fit))
  v <- vc$vcov[vc$grp == "participant_id"]
  if (!length(v)) NA_real_ else v[1]
}

residual_var <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  vc <- as.data.frame(lme4::VarCorr(fit))
  v <- vc$vcov[vc$grp == "Residual"]
  if (!length(v)) NA_real_ else v[1]
}

# Lower-cases only the first letter, for dropping a label into mid-sentence.
# tolower() cannot be used because it would also flatten "AI" to "ai".
decapitalise <- function(x) sub("^(\\w)", "\\L\\1", x, perl = TRUE)

# ---- Number formatting (APA) ------------------------------------------------

fmt_num <- function(x, digits = 2, dash = "--") {
  x <- round(as.numeric(x), digits)
  # Rounding a small negative number yields -0, which prints as "-0.00".
  x[!is.na(x) & x == 0] <- 0
  ifelse(is.na(x), dash, formatC(x, format = "f", digits = digits))
}

# APA drops the leading zero on statistics that cannot exceed 1 in absolute
# value (correlations, reliabilities, R^2, ICCs, alpha levels). Statistics that
# can (b, d, t, skew) keep it, so use fmt_num for those.
fmt_r <- function(x, digits = 2, dash = "--") {
  sub("^(-?)0\\.", "\\1.", fmt_num(x, digits = digits, dash = dash))
}

# "M (SD)" cell, formatted with fmt_num -- identical in both papers' own
# 03_descriptives.R, so shared here rather than duplicated.
mean_sd <- function(x, digits = 1) {
  sprintf("%s (%s)", fmt_num(mean(x, na.rm = TRUE), digits),
          fmt_num(stats::sd(x, na.rm = TRUE), digits))
}

# APA drops the leading zero on p values and reports < .001 rather than 0.
fmt_p <- function(p, dash = "--") {
  out <- ifelse(
    is.na(p), dash,
    ifelse(p < .001, "$<$ .001",
           sub("^0\\.", ".", formatC(as.numeric(p), format = "f", digits = 3)))
  )
  out
}

# Plain-text variant for console messages, where "$<$ .001" is just noise.
fmt_p_plain <- function(p, dash = "--") {
  gsub("\\$<\\$", "<", fmt_p(p, dash = dash))
}

fmt_ci <- function(lo, hi, digits = 2, dash = "--") {
  ifelse(is.na(lo) | is.na(hi), dash,
         sprintf("[%s, %s]",
                 formatC(as.numeric(lo), format = "f", digits = digits),
                 formatC(as.numeric(hi), format = "f", digits = digits)))
}

star <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**",
         ifelse(p < .05, "*", ""))))
}

# ---- LaTeX output -----------------------------------------------------------

escape_tex <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "--"
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

# Writes a standalone table *fragment* -- \begin{table} ... \end{table} with no
# preamble -- for \input{} into an Overleaf manuscript.
#
# Requires \usepackage{booktabs} and \usepackage{threeparttable}.
#
# escape = FALSE is the right choice for model tables, whose cells are produced
# by the fmt_* helpers above and deliberately contain math ($<$ .001). Use
# escape = TRUE for tables carrying free text such as response labels.
write_apa_table <- function(df, file, caption, label = NULL, note = NULL,
                            align = NULL, header = NULL, escape = FALSE,
                            landscape = FALSE, size = NULL, tabcolsep = NULL) {
  stopifnot(is.data.frame(df))
  body <- df
  body[] <- if (escape) lapply(body, escape_tex) else lapply(body, function(z) {
    z <- as.character(z)
    z[is.na(z)] <- "--"
    z
  })
  if (is.null(header)) header <- names(df)
  if (is.null(align)) align <- paste0("l", strrep("r", ncol(df) - 1L))

  rows <- if (nrow(body)) {
    vapply(seq_len(nrow(body)), function(i) {
      cells <- unlist(body[i, ], use.names = FALSE)
      # Group-header rows follow the pipeline-wide convention: the group name
      # in the first column, empty strings everywhere else. Emitted as one
      # spanning cell rather than a padded row, because otherwise the header
      # text -- which is usually far longer than any label beneath it -- sets
      # the width of the first column and pushes the data columns off the page.
      if (length(cells) > 1L && nzchar(cells[1L]) && !any(nzchar(cells[-1L]))) {
        return(sprintf("\\multicolumn{%d}{l}{%s} \\\\", length(cells), cells[1L]))
      }
      paste0(paste(cells, collapse = " & "), " \\\\")
    }, character(1))
  } else character(0)

  # The caption/table/notes block, independent of orientation.
  inner <- c(
    sprintf("\\caption{%s}", caption),
    if (!is.null(label)) sprintf("\\label{%s}", label),
    # Wide tables need a smaller font to fit the APA text width.
    if (!is.null(size)) size,
    "\\begin{threeparttable}",
    # Braced so a narrower column separation (for wide tables) doesn't leak
    # into tables that come after this one in the document.
    if (!is.null(tabcolsep)) sprintf("{\\setlength{\\tabcolsep}{%s}", tabcolsep),
    sprintf("\\begin{tabular}{%s}", align),
    "\\toprule",
    paste0(paste(header, collapse = " & "), " \\\\"),
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    if (!is.null(tabcolsep)) "}",
    if (!is.null(note)) c(
      "\\begin{tablenotes}[flushleft]",
      "\\small",
      sprintf("\\item \\textit{Note.} %s", note),
      "\\end{tablenotes}"
    ),
    "\\end{threeparttable}"
  )

  lines <- c(
    "% Generated by the R analysis pipeline -- do not edit by hand.",
    if (landscape)
      "% Requires \\usepackage{booktabs}, \\usepackage{threeparttable} and \\usepackage{graphicx}."
    else
      "% Requires \\usepackage{booktabs} and \\usepackage{threeparttable}.",
    if (landscape) c(
      # A plain \rotatebox (graphicx) rather than sidewaystable (rotating):
      # sidewaystable alternates rotation direction by odd/even page in a
      # twoside document, so two of these in a row can rotate opposite
      # ways. This is fixed at 90 degrees regardless of page parity --
      # flip the sign here if it comes out backwards for your document.
      "\\begin{table}[p]",
      "\\centering",
      "\\rotatebox{90}{%",
      "\\begin{minipage}{\\textheight}",
      "\\centering",
      inner,
      "\\end{minipage}",
      "}",
      "\\end{table}"
    ) else c(
      "\\begin{table}[htbp]",
      "\\centering",
      inner,
      "\\end{table}"
    )
  )

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file)
  record_output(file)
  message("  wrote ", sub(paste0("^", normalizePath(ROOT, winslash = "/"), "/"), "",
                          normalizePath(file, winslash = "/", mustWork = FALSE)))
  invisible(file)
}

write_result_csv <- function(df, name) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(OUT_DIR, paste0(name, ".csv"))
  utils::write.csv(df, path, row.names = FALSE, na = "")
  record_output(path)
  invisible(path)
}

# ---- Stale-output tracking ---------------------------------------------------
# Output directories are regenerated from the current scripts. Files left behind
# by an earlier version of the pipeline are removed at the end of a run, so that
# a table or CSV no longer produced by any step cannot be mistaken for a current
# result -- or silently kept alive by an \input{} in the manuscript.

GENERATED_OUTPUTS <- new.env(parent = emptyenv())
GENERATED_OUTPUTS$paths <- character(0)

record_output <- function(path) {
  GENERATED_OUTPUTS$paths <- c(
    GENERATED_OUTPUTS$paths,
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )
  invisible(NULL)
}

remove_stale_outputs <- function() {
  existing <- c(
    list.files(OUT_DIR, pattern = "\\.csv$", full.names = TRUE),
    list.files(TEX_DIR, pattern = "\\.tex$", full.names = TRUE),
    list.files(FIG_DIR, pattern = "\\.(pdf|png)$", full.names = TRUE)
  )
  existing <- normalizePath(existing, winslash = "/", mustWork = FALSE)
  stale <- setdiff(existing, GENERATED_OUTPUTS$paths)
  for (path in stale) {
    unlink(path)
    message("  removed stale output ", basename(path))
  }
  invisible(stale)
}
