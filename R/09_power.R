# A priori power analysis for the mixed-effects designs (simr).
#
# NOTHING IN THIS SCRIPT USES THE COLLECTED DATA. The design, the variance
# components and the effect sizes are all specified from external sources or
# stated assumptions, so the analysis is not conditioned on the sample it is
# meant to justify. simr::makeLmer builds a model directly from those parameters
# and powerSim simulates from it.
#
# Requires lme4 >= 2.0. simr's makeLmer, `VarCorr<-` and `sigma<-` all call
# lme4::getReCovs, which does not exist in lme4 1.x; on lme4 1.1.37 they fail
# with "'getReCovs' is not an exported object from 'namespace:lme4'".
#
# EVERYTHING IS IN STANDARDISED UNITS. Total outcome variance is fixed at 1 and
# partitioned into a between-participant component (the ICC) and a residual
# component (1 - ICC). This removes any need to borrow a scale SD from the
# observed data, and removes the need to convert between the 5-, 6- and 7-point
# scales used across studies: an effect of b = 0.30 is 0.30 total SDs.
#
# EFFECT SIZES.
#
#   Kleine, Kokje, Lermer & Gaube (2023), JMIR Human Factors, 10, e46859.
#     206 prospective psychotherapists each rated TWO AI-enabled mental health
#     tools -- the same within-participant, two-tool structure assumed here,
#     which is what makes it a defensible anchor. Their Table 1 gives, across
#     the two tools: perceived usefulness 3.2 (0.9) vs 2.8 (1.1); trust
#     3.4 (0.9) vs 3.0 (1.0); intention 2.9 (1.1) vs 2.4 (1.2), corresponding to
#     d of about 0.40 to 0.43. The between-tool correlation is not reported, so d
#     is computed from the pooled SD; for correlated measures d_z is normally
#     LARGER than d, so treating 0.40 as the target d_z is conservative. Their
#     structural models give perceived usefulness -> intention of beta = .51
#     (feedback tool) and .28 (treatment-recommendation tool); the smaller path
#     is used as the smallest effect of interest.
#
#   Wagner & Schwind (2025), BMC Psychology, 13, 719.
#     179 licensed psychotherapists. Used ONLY as a more conservative anchor for
#     the perception-to-outcome path: their smallest significant standardised
#     predictor of AI acceptance attitudes was beta = .148, with others at .191
#     and .231. Their design is not within-participant and reports no group
#     comparison, so it anchors neither the tool contrast nor any group contrast.
#
#   No source anchors an adult versus parent/carer difference, because comparing
#   self-report with proxy report of attitudes toward generative AI in mental
#   health care has no published precedent -- that is the gap this study
#   addresses. No target is specified for it, and none is invented. Instead the
#   group term is held at zero throughout, which turns it into a check that the
#   simulation recovers the nominal false-positive rate.
#
#   Maas & Hox (2005), Methodology, 1, 86-92.
#     Two uses. First, the level-2 sample size: fixed effects and their standard
#     errors are estimated accurately with 50 or more level-2 units. Second, the
#     practice of examining a RANGE of intraclass correlations rather than one
#     assumed value, since no external estimate of the correlation between a
#     participant's two tool ratings is available for this design.

# Runnable on its own: sets up the session if it is not already set up.
if (!exists("SETUP_DONE")) {
  .p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(.p, "R", "_setup.R"))) {
    if (identical(dirname(.p), .p)) stop("Run from inside the repository.")
    .p <- dirname(.p)
  }
  source(file.path(.p, "R", "_setup.R"))
}

message("09_power")

stopifnot(requireNamespace("simr", quietly = TRUE))
if (utils::packageVersion("lme4") < "2.0") {
  stop("09_power requires lme4 >= 2.0; simr's makeLmer fails on lme4 1.x. ",
       "Run install.packages(c('Matrix','lme4','lmerTest','simr')).",
       call. = FALSE)
}

# Match the simulated test to the final analyses, which use Satterthwaite
# degrees of freedom via lmerTest.
simr::simrOptions(lmerTestDdf = "Satterthwaite")

# ---- Externally specified assumptions ---------------------------------------

POWER_SEED_BASE <- 700000  # per-condition seeds are POWER_SEED_BASE + row index
POWER_NSIM <- 1000         # simulations per power estimate
POWER_NSIM_CURVE <- 1000   # simulations per point on a sample-size curve
POWER_ALPHA <- 0.05
POWER_TARGET_N <- 200      # planned participants, each contributing 2 ratings
POWER_BREAKS <- c(25, 50, 75, 100, 150, 200, 250)

# Conditions are run in parallel. Each carries its own seed, so results do not
# depend on how the work is distributed and a serial run reproduces a parallel
# one exactly. Set to 1 to force serial execution.
POWER_CORES <- max(1L, min(6L, parallel::detectCores(logical = TRUE) - 1L))

# No external estimate of the within-participant correlation between the two
# tool ratings exists, so power is reported across a range of plausible ICCs
# rather than at a single assumed value (Maas & Hox, 2005).
POWER_ICCS <- c(0.3, 0.5, 0.7)

TARGET_DZ_TOOL <- 0.40               # Kleine et al. (2023), within participants
TARGET_BETA_PU <- 0.28               # Kleine et al. (2023), between participants
TARGET_BETA_PU_CONSERVATIVE <- 0.148 # Wagner & Schwind (2025)

# With total variance 1, the SD of a within-participant difference is
# sqrt(2 * residual variance), so a target d_z converts as follows. A
# standardised between-person path converts to itself, because the outcome and
# the standardised predictor each have approximately unit variance.
b_within_from_dz <- function(dz, icc) dz * sqrt(2 * (1 - icc))

# ---- Synthetic design -------------------------------------------------------
#
# Group allocation ALTERNATES rather than being assigned in blocks. This matters:
# the between-person covariate is quantile-spaced in ascending order, so a
# blocked allocation would make group membership and the covariate almost
# perfectly collinear, inflating the covariate's standard error and badly
# understating its power. Alternating allocation is orthogonal to any monotone
# covariate and keeps every prefix of the design balanced.
#
# The orthogonality is asserted rather than assumed, so a future change to the
# design generator cannot silently reintroduce the problem.

make_design <- function(n_participants) {
  base <- data.frame(
    participant_id = factor(seq_len(n_participants)),
    parent = rep(c(0L, 1L), length.out = n_participants),
    pu_btw = as.numeric(scale(stats::qnorm(
      seq_len(n_participants) / (n_participants + 1)
    )))
  )
  design <- rbind(cbind(base, bespoke = 0L), cbind(base, bespoke = 1L))

  collinearity <- abs(stats::cor(design$parent, design$pu_btw))
  if (collinearity > 0.05) {
    stop("Synthetic design is confounded: |cor(parent, pu_btw)| = ",
         round(collinearity, 3), call. = FALSE)
  }
  design
}

build_model <- function(n_participants, icc, b_tool, b_pu, b_group = 0) {
  simr::makeLmer(
    y ~ bespoke + parent + pu_btw + (1 | participant_id),
    fixef = c("(Intercept)" = 4, "bespoke" = b_tool,
              "parent" = b_group, "pu_btw" = b_pu),
    VarCorr = icc,
    sigma = sqrt(1 - icc),
    data = make_design(n_participants)
  )
}

# ---- Verify the constructed model carries the intended parameters ------------
# makeLmer will accept a malformed specification silently, so the parameters are
# read back out and checked before any simulation runs.

check_icc <- 0.5
check_model <- build_model(POWER_TARGET_N, check_icc,
                          b_tool = b_within_from_dz(TARGET_DZ_TOOL, check_icc),
                          b_pu = TARGET_BETA_PU)
check_vc <- as.data.frame(lme4::VarCorr(check_model))
check_between <- check_vc$vcov[check_vc$grp == "participant_id"]
check_resid <- check_vc$vcov[check_vc$grp == "Residual"]

stopifnot(
  isTRUE(all.equal(unname(lme4::fixef(check_model)[["bespoke"]]),
                   b_within_from_dz(TARGET_DZ_TOOL, check_icc))),
  isTRUE(all.equal(unname(lme4::fixef(check_model)[["pu_btw"]]), TARGET_BETA_PU)),
  isTRUE(all.equal(check_between, check_icc)),
  isTRUE(all.equal(check_resid, 1 - check_icc))
)
message("  parameter check passed: between var = ", fmt_num(check_between, 3),
        ", residual var = ", fmt_num(check_resid, 3),
        ", implied ICC = ", fmt_num(check_between / (check_between + check_resid), 3))

# ---- Condition table --------------------------------------------------------
# Every condition carries its own seed, so any single estimate can be reproduced
# without rerunning the whole analysis.

conditions <- rbind(
  # Power at the planned sample size.
  do.call(rbind, lapply(POWER_ICCS, function(icc) rbind(
    data.frame(key = "tool", term = "bespoke", icc = icc,
               n_participants = POWER_TARGET_N, b_pu = TARGET_BETA_PU,
               nsim = POWER_NSIM, part = "target", stringsAsFactors = FALSE),
    data.frame(key = "usefulness", term = "pu_btw", icc = icc,
               n_participants = POWER_TARGET_N, b_pu = TARGET_BETA_PU,
               nsim = POWER_NSIM, part = "target", stringsAsFactors = FALSE),
    data.frame(key = "usefulness_conservative", term = "pu_btw", icc = icc,
               n_participants = POWER_TARGET_N,
               b_pu = TARGET_BETA_PU_CONSERVATIVE,
               nsim = POWER_NSIM, part = "target", stringsAsFactors = FALSE),
    data.frame(key = "group_null", term = "parent", icc = icc,
               n_participants = POWER_TARGET_N, b_pu = TARGET_BETA_PU,
               nsim = POWER_NSIM, part = "target", stringsAsFactors = FALSE)
  ))),
  # Sample-size curves.
  do.call(rbind, lapply(POWER_ICCS, function(icc) do.call(rbind, lapply(
    POWER_BREAKS, function(n) rbind(
      data.frame(key = "tool", term = "bespoke", icc = icc, n_participants = n,
                 b_pu = TARGET_BETA_PU, nsim = POWER_NSIM_CURVE,
                 part = "curve", stringsAsFactors = FALSE),
      data.frame(key = "usefulness", term = "pu_btw", icc = icc,
                 n_participants = n, b_pu = TARGET_BETA_PU,
                 nsim = POWER_NSIM_CURVE, part = "curve",
                 stringsAsFactors = FALSE)
    )
  ))))
)
conditions$seed <- POWER_SEED_BASE + seq_len(nrow(conditions))

run_condition <- function(i) {
  row <- conditions[i, ]
  model <- build_model(row$n_participants, row$icc,
                       b_tool = b_within_from_dz(TARGET_DZ_TOOL, row$icc),
                       b_pu = row$b_pu, b_group = 0)
  sim <- summary(simr::powerSim(
    model, test = simr::fixed(row$term, "t"), nsim = row$nsim,
    alpha = POWER_ALPHA, seed = row$seed, progress = FALSE
  ))
  data.frame(
    row,
    b = unname(lme4::fixef(model)[[row$term]]),
    power = sim$mean, ci_low = sim$lower, ci_high = sim$upper,
    successes = sim$successes, trials = sim$trials,
    stringsAsFactors = FALSE
  )
}

message("  running ", nrow(conditions), " conditions (",
        format(sum(conditions$nsim), big.mark = ","), " model fits) on ",
        POWER_CORES, " core", if (POWER_CORES > 1L) "s" else "")

power_started <- Sys.time()
if (POWER_CORES > 1L) {
  cluster <- parallel::makeCluster(POWER_CORES)
  invisible(parallel::clusterEvalQ(cluster, {
    suppressPackageStartupMessages({
      library(lme4)
      library(lmerTest)
      library(simr)
    })
    simr::simrOptions(lmerTestDdf = "Satterthwaite")
    NULL
  }))
  parallel::clusterExport(
    cluster,
    varlist = c("conditions", "make_design", "build_model", "b_within_from_dz",
                "TARGET_DZ_TOOL", "POWER_ALPHA", "run_condition"),
    envir = environment()
  )
  results <- parallel::parLapplyLB(cluster, seq_len(nrow(conditions)),
                                   run_condition)
  parallel::stopCluster(cluster)
} else {
  results <- lapply(seq_len(nrow(conditions)), run_condition)
}
power_results <- do.call(rbind, results)

message("  completed in ",
        fmt_num(as.numeric(difftime(Sys.time(), power_started, units = "mins")), 1),
        " minutes")

power_at_target <- power_results[power_results$part == "target", ]
power_curves <- power_results[power_results$part == "curve", ]
write_result_csv(power_results, "09_power_results")
write_result_csv(power_at_target, "09_power_at_target_n")
write_result_csv(power_curves, "09_power_curves")

for (icc in POWER_ICCS) {
  rows <- power_at_target[power_at_target$icc == icc, ]
  message("    ICC ", icc, ": tool ",
          fmt_num(100 * rows$power[rows$key == "tool"], 1),
          "%, usefulness ",
          fmt_num(100 * rows$power[rows$key == "usefulness"], 1),
          "% (conservative ",
          fmt_num(100 * rows$power[rows$key == "usefulness_conservative"], 1),
          "%), false-positive rate ",
          fmt_num(100 * rows$power[rows$key == "group_null"], 1), "%")
}

n_for_80 <- function(key, icc) {
  rows <- power_curves[power_curves$key == key & power_curves$icc == icc, ]
  reached <- rows$n_participants[rows$power >= 0.80]
  if (!length(reached)) NA_integer_ else as.integer(min(reached))
}

n80_range <- function(key) {
  ns <- vapply(POWER_ICCS, function(i) n_for_80(key, i), integer(1))
  if (all(is.na(ns))) return("--")
  if (length(unique(ns[!is.na(ns)])) == 1L) return(as.character(unique(ns[!is.na(ns)])))
  paste0(min(ns, na.rm = TRUE), "--", max(ns, na.rm = TRUE))
}

# Numbers are still computed and saved (09_power_results.csv,
# 09_power_at_target_n.csv, 09_power_curves.csv) for reference; no
# power_analysis.tex table is written -- already reported in the manuscript text.

message("  N for 80% power: tool = ", n80_range("tool"),
        ", usefulness = ", n80_range("usefulness"))
