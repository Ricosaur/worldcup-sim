# calibrate.R
#
# Calibrate elo_per_goal, base_total, and rho against World Cup match data.
#
# Strategy: train on 2014 Brazil WC + 2018 Russia WC (128 matches total),
# validate on 2022 Qatar WC (64 matches). True temporal split, same competition
# type, no data leakage.
#
# After running:
#   1. Paste calibrated params into default_params() in R/model.R if validated.
#   2. Run source("validate.R"); validate_2022() for full Spearman/Brier picture.
#
# Usage:
#   source("R/calibrate.R")
#   calibrate_wc()

source("R/model.R")
source("validate.R")   # provides fetch_all_wc, GROUPS_2014/2018, CODES_*, etc.
suppressWarnings(suppressMessages({ library(dplyr); library(readr) }))

build_training_set <- function() {
  d14 <- fetch_all_wc(TEAMS_2014, 2014, CODES_2014, URL_OVERRIDES_2014)
  d18 <- fetch_all_wc(TEAMS_2018, 2018, CODES_2018, URL_OVERRIDES_2018)
  bind_rows(
    bind_rows(Filter(Negate(is.null), d14)),
    bind_rows(Filter(Negate(is.null), d18))
  ) %>%
    mutate(key = paste(date, pmin(team_a, team_b), pmax(team_a, team_b), sep = "|")) %>%
    distinct(key, .keep_all = TRUE)
}

objective <- function(par, matches) {
  params <- list(elo_per_goal = par[1], base_total = par[2],
                 host_bump = 80, rho = par[3], fast = FALSE)
  scores <- vapply(seq_len(nrow(matches)), function(i) {
    r  <- matches[i, ]
    eg <- elo_to_expected_goals(r$elo_a, r$elo_b, params = params)
    g  <- 0:8
    P  <- outer(g, g, function(h, a)
      dpois(h, eg[["home"]]) * dpois(a, eg[["away"]]) *
        mapply(dc_tau, h, a,
               MoreArgs = list(lambda = eg[["home"]], mu = eg[["away"]],
                               rho = params$rho)))
    P <- P / sum(P)
    p_act <- if (r$score_a > r$score_b) sum(P[row(P) > col(P)])
             else if (r$score_a == r$score_b) sum(P[row(P) == col(P)])
             else sum(P[row(P) < col(P)])
    -log(max(p_act, 1e-10))
  }, numeric(1))
  mean(scores)
}

calibrate_wc <- function() {
  cat("Building training set (2014 + 2018 WC)...\n")
  matches <- build_training_set()
  cat(sprintf("Training on %d unique matches.\n\n", nrow(matches)))

  grid <- expand.grid(
    elo_per_goal = seq(180, 320, by = 20),
    base_total   = seq(2.2, 3.2, by = 0.2),
    rho          = seq(-0.10, 0.00, by = 0.02),
    stringsAsFactors = FALSE
  )
  cat(sprintf("Grid search: %d combinations...\n", nrow(grid)))
  grid$loss <- vapply(seq_len(nrow(grid)), function(i) {
    if (i %% 50 == 0) cat(sprintf("  %d / %d\r", i, nrow(grid)))
    objective(c(grid$elo_per_goal[i], grid$base_total[i], grid$rho[i]), matches)
  }, numeric(1))
  best_g <- grid[which.min(grid$loss), ]
  cat(sprintf("\nBest grid: elo_per_goal=%.0f, base_total=%.1f, rho=%.2f  (loss=%.4f)\n",
              best_g$elo_per_goal, best_g$base_total, best_g$rho, best_g$loss))

  cat("Refining with Nelder-Mead...\n")
  opt <- optim(c(best_g$elo_per_goal, best_g$base_total, best_g$rho),
               objective, matches = matches, method = "Nelder-Mead",
               control = list(maxit = 1000, reltol = 1e-6))

  cat(sprintf(paste0(
    "\n=== Calibrated params (paste into default_params() in R/model.R) ===\n",
    "  elo_per_goal = %.1f   (current default: 245)\n",
    "  base_total   = %.3f  (current default: 2.6)\n",
    "  rho          = %.4f  (current default: -0.03)\n",
    "  host_bump    = 80     (not calibrated -- leave unchanged)\n",
    "  Training log-loss: %.4f\n"
  ), opt$par[1], opt$par[2], opt$par[3], opt$value))

  cat("\nValidating on 2022 WC...\n")
  d22   <- fetch_all_2022()
  m22   <- bind_rows(Filter(Negate(is.null), d22)) %>%
    mutate(key = paste(date, pmin(team_a, team_b), pmax(team_a, team_b), sep = "|")) %>%
    distinct(key, .keep_all = TRUE)
  loss_default <- objective(c(245, 2.6, -0.03), m22)
  loss_cal     <- objective(opt$par, m22)
  cat(sprintf("  Log-loss (default params): %.4f\n", loss_default))
  cat(sprintf("  Log-loss (calibrated):     %.4f\n", loss_cal))
  cat(if (loss_cal < loss_default) "  -> IMPROVED: update default_params()\n"
      else "  -> No improvement on 2022 -- keep defaults\n")

  invisible(opt$par)
}
