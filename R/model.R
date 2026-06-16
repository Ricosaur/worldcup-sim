# model.R
# Team strength + match scoring for the World Cup simulator.
#
# DESIGN (see HANDOVER.md for the full rationale):
# International football can't be fit with a pure Dixon-Coles model because
# confederations play in near-separate pools, so their strength scales don't
# connect. We therefore use a GLOBAL ELO RATING as the team-strength backbone
# (it already encodes cross-confederation comparisons from decades of results),
# and convert each match's Elo gap into expected goals, then draw scorelines
# with a Poisson / Dixon-Coles model.
#
# Optionally, recent results (qualifiers, friendlies) adjust the Elo via the
# standard Elo update, weighted by match importance and recency. That keeps the
# ratings current without trying to fit confederations from scratch.

suppressWarnings(suppressMessages({ library(dplyr) }))

# ----- Elo -> expected goals -------------------------------------------------

# Convert an Elo difference into an expected goal supremacy, then into each
# side's expected goals. The mapping constants are tunable and should be
# CALIBRATED against historical international results (see calibrate.R stub).
#
#   elo_diff = elo_home - elo_away (+ host bump if applicable)
#   expected supremacy (home minus away goals) grows with elo_diff
#   total match goals is modelled as roughly constant, split by supremacy
#
# This is the standard "bivariate Poisson from a strength gap" approach.
elo_to_expected_goals <- function(elo_home, elo_away,
                                   host_home = FALSE,
                                   params = default_params()) {
  ediff <- (elo_home - elo_away) + if (host_home) params$host_bump else 0

  # Supremacy: expected goal difference. Linear in Elo gap through a divisor.
  supremacy <- ediff / params$elo_per_goal

  # Total expected goals in the match (both teams), mild shrink for big gaps so
  # blowouts don't explode. base_total is the average goals in a balanced game.
  total <- params$base_total

  # Split the total around the supremacy. Clamp to keep lambdas positive.
  lh <- pmax(0.05, (total + supremacy) / 2)
  la <- pmax(0.05, (total - supremacy) / 2)
  c(home = lh, away = la)
}

# Default scoring parameters. CALIBRATE these (calibrate.R) before trusting
# absolute probabilities; the defaults are reasonable starting values drawn
# from typical international-football figures.
default_params <- function() {
  list(
    elo_per_goal = 245,   # Elo points roughly equal to one goal of supremacy
    base_total   = 2.6,   # average total goals in a neutral, balanced match
    host_bump    = 80,    # Elo-equivalent home advantage for the host nation
    rho          = -0.03  # Dixon-Coles low-score correction
  )
}

# ----- Dixon-Coles low-score correction --------------------------------------
# Same tau() as the Eliteserien model: nudges the probabilities of 0-0/1-0/0-1/
# 1-1 to match observed rates better than independent Poisson.

dc_tau <- function(hg, ag, lambda, mu, rho) {
  out <- 1
  if (hg == 0 && ag == 0) out <- 1 - lambda * mu * rho
  else if (hg == 0 && ag == 1) out <- 1 + lambda * rho
  else if (hg == 1 && ag == 0) out <- 1 + mu * rho
  else if (hg == 1 && ag == 1) out <- 1 - rho
  max(out, 1e-10)
}

# Sample one match scoreline given expected goals, with the DC correction
# applied via rejection over a capped score grid. Returns c(hg, ag).
sample_scoreline <- function(lh, la, rho = -0.03, max_goals = 8) {
  gh <- 0:max_goals
  # Joint probability matrix with DC correction.
  P <- outer(gh, gh, function(h, a) {
    dpois(h, lh) * dpois(a, la) *
      mapply(dc_tau, h, a, MoreArgs = list(lambda = lh, mu = la, rho = rho))
  })
  P <- P / sum(P)
  idx <- sample.int(length(P), 1, prob = as.vector(P))
  h <- (idx - 1) %% length(gh)
  a <- (idx - 1) %/% length(gh)
  c(h, a)
}

# ----- Elo update (for ingesting recent results) -----------------------------

# Standard Elo update with goal-difference and match-importance weighting,
# following the eloratings.net convention. Use this to roll ratings forward
# through qualifiers and friendlies before the tournament.
#
#   K       base update size
#   imp     match importance multiplier (friendly < qualifier < major)
#   gd      goal difference multiplier (bigger wins move ratings more)
elo_update <- function(elo_a, elo_b, score_a, score_b,
                       K = 40, importance = 1) {
  expected_a <- 1 / (1 + 10 ^ ((elo_b - elo_a) / 400))
  result_a <- if (score_a > score_b) 1 else if (score_a == score_b) 0.5 else 0
  gd <- abs(score_a - score_b)
  gd_mult <- if (gd <= 1) 1 else if (gd == 2) 1.5 else (11 + gd) / 8
  delta <- K * importance * gd_mult * (result_a - expected_a)
  c(a = elo_a + delta, b = elo_b - delta)
}

# Importance multipliers by competition type (eloratings.net style, simplified).
IMPORTANCE <- c(
  friendly        = 1.0,
  qualifier       = 2.5,
  nations_league  = 3.0,
  continental     = 3.5,
  world_cup       = 4.0
)
