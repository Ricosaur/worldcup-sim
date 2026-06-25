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
    rho          = -0.03, # Dixon-Coles low-score correction
    fast         = TRUE   # use direct Poisson draws (validated; see HANDOVER.md)
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

# ----- Bayesian in-tournament Elo adjustment ---------------------------------

# Nudges each team's Elo based on how they are over/underperforming their
# pre-tournament rating during the current tournament. Uses shrinkage so the
# adjustment is negligible after 1 match and grows as evidence accumulates.
#
#   adj = elo + (overperf_per_match * elo_per_overperf) * n/(n+k)
#
# k = 10: after 3 group games the weight is ~23%; after a full 7-match run ~41%.
# elo_per_overperf = 60: 1 unexpected point per match ≈ 60 Elo.
adjusted_elo <- function(elo, results_df, params = default_params(),
                         k = 10, elo_per_overperf = 60) {
  if (is.null(results_df) || nrow(results_df) == 0) return(elo)
  teams    <- names(elo)
  exp_pts  <- setNames(numeric(length(teams)), teams)
  act_pts  <- setNames(numeric(length(teams)), teams)
  n_played <- setNames(integer(length(teams)), teams)

  for (i in seq_len(nrow(results_df))) {
    r  <- results_df[i, ]
    ta <- as.character(r$team_a); tb <- as.character(r$team_b)
    if (!ta %in% teams || !tb %in% teams) next
    if (is.na(r$score_a) || is.na(r$score_b)) next

    eg <- elo_to_expected_goals(elo[[ta]], elo[[tb]], params = params)
    g  <- 0:8
    P  <- outer(g, g, function(h, a)
      dpois(h, eg[["home"]]) * dpois(a, eg[["away"]]) *
        mapply(dc_tau, h, a,
               MoreArgs = list(lambda = eg[["home"]], mu = eg[["away"]],
                               rho = params$rho)))
    P <- P / sum(P)
    p_win  <- sum(P[row(P) > col(P)])
    p_draw <- sum(P[row(P) == col(P)])
    p_loss <- sum(P[row(P) < col(P)])

    pts_a <- if (r$score_a > r$score_b) 3L else if (r$score_a == r$score_b) 1L else 0L
    pts_b <- if (r$score_b > r$score_a) 3L else if (r$score_b == r$score_a) 1L else 0L

    exp_pts[ta]  <- exp_pts[ta]  + 3 * p_win  + p_draw
    exp_pts[tb]  <- exp_pts[tb]  + 3 * p_loss + p_draw
    act_pts[ta]  <- act_pts[ta]  + pts_a
    act_pts[tb]  <- act_pts[tb]  + pts_b
    n_played[ta] <- n_played[ta] + 1L
    n_played[tb] <- n_played[tb] + 1L
  }

  adj <- elo
  for (team in teams) {
    n <- n_played[team]
    if (n == 0L) next
    overperf_per_match <- (act_pts[team] - exp_pts[team]) / n
    adj[team] <- elo[team] + overperf_per_match * elo_per_overperf * (n / (n + k))
  }
  adj
}

# ----- Single-match probability (analytical) ---------------------------------

# Given two Elo ratings, compute win/draw/loss probabilities and the full score
# distribution using the same DC model as the simulator.
# home_a = TRUE adds a host bump to team A (use for non-neutral venues).
match_probabilities <- function(elo_a, elo_b, home_a = FALSE,
                                params = default_params(), max_goals = 8) {
  lambdas <- elo_to_expected_goals(elo_a, elo_b,
                                   host_home = home_a, params = params)
  lh  <- unname(lambdas["home"])
  la  <- unname(lambdas["away"])
  rho <- params$rho
  gh  <- 0:max_goals
  P   <- outer(gh, gh, function(h, a) {
    dpois(h, lh) * dpois(a, la) *
      mapply(dc_tau, h, a, MoreArgs = list(lambda = lh, mu = la, rho = rho))
  })
  P <- P / sum(P)
  list(
    win_a        = sum(P[row(P) > col(P)]),
    draw         = sum(diag(P)),
    win_b        = sum(P[row(P) < col(P)]),
    xg_a         = lh,
    xg_b         = la,
    score_matrix = P,
    goals        = gh
  )
}
