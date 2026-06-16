# tournament.R
# World Cup tournament structure simulation: group stage -> knockout bracket.
#
# This is the part with no Eliteserien equivalent. A league is one round-robin
# table; a World Cup is a group stage that FEEDS a single-elimination bracket,
# with knockout draws resolved by extra time and then penalties (near coin
# flip). The Monte Carlo simulates the whole structure each iteration.
#
# Format assumed: 2026 expanded format, 48 teams, 12 groups of 4, top 2 plus 8
# best third-placed teams advance to a 32-team knockout. This is configurable;
# the classic 32-team / 8-group / top-2 format is also supported via config.
# See HANDOVER.md — the bracket wiring for the 48-team format is the fiddliest
# part and is left as a clearly-marked stub to finish.

suppressWarnings(suppressMessages({ library(dplyr) }))

source("R/model.R")

# ----- One match -------------------------------------------------------------

# Simulate a single match. If knockout = TRUE, a draw is broken by extra time
# (modelled as more expected goals) and then penalties (coin flip).
# Returns list(hg, ag, winner) where winner is "home"/"away" (knockout only).
play_match <- function(elo_h, elo_a, host_h = FALSE, host_a = FALSE,
                       params = default_params(), knockout = FALSE) {
  eg <- elo_to_expected_goals(elo_h, elo_a, host_home = host_h, params = params)
  sc <- sample_scoreline(eg[["home"]], eg[["away"]], rho = params$rho)
  hg <- sc[1]; ag <- sc[2]

  winner <- NA_character_
  if (knockout) {
    if (hg > ag) winner <- "home"
    else if (ag > hg) winner <- "away"
    else {
      # Extra time: a short period with reduced expected goals.
      et <- elo_to_expected_goals(elo_h, elo_a, host_home = host_h, params = params)
      et_sc <- sample_scoreline(et[["home"]] * 0.33, et[["away"]] * 0.33,
                                rho = params$rho, max_goals = 4)
      hg <- hg + et_sc[1]; ag <- ag + et_sc[2]
      if (hg > ag) winner <- "home"
      else if (ag > hg) winner <- "away"
      else winner <- if (runif(1) < 0.5) "home" else "away"  # penalties
    }
  }
  list(hg = hg, ag = ag, winner = winner)
}

# ----- Group stage -----------------------------------------------------------

# Play all matches in one group (round robin) and return the standings.
# teams: character vector of team names in this group.
# elo:   named numeric vector of Elo ratings.
# host:  the host nation name (gets the host bump when playing at "home";
#        at a World Cup all venues are neutral except for the host).
simulate_group <- function(teams, elo, host = NULL, params = default_params()) {
  n <- length(teams)
  tab <- data.frame(team = teams, pts = 0L, gf = 0L, ga = 0L,
                    stringsAsFactors = FALSE)
  add <- function(tab, t, pts, gf, ga) {
    r <- match(t, tab$team)
    tab$pts[r] <- tab$pts[r] + pts; tab$gf[r] <- tab$gf[r] + gf
    tab$ga[r] <- tab$ga[r] + ga; tab
  }
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    a <- teams[i]; b <- teams[j]
    hh <- !is.null(host) && a == host
    ah <- !is.null(host) && b == host
    m <- play_match(elo[[a]], elo[[b]], host_h = hh, host_a = ah, params = params)
    pa <- if (m$hg > m$ag) 3 else if (m$hg == m$ag) 1 else 0
    pb <- if (m$ag > m$hg) 3 else if (m$ag == m$hg) 1 else 0
    tab <- add(tab, a, pa, m$hg, m$ag)
    tab <- add(tab, b, pb, m$ag, m$hg)
  }
  tab$gd <- tab$gf - tab$ga
  # Tiebreakers: points, goal difference, goals scored, then random (a true WC
  # uses head-to-head and fair-play first; random is a reasonable approximation
  # and avoids alphabetical bias. Refine if needed — see HANDOVER.md).
  tab <- tab[order(-tab$pts, -tab$gd, -tab$gf, runif(nrow(tab))), ]
  tab$rank <- seq_len(nrow(tab))
  tab
}

# ----- Knockout bracket ------------------------------------------------------

# Simulate a single-elimination bracket from a seeded list of qualified teams.
# bracket: character vector of team names, ordered so that adjacent pairs meet
#          in round 1 (1v2, 3v4, ...). Length must be a power of 2.
# Returns a list with the champion and the round each team reached.
simulate_knockout <- function(bracket, elo, host = NULL,
                              params = default_params()) {
  reached <- setNames(rep("R32", length(bracket)), bracket)  # default label
  round_names <- c("R32", "R16", "QF", "SF", "F", "W")
  alive <- bracket
  ri <- 1
  while (length(alive) > 1) {
    next_alive <- character(0)
    for (k in seq(1, length(alive), by = 2)) {
      a <- alive[k]; b <- alive[k + 1]
      hh <- !is.null(host) && a == host
      ah <- !is.null(host) && b == host
      m <- play_match(elo[[a]], elo[[b]], host_h = hh, host_a = ah,
                      params = params, knockout = TRUE)
      winner <- if (m$winner == "home") a else b
      next_alive <- c(next_alive, winner)
    }
    ri <- ri + 1
    for (t in next_alive) reached[t] <- round_names[min(ri, length(round_names))]
    alive <- next_alive
  }
  list(champion = alive[1], reached = reached)
}

# ----- Full tournament -------------------------------------------------------

# groups: named list, each element a character vector of 4 team names.
# elo:    named numeric vector of Elo for every team.
# advance_fn: function(group_tables) -> ordered character vector forming the
#             knockout bracket. This encodes the qualification + seeding rules
#             and is format-specific. A 32-team (8 groups, top 2) version is
#             provided; the 48-team version is a STUB (see HANDOVER.md).
simulate_tournament <- function(groups, elo, host = NULL,
                                advance_fn, params = default_params()) {
  group_tables <- lapply(groups, simulate_group, elo = elo, host = host,
                         params = params)
  bracket <- advance_fn(group_tables)
  ko <- simulate_knockout(bracket, elo, host = host, params = params)
  ko
}

# ----- Advancement rule: classic 32-team format ------------------------------
# 8 groups of 4, top 2 advance, standard cross-group bracket pairing
# (1A v 2B, 1C v 2D, ...). Provided and working.
advance_32 <- function(group_tables) {
  gl <- names(group_tables)
  winners <- sapply(group_tables, function(t) t$team[t$rank == 1])
  runners <- sapply(group_tables, function(t) t$team[t$rank == 2])
  names(winners) <- gl; names(runners) <- gl
  # Standard FIFA bracket pairing for 8 groups A-H.
  # R16: 1A-2B, 1C-2D, 1E-2F, 1G-2H, 1B-2A, 1D-2C, 1F-2E, 1H-2G
  pair <- function(w, r) c(winners[[w]], runners[[r]])
  c(pair("A","B"), pair("C","D"), pair("E","F"), pair("G","H"),
    pair("B","A"), pair("D","C"), pair("F","E"), pair("H","G"))
}

# ----- Advancement rule: 48-team 2026 format ---------------------------------
# 12 groups of 4 (A-L); top 2 (24 teams) plus 8 best third-placed = 32 in R32.
# Third-place ranking: points, goal difference, goals scored, then random.
# Bracket: 12 winner-vs-runner-up pairs (cross-group) + 4 third-vs-third pairs.
#
# The cross-group pairing (A<->D, B<->E, C<->F, G<->J, H<->K, I<->L) is an
# approximation. For exact R16/QF path accuracy, substitute the official FIFA
# 2026 bracket lookup table. Overall win probabilities are not sensitive to
# this approximation.
advance_48 <- function(group_tables) {
  gl <- names(group_tables)
  if (length(gl) != 12) {
    stop(sprintf(
      "advance_48 expects 12 groups, got %d. Check groups.csv.", length(gl)
    ))
  }

  get_rank <- function(rank) {
    vapply(group_tables, function(t) t$team[t$rank == rank], character(1))
  }
  winners <- get_rank(1)
  runners  <- get_rank(2)

  # Collect all 12 third-placed rows, rank by points / GD / GF / random.
  thirds_df <- do.call(rbind, lapply(gl, function(g) {
    row <- group_tables[[g]][group_tables[[g]]$rank == 3, ]
    row$group <- g
    row
  }))
  thirds_df <- thirds_df[
    order(-thirds_df$pts, -thirds_df$gd, -thirds_df$gf, runif(nrow(thirds_df))),
  ]
  best8 <- thirds_df$team[seq_len(8)]

  # R32: each winner faces the runner-up from its cross-paired group.
  cross <- c(A="D", B="E", C="F", D="A", E="B", F="C",
             G="J", H="K", I="L", J="G", K="H", L="I")
  w_vs_r <- unlist(
    lapply(gl, function(g) c(winners[[g]], runners[[cross[[g]]]])),
    use.names = FALSE
  )

  # 8 best third-placed fill 4 R32 slots, paired in rank order (1v2, 3v4, ...).
  t_vs_t <- as.vector(matrix(best8, nrow = 2))

  c(w_vs_r, t_vs_t)
}
