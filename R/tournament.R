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
  if (isTRUE(params$fast)) {
    hg <- rpois(1, eg[["home"]]); ag <- rpois(1, eg[["away"]])
  } else {
    sc <- sample_scoreline(eg[["home"]], eg[["away"]], rho = params$rho)
    hg <- sc[1]; ag <- sc[2]
  }

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

# 2026 WC tiebreaker order (after overall points):
#   1. H2H points among tied teams
#   2. H2H goal difference among tied teams
#   3. H2H goals for among tied teams
#   4. Overall goal difference
#   5. Overall goals for
#   6. Random (approximates drawing of lots / fair play)
.rank_group <- function(idx, tab, h2h_pts, h2h_gf, h2h_ga) {
  idx <- idx[order(-tab$pts[idx])]
  n   <- length(idx)
  result <- integer(0)
  i <- 1
  while (i <= n) {
    j <- i
    while (j < n && tab$pts[idx[j + 1]] == tab$pts[idx[i]]) j <- j + 1
    if (j == i) {
      result <- c(result, idx[i])
    } else {
      tied    <- idx[i:j]
      m       <- length(tied)
      h2h_p   <- rowSums(h2h_pts[tied, tied, drop = FALSE])
      h2h_gd  <- rowSums(h2h_gf[tied, tied, drop = FALSE]) -
                 rowSums(h2h_ga[tied, tied, drop = FALSE])
      h2h_gfs <- rowSums(h2h_gf[tied, tied, drop = FALSE])
      sub_ord <- order(-h2h_p, -h2h_gd, -h2h_gfs,
                       -tab$gd[tied], -tab$gf[tied], runif(m))
      result  <- c(result, tied[sub_ord])
    }
    i <- j + 1
  }
  result
}

# Play all matches in one group (round robin) and return the standings.
# teams: character vector of team names in this group.
# elo:   named numeric vector of Elo ratings.
# host:  the host nation name (gets the host bump when playing at "home";
#        at a World Cup all venues are neutral except for the host).
simulate_group <- function(teams, elo, host = NULL, known_results = NULL,
                           params = default_params()) {
  n   <- length(teams)
  tab <- data.frame(team = teams, pts = 0L, gf = 0L, ga = 0L,
                    stringsAsFactors = FALSE)
  h2h_pts <- matrix(0L, n, n)
  h2h_gf  <- matrix(0L, n, n)
  h2h_ga  <- matrix(0L, n, n)

  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    a <- teams[i]; b <- teams[j]

    # Use actual result if available, otherwise simulate
    played <- if (!is.null(known_results) && nrow(known_results) > 0)
      known_results[
        (known_results$team_a == a & known_results$team_b == b) |
        (known_results$team_a == b & known_results$team_b == a), ]
    else NULL

    if (!is.null(played) && nrow(played) > 0) {
      r  <- played[1, ]
      hg <- if (r$team_a == a) r$score_a else r$score_b
      ag <- if (r$team_a == a) r$score_b else r$score_a
    } else {
      hh <- !is.null(host) && a == host
      ah <- !is.null(host) && b == host
      m  <- play_match(elo[[a]], elo[[b]], host_h = hh, host_a = ah, params = params)
      hg <- m$hg; ag <- m$ag
    }

    pa <- if (hg > ag) 3L else if (hg == ag) 1L else 0L
    pb <- if (ag > hg) 3L else if (ag == hg) 1L else 0L
    tab$pts[i] <- tab$pts[i] + pa; tab$gf[i] <- tab$gf[i] + hg; tab$ga[i] <- tab$ga[i] + ag
    tab$pts[j] <- tab$pts[j] + pb; tab$gf[j] <- tab$gf[j] + ag; tab$ga[j] <- tab$ga[j] + hg
    h2h_pts[i, j] <- pa; h2h_pts[j, i] <- pb
    h2h_gf[i, j]  <- hg; h2h_gf[j, i] <- ag
    h2h_ga[i, j]  <- ag; h2h_ga[j, i] <- hg
  }
  tab$gd      <- tab$gf - tab$ga
  final_order <- .rank_group(seq_len(n), tab, h2h_pts, h2h_gf, h2h_ga)
  tab         <- tab[final_order, ]
  tab$rank    <- seq_len(n)
  tab
}

# ----- Knockout bracket ------------------------------------------------------

# Simulate a single-elimination bracket from a seeded list of qualified teams.
# bracket: character vector of team names, ordered so that adjacent pairs meet
#          in round 1 (1v2, 3v4, ...). Length must be a power of 2.
# Returns a list with the champion and the round each team reached.
simulate_knockout <- function(bracket, elo, host = NULL,
                              params = default_params()) {
  reached      <- setNames(rep("R32", length(bracket)), bracket)
  round_names  <- c("R32", "R16", "QF", "SF", "F", "W")
  alive        <- bracket
  ri           <- 1
  slot_history <- list(alive)   # [[1]] = 32 R32 entrants in bracket order
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
    slot_history[[ri]] <- next_alive
    alive <- next_alive
  }
  list(champion = alive[1], reached = reached, slot_history = slot_history)
}

# ----- Full tournament -------------------------------------------------------

# groups: named list, each element a character vector of 4 team names.
# elo:    named numeric vector of Elo for every team.
# advance_fn: function(group_tables) -> ordered character vector forming the
#             knockout bracket. This encodes the qualification + seeding rules
#             and is format-specific. A 32-team (8 groups, top 2) version is
#             provided; the 48-team version is a STUB (see HANDOVER.md).
simulate_tournament <- function(groups, elo, hosts = character(0),
                                advance_fn, params = default_params(),
                                results_df = NULL) {
  # Adjust Elo based on observed tournament performance before simulating.
  elo_sim <- if (!is.null(results_df) && nrow(results_df) > 0)
    adjusted_elo(elo, results_df, params = params)
  else elo

  group_tables <- lapply(names(groups), function(g) {
    gteams <- groups[[g]]
    host_g <- intersect(gteams, hosts)
    known  <- if (!is.null(results_df) && nrow(results_df) > 0)
      results_df[results_df$team_a %in% gteams & results_df$team_b %in% gteams, ]
    else NULL
    simulate_group(gteams, elo = elo_sim,
                   host          = if (length(host_g)) host_g[1] else NULL,
                   known_results = known,
                   params        = params)
  })
  names(group_tables) <- names(groups)
  bracket <- advance_fn(group_tables)
  ko      <- simulate_knockout(bracket, elo_sim, host = NULL, params = params)
  list(champion = ko$champion, reached = ko$reached,
       group_tables = group_tables, slot_history = ko$slot_history)
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
#
# Official FIFA 2026 bracket (verified from fifa.com match schedule and
# wikipedia.org/wiki/2026_FIFA_World_Cup_knockout_stage):
#
# R32 matches and their R16/QF/SF progression:
#   M74: 1E v 3rd  M77: 1I v 3rd  -> R16 M89  -> QF M97
#   M73: 2A v 2B   M75: 1F v 2C   -> R16 M90  -> QF M97
#   M83: 2K v 2L   M84: 1H v 2J   -> R16 M93  -> QF M98  -> SF M101
#   M81: 1D v 3rd  M82: 1G v 3rd  -> R16 M94  -> QF M98
#   M76: 1C v 2F   M78: 2E v 2I   -> R16 M91  -> QF M99
#   M79: 1A v 3rd  M80: 1L v 3rd  -> R16 M92  -> QF M99  -> SF M102 -> Final
#   M86: 1J v 2H   M88: 2D v 2G   -> R16 M95  -> QF M100
#   M85: 1B v 3rd  M87: 1K v 3rd  -> R16 M96  -> QF M100
#
# Third-place slots: each slot (named by the group winner it faces) may only
# be filled by a third from the listed groups. Assigned by greedy bipartite
# matching (most-constrained slot first). This correctly handles all 495
# possible qualifying combinations; the exact assignment approximates FIFA's
# Annex-C table but gives a valid bracket for every combination.

.THIRD_SLOT_ALLOWED <- list(
  "E" = c("A","B","C","D","F"),
  "I" = c("C","D","F","G","H"),
  "A" = c("C","E","F","H","I"),
  "L" = c("E","H","I","J","K"),
  "D" = c("B","E","F","I","J"),
  "G" = c("A","E","H","I","J"),
  "B" = c("E","F","G","I","J"),
  "K" = c("D","E","I","J","L")
)

.assign_thirds <- function(qualifying_groups) {
  slots <- names(.THIRD_SLOT_ALLOWED)

  backtrack <- function(remaining, done) {
    if (length(done) == length(slots)) return(done)
    todo    <- setdiff(slots, names(done))
    n_avail <- vapply(todo, function(s)
      sum(.THIRD_SLOT_ALLOWED[[s]] %in% remaining), integer(1))
    todo    <- todo[order(n_avail, todo)]   # most-constrained first, alpha tiebreak
    slot    <- todo[1]
    eligible <- sort(intersect(.THIRD_SLOT_ALLOWED[[slot]], remaining))
    for (g in eligible) {
      result <- backtrack(remaining[remaining != g],
                          c(done, setNames(g, slot)))
      if (!is.null(result)) return(result)
    }
    NULL  # dead end; caller will try next candidate
  }

  result <- backtrack(qualifying_groups, character(0))
  if (is.null(result))
    stop("advance_48: no valid third-place assignment for groups: ",
         paste(qualifying_groups, collapse = ","))
  result[slots]
}

advance_48 <- function(group_tables) {
  gl <- names(group_tables)
  if (length(gl) != 12)
    stop(sprintf("advance_48 expects 12 groups, got %d. Check groups.csv.", length(gl)))

  get_rank <- function(rank)
    vapply(group_tables, function(t) t$team[t$rank == rank], character(1))

  winners <- get_rank(1)
  runners  <- get_rank(2)

  thirds_df <- do.call(rbind, lapply(gl, function(g) {
    row <- group_tables[[g]][group_tables[[g]]$rank == 3, ]
    row$group <- g; row
  }))
  thirds_df <- thirds_df[
    order(-thirds_df$pts, -thirds_df$gd, -thirds_df$gf, runif(nrow(thirds_df))), ]
  best8      <- thirds_df[seq_len(8), ]
  best8_team <- setNames(best8$team, best8$group)

  slot_grp <- .assign_thirds(best8$group)
  t3 <- function(g) unname(best8_team[unname(slot_grp[g])])

  # Bracket vector in simulate_knockout order (adjacent pairs meet each round).
  # Pairs map to R32 matches: M74, M77, M73, M75 | M83, M84, M81, M82 |
  #                           M76, M78, M79, M80 | M86, M88, M85, M87
  c(
    winners[["E"]], t3("E"),          # M74: 1E vs 3rd
    winners[["I"]], t3("I"),          # M77: 1I vs 3rd
    runners[["A"]], runners[["B"]],   # M73: 2A vs 2B
    winners[["F"]], runners[["C"]],   # M75: 1F vs 2C
    runners[["K"]], runners[["L"]],   # M83: 2K vs 2L
    winners[["H"]], runners[["J"]],   # M84: 1H vs 2J
    winners[["D"]], t3("D"),          # M81: 1D vs 3rd
    winners[["G"]], t3("G"),          # M82: 1G vs 3rd
    winners[["C"]], runners[["F"]],   # M76: 1C vs 2F
    runners[["E"]], runners[["I"]],   # M78: 2E vs 2I
    winners[["A"]], t3("A"),          # M79: 1A vs 3rd
    winners[["L"]], t3("L"),          # M80: 1L vs 3rd
    winners[["J"]], runners[["H"]],   # M86: 1J vs 2H
    runners[["D"]], runners[["G"]],   # M88: 2D vs 2G
    winners[["B"]], t3("B"),          # M85: 1B vs 3rd
    winners[["K"]], t3("K")           # M87: 1K vs 3rd
  )
}
