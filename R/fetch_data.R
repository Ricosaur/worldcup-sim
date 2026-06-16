# fetch_data.R
# Data layer for the World Cup simulator.
#
# Two inputs are needed:
#   1. Current Elo ratings for every participating team (the strength backbone).
#   2. The group draw (which teams are in which group), plus the host.
#
# Optionally, a results file (qualifiers + friendlies) to roll Elo forward.
#
# SOURCES (see HANDOVER.md):
#   - eloratings.net publishes world football Elo and full international match
#     histories. It is the recommended source (far better for prediction than
#     FIFA's own ranking). Its data is downloadable; no fragile scraping needed
#     for the ratings snapshot.
#   - The group draw is published by FIFA once the draw is made; until then you
#     can simulate from a provisional/pot-based draw or a mock draw.
#
# Like the Eliteserien project, the canonical inputs live as CSVs in data/ and
# (when deployed) are read from GitHub. No runtime scraping.

suppressWarnings(suppressMessages({ library(dplyr); library(readr) }))

CACHE_URL_ELO    <- "https://raw.githubusercontent.com/ricosaur/worldcup-sim/main/data/elo_ratings.csv"
CACHE_URL_GROUPS <- "https://raw.githubusercontent.com/ricosaur/worldcup-sim/main/data/groups.csv"

# ----- Elo ratings -----------------------------------------------------------
# elo_ratings.csv schema: team, elo
read_elo <- function(path = "data/elo_ratings.csv", url = CACHE_URL_ELO) {
  df <- tryCatch(readr::read_csv(url, show_col_types = FALSE),
                 error = function(e) NULL)
  if (is.null(df) && file.exists(path)) {
    df <- readr::read_csv(path, show_col_types = FALSE)
  }
  if (is.null(df)) stop("Could not read Elo ratings.")
  setNames(as.numeric(df$elo), as.character(df$team))
}

# ----- Group draw ------------------------------------------------------------
# groups.csv schema: group, team   (group is a letter A, B, C, ...)
# Returns a named list: $A = c(team1,...), $B = c(...), ...
read_groups <- function(path = "data/groups.csv", url = CACHE_URL_GROUPS) {
  df <- tryCatch(readr::read_csv(url, show_col_types = FALSE),
                 error = function(e) NULL)
  if (is.null(df) && file.exists(path)) {
    df <- readr::read_csv(path, show_col_types = FALSE)
  }
  if (is.null(df)) stop("Could not read group draw.")
  split(as.character(df$team), as.character(df$group))
}

# ----- Rolling Elo forward through results (optional) ------------------------
# results.csv schema: date, team_a, team_b, score_a, score_b, importance
# importance matches a key in model.R's IMPORTANCE vector.
# Applies sequential Elo updates so the ratings reflect recent form.
roll_elo_through_results <- function(elo, results_path = "data/results.csv") {
  if (!file.exists(results_path)) return(elo)
  res <- readr::read_csv(results_path, show_col_types = FALSE) %>%
    arrange(as.Date(date))
  for (k in seq_len(nrow(res))) {
    a <- res$team_a[k]; b <- res$team_b[k]
    if (is.null(elo[[a]]) || is.null(elo[[b]])) next
    imp <- IMPORTANCE[[res$importance[k]]]
    if (is.null(imp) || is.na(imp)) imp <- 1
    u <- elo_update(elo[[a]], elo[[b]], res$score_a[k], res$score_b[k],
                    importance = imp)
    elo[[a]] <- u[["a"]]; elo[[b]] <- u[["b"]]
  }
  elo
}
