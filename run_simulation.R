# run_simulation.R
# End-to-end example: load data, run the Monte Carlo, print title probabilities.
# Run with:  Rscript run_simulation.R   (or source() in an R session)
#
# This is the headless core. A Shiny front end (like the Eliteserien app) can be
# layered on top later; see HANDOVER.md.

source("R/model.R")
source("R/tournament.R")
source("R/fetch_data.R")

N_SIMS <- 5000   # raise to 5000+ for final runs

elo     <- read_elo()
groups  <- read_groups()
results <- read_results()

teams <- unlist(groups, use.names = FALSE)
champ_count  <- setNames(integer(length(teams)), teams)
final_count  <- setNames(integer(length(teams)), teams)
sf_count     <- setNames(integer(length(teams)), teams)

reached_rank <- c(R32 = 1, R16 = 2, QF = 3, SF = 4, F = 5, W = 6)

# Group stage is fully played (no remaining variance) and the real R32
# bracket is known, so build it once rather than re-deriving it every
# iteration; see REAL_BRACKET_2026 in R/tournament.R.
elo_sim <- if (!is.null(results) && nrow(results) > 0) {
  adjusted_elo(elo, results, params = default_params())
} else elo
override <- if (real_bracket_2026_valid(groups)) REAL_BRACKET_2026 else NULL
bb      <- build_bracket(groups, elo_sim, hosts = c("United States", "Canada", "Mexico"),
                         advance_fn = advance_48, results_df = results,
                         bracket_override = override)

cat(sprintf("Running %d tournament simulations...\n", N_SIMS))
for (s in seq_len(N_SIMS)) {
  ko <- simulate_knockout(bb$bracket, elo_sim, host = NULL,
                          params = default_params(), known_results = results)
  champ_count[ko$champion] <- champ_count[ko$champion] + 1
  r <- ko$reached
  for (t in names(r)) {
    if (reached_rank[r[t]] >= reached_rank["F"]) final_count[t] <- final_count[t] + 1
    if (reached_rank[r[t]] >= reached_rank["SF"]) sf_count[t] <- sf_count[t] + 1
  }
}

out <- data.frame(
  team = teams,
  win_pct = round(100 * champ_count[teams] / N_SIMS, 1),
  final_pct = round(100 * final_count[teams] / N_SIMS, 1),
  semi_pct = round(100 * sf_count[teams] / N_SIMS, 1)
)
out <- out[order(-out$win_pct), ]
cat("\nTitle / Final / Semi-final probabilities:\n")
print(head(out, 16), row.names = FALSE)
