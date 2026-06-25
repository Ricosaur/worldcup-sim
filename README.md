# 2026 FIFA World Cup Simulator

Monte Carlo simulator for the 2026 FIFA World Cup (48 teams, 12 groups,
June 11 - July 19, USA/Canada/Mexico). Simulates the group stage and knockout
bracket thousands of times and reports each team's probability of winning,
reaching the final, semis, quarterfinals, R16, and advancing from the group.

Live at: [shinyapps.io](https://ricosaur.shinyapps.io/worldcup-sim/)

Match results and Elo ratings are updated automatically three times daily via
GitHub Actions.

## How it works

Team strength is modelled as a global Elo rating (sourced from eloratings.net).
Each match converts the Elo gap between the two teams into expected goals, then
draws a scoreline from a Poisson distribution with Dixon-Coles low-score
correction. See HANDOVER.md for the full design rationale, and `data/ELO_DATA.md`
for data sourcing and attribution.

## App tabs

- **Probabilities** -- win / final / semi / QF / R16 / group advance % for all
  48 teams, with colour bars
- **Top 16** -- bar chart of the 16 most likely champions
- **Standings** -- current group standings computed from results to date
- **Group Breakdown** -- each team's 1st / 2nd / 3rd / 4th finish probability
  per group, from the simulation
- **Head-to-Head** -- pick any two teams; get analytical win / draw / loss % and
  top scoreline probabilities
- **Bracket** -- split left/right visual bracket showing the most likely team
  per slot, with analytical 90-min win probabilities vs the likely opponent

## Running locally

```r
shiny::runApp("app.R")
```

Or headless (prints probabilities to console):

```r
source("run_simulation.R")
```

## File structure

```
app.R                  Shiny front end
run_simulation.R       Headless end-to-end script
update_results.R       Fetches finished WC matches + live Elo; run by Actions

R/model.R              Elo -> expected goals, Dixon-Coles scoring, Elo update
R/tournament.R         Group stage + knockout bracket (advance_48)
R/fetch_data.R         Reads Elo ratings, group draw, results (CSV + GitHub)
R/calibrate.R          Calibrate scoring params against eloratings.net history

data/elo_ratings.csv   Current Elo ratings for all 48 teams (auto-updated)
data/groups.csv        Official 2026 group draw A-L
data/results.csv       WC match results to date (auto-updated)
data/ELO_DATA.md       Data sourcing and attribution

.github/workflows/
  update_results.yml   Runs update_results.R at 08:00, 16:00, 23:00 UTC
```

## Calibration

Scoring parameters (elo_per_goal, base_total, rho) can be tuned against
historical match data:

```r
source("R/calibrate.R")
result <- calibrate()
```

Fetches match histories with Elo at match time from eloratings.net for 20 teams
across all confederations, minimises log-loss over a parameter grid, then
refines with Nelder-Mead. Paste the output into `default_params()` in
`R/model.R`.

## Model parameters

Defined in `default_params()` in `R/model.R`:

| Parameter | Default | Description |
|---|---|---|
| `elo_per_goal` | 245 | Elo points per goal of supremacy |
| `base_total` | 2.6 | Average total goals in a balanced neutral match |
| `host_bump` | 80 | Elo-equivalent home advantage for the host nations |
| `rho` | -0.03 | Dixon-Coles low-score correction |
| `fast` | TRUE | Direct rpois() draws (~10x faster; validated) |

## Data source

Elo ratings and match history: [eloratings.net](https://www.eloratings.net/)
(World Football Elo Ratings, maintained by Kirill Bukin and Erik Gebhardt).
See `data/ELO_DATA.md` for full attribution.
