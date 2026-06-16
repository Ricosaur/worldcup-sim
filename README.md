# 2026 FIFA World Cup Simulator

A Monte Carlo simulator for the 2026 FIFA World Cup (USA / Canada / Mexico, June 11 – July 19). Given current team strengths and the official group draw, it runs thousands of simulated tournaments and reports the probability of each team winning, reaching the final, the semi-finals, the quarter-finals, the Round of 16, and advancing from the group stage.

## How it works

Each simulated tournament consists of:

1. **Group stage** — 12 groups of 4 teams play a full round-robin. Match scorelines are drawn from a Poisson model parameterised by each team's Elo rating gap.
2. **Advancement** — the top 2 from each group plus the 8 best third-placed teams (32 teams total) advance to the knockout bracket.
3. **Knockout rounds** — R32 → R16 → QF → SF → Final. Draws in knockout matches go to extra time and then a coin-flip penalty shootout.

Team strength is based on **global Elo ratings** from [eloratings.net](https://www.eloratings.net/), updated through live World Cup match results fetched daily from the [football-data.org](https://www.football-data.org/) API.

See [data/ELO_DATA.md](data/ELO_DATA.md) for full documentation of the Elo rating data and its sources.

## Project structure

```
app.R                  Shiny front end — run locally or deploy to shinyapps.io
run_simulation.R       Standalone script to run simulations from the terminal
update_results.R       Fetches finished World Cup matches and updates data/results.csv
R/
  model.R              Elo → expected goals, Dixon-Coles scoreline, Elo update
  tournament.R         Group stage, knockout bracket, advance_48 (2026 format)
  fetch_data.R         Reads Elo ratings, group draw, and results from CSV / GitHub
  calibrate.R          Stub — calibration plan documented inside
data/
  elo_ratings.csv      Current Elo ratings for all 48 qualified teams
  groups.csv           Official 2026 group draw (A–L)
  results.csv          World Cup match results to date (auto-updated)
  ELO_DATA.md          Documentation for the Elo rating data
.github/workflows/
  update_results.yml   GitHub Actions: fetches new results 3× daily during the tournament
```

## Running locally

```r
# Install dependencies (once)
install.packages(c("shiny", "bslib", "ggplot2", "DT", "dplyr", "readr", "httr2"))

# Launch the app
shiny::runApp("app.R")
```

Use the slider to choose the number of simulations (500–10 000) and click **Run simulation**. Results appear as a sortable table and a bar chart of the top 16 teams.

## Data sources

| Data | Source | Update frequency |
|---|---|---|
| Elo ratings | [eloratings.net](https://www.eloratings.net/) via Kaggle (pre-tournament snapshot, then live WC results) | Rolled forward after each match |
| Group draw | [football-data.org](https://www.football-data.org/) API (`/competitions/WC/matches`) | Fixed after the draw |
| Match results | [football-data.org](https://www.football-data.org/) API | 3× daily via GitHub Actions |

Match results are fetched automatically by a GitHub Actions workflow (`.github/workflows/update_results.yml`) and committed back to `data/results.csv`. The Shiny app reads these directly from GitHub on each simulation run, so deployed instances stay current without any manual intervention.

To run the update script locally, set your API key and run:

```r
Sys.setenv(FOOTBALLDATA_KEY = "your_key_here")
source("update_results.R")
```

A free API key is available at [football-data.org](https://www.football-data.org/).

## Deploying to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "your_account", token = "...", secret = "...")
rsconnect::deployApp()
```

## License

Code: MIT. Elo rating data: CC BY-SA 4.0 — see [data/ELO_DATA.md](data/ELO_DATA.md) for full attribution.
