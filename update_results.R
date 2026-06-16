# update_results.R
# Fetch finished WC 2026 matches from football-data.org and append new rows
# to data/results.csv. Safe to run repeatedly -- already-recorded matches are
# skipped. Run this once after each matchday, then push to GitHub.
#
# Requires FOOTBALLDATA_KEY environment variable.

library(httr2)
library(readr)

api_key <- Sys.getenv("FOOTBALLDATA_KEY")
if (!nzchar(api_key)) stop("FOOTBALLDATA_KEY env var not set.")

RESULTS_PATH <- "data/results.csv"

# Team name mapping: football-data.org -> our elo_ratings/groups CSV names.
NAME_MAP <- c(
  "Bosnia-Herzegovina"  = "Bosnia and Herzegovina",
  "Cape Verde Islands"  = "Cape Verde",
  "Congo DR"            = "DR Congo"
)

normalise <- function(name) {
  mapped <- NAME_MAP[name]
  ifelse(is.na(mapped), name, mapped)
}

# ----- Fetch finished matches -------------------------------------------------
resp <- request("https://api.football-data.org/v4/competitions/WC/matches") |>
  req_headers(`X-Auth-Token` = api_key) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()

if (resp_status(resp) != 200) {
  stop(sprintf("API error %d: %s", resp_status(resp), resp_body_string(resp)))
}

all_matches <- resp_body_json(resp, simplifyVector = TRUE)$matches
finished    <- all_matches[all_matches$status == "FINISHED", ]
cat(sprintf("Finished matches from API: %d\n", nrow(finished)))

# ----- Build new rows --------------------------------------------------------
new_rows <- data.frame(
  date       = as.Date(substr(finished$utcDate, 1, 10)),
  team_a     = normalise(finished$homeTeam$name),
  team_b     = normalise(finished$awayTeam$name),
  score_a    = finished$score$fullTime$home,
  score_b    = finished$score$fullTime$away,
  importance = "world_cup",
  stringsAsFactors = FALSE
)

# ----- Append only new rows --------------------------------------------------
if (file.exists(RESULTS_PATH)) {
  existing <- read_csv(RESULTS_PATH, show_col_types = FALSE)
  key <- function(d) paste(d$date, d$team_a, d$team_b, sep = "|")
  new_rows <- new_rows[!key(new_rows) %in% key(existing), ]
  cat(sprintf("Already recorded: %d  |  New: %d\n",
              nrow(existing), nrow(new_rows)))
} else {
  if (!dir.exists("data")) dir.create("data")
  cat(sprintf("No existing results file -- writing %d rows fresh.\n",
              nrow(new_rows)))
}

if (nrow(new_rows) == 0) {
  cat("Nothing to add.\n")
} else {
  write_csv(new_rows, RESULTS_PATH, append = file.exists(RESULTS_PATH))
  cat(sprintf("Appended %d new result(s) to %s\n", nrow(new_rows), RESULTS_PATH))
  print(new_rows)
}
