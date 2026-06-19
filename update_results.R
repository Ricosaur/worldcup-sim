# update_results.R
# Fetch finished WC 2026 matches from football-data.org and append new rows
# to data/results.csv. Also fetches live Elo ratings from eloratings.net and
# writes them to data/elo_ratings.csv. Safe to run repeatedly.
#
# Requires FOOTBALLDATA_KEY environment variable.

library(httr2)
library(readr)

api_key <- Sys.getenv("FOOTBALLDATA_KEY")
if (!nzchar(api_key)) stop("FOOTBALLDATA_KEY env var not set.")

RESULTS_PATH <- "data/results.csv"
ELO_PATH     <- "data/elo_ratings.csv"
ELO_TSV_URL  <- "https://www.eloratings.net/World.tsv"

# Our team names -> eloratings.net 2-letter codes (verified against en.teams.tsv).
ELO_CODE_MAP <- c(
  "Spain"                  = "ES", "Argentina"              = "AR",
  "France"                 = "FR", "England"                = "EN",
  "Portugal"               = "PT", "Colombia"               = "CO",
  "Brazil"                 = "BR", "Netherlands"            = "NL",
  "Germany"                = "DE", "Norway"                 = "NO",
  "Croatia"                = "HR", "Japan"                  = "JP",
  "Ecuador"                = "EC", "Mexico"                 = "MX",
  "Belgium"                = "BE", "Uruguay"                = "UY",
  "Switzerland"            = "CH", "Senegal"                = "SN",
  "Turkey"                 = "TR", "Morocco"                = "MA",
  "Australia"              = "AU", "Austria"                = "AT",
  "Scotland"               = "SQ", "South Korea"            = "KR",
  "Paraguay"               = "PY", "United States"          = "US",
  "Algeria"                = "DZ", "Canada"                 = "CA",
  "Iran"                   = "IR", "Sweden"                 = "SE",
  "Ivory Coast"            = "CI", "Panama"                 = "PA",
  "Uzbekistan"             = "UZ", "Czechia"                = "CZ",
  "Egypt"                  = "EG", "Jordan"                 = "JO",
  "DR Congo"               = "CD", "Bosnia and Herzegovina" = "BA",
  "Iraq"                   = "IQ", "Cape Verde"             = "CV",
  "Saudi Arabia"           = "SA", "Tunisia"                = "TN",
  "New Zealand"            = "NZ", "Haiti"                  = "HT",
  "South Africa"           = "ZA", "Ghana"                  = "GH",
  "Qatar"                  = "QA", "Curaçao"        = "CW"
)

update_elo_ratings <- function() {
  resp <- tryCatch(
    request(ELO_TSV_URL) |> req_error(is_error = \(r) FALSE) |> req_perform(),
    error = function(e) { message("Cannot reach eloratings.net: ", e$message); NULL }
  )
  if (is.null(resp) || resp_status(resp) != 200) {
    message("Elo update skipped (fetch failed).")
    return(invisible(NULL))
  }

  lines  <- strsplit(resp_body_string(resp), "\n")[[1]]
  lines  <- lines[nzchar(trimws(lines))]
  parsed <- strsplit(lines, "\t")
  # World.tsv layout: rank | rank2 | code | current_rating | ...
  codes   <- vapply(parsed, `[`, character(1), 3)
  ratings <- suppressWarnings(as.integer(vapply(parsed, `[`, character(1), 4)))
  live    <- setNames(ratings, codes)

  existing     <- read_csv(ELO_PATH, show_col_types = FALSE)
  team_codes   <- ELO_CODE_MAP[existing$team]
  new_ratings  <- as.integer(live[team_codes])

  missing <- existing$team[is.na(new_ratings)]
  if (length(missing) > 0) {
    message("No eloratings.net match for: ", paste(missing, collapse = ", "))
    new_ratings[is.na(new_ratings)] <- existing$elo[is.na(new_ratings)]
  }

  existing$elo <- new_ratings
  write_csv(existing, ELO_PATH)
  cat(sprintf("Updated Elo ratings from eloratings.net (%d teams).\n", nrow(existing)))
  invisible(existing)
}

# Team name mapping: football-data.org -> our elo_ratings/groups CSV names.
NAME_MAP <- c(
  "Bosnia-Herzegovina"  = "Bosnia and Herzegovina",
  "Cape Verde Islands"  = "Cape Verde",
  "Congo DR"            = "DR Congo",
  "CuraÃ§ao"           = "Curaçao",
  "Curacao"             = "Curaçao"
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

# ----- Live Elo from eloratings.net ------------------------------------------
update_elo_ratings()
