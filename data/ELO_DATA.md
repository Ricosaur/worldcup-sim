# Elo Rating Data

`elo_ratings.csv` contains the current Elo rating for each of the 48 teams qualified for the 2026 FIFA World Cup. Schema: `team, elo`.

The pre-tournament snapshot was sourced from the Kaggle dataset [2026 FIFA World Cup — Historical Elo Ratings](https://www.kaggle.com/datasets/richardrekdal/2026-fifa-world-cup-historical-elo-ratings), which is itself scraped from [eloratings.net](https://www.eloratings.net/) (World Football Elo Ratings, maintained by Kirill Bukin and Erik Gebhardt). During the tournament, ratings are rolled forward after each match using the standard Elo update implemented in `R/model.R`.

## How Elo works for football

Elo is a paired-comparison rating system. After each match, each team's rating updates by:

```
new_rating = old_rating + K * G * (W - W_e)
```

where `K` is a base weight (scaled by match importance), `G` adjusts for goal difference, `W` is the actual result (1 / 0.5 / 0), and `W_e` is the win expectancy given the pre-match rating gap. The eloratings.net variant has been published continuously since the early 1990s; historical ratings are backfilled to 1872.

Useful rules of thumb:
- A **100-point gap** → ~64% win expectancy for the higher-rated team at a neutral venue.
- A **200-point gap** → ~76%. A **400-point gap** → ~91%.
- Host advantage is valued at roughly **+100 points** in this variant (configurable in `R/model.R`).

## Why Elo rather than a from-scratch Dixon-Coles model?

International football teams play in separate confederation pools (UEFA, CONMEBOL, CAF, etc.) with very few cross-confederation matches outside of World Cups. A model fit from scratch on recent results cannot reliably compare, say, a European qualifier to a CONMEBOL qualifier because they rarely meet. Elo solves this by maintaining a single global rating updated continuously since the 1870s — cross-confederation comparisons are encoded from decades of World Cup and friendly results.

## Confederation breakdown (48 qualified teams)

| Confederation | Count | Teams |
|---|---|---|
| Hosts | 3 | United States, Canada, Mexico |
| UEFA | 16 | England, France, Croatia, Portugal, Norway, Germany, Netherlands, Switzerland, Scotland, Spain, Austria, Belgium, Bosnia and Herzegovina, Sweden, Turkey, Czechia |
| CONMEBOL | 6 | Argentina, Brazil, Ecuador, Paraguay, Uruguay, Colombia |
| CONCACAF (non-host) | 3 | Panama, Curaçao, Haiti |
| CAF | 9 | Morocco, Tunisia, Egypt, Algeria, Ghana, Cape Verde, Senegal, South Africa, Ivory Coast |
| AFC | 8 | Japan, Iran, Uzbekistan, Jordan, South Korea, Australia, Qatar, Saudi Arabia |
| OFC | 1 | New Zealand |
| Inter-confederation playoff | 2 | DR Congo, Iraq |

## Attribution

- **Original source:** [World Football Elo Ratings](https://www.eloratings.net/) (eloratings.net).
- **Kaggle dataset:** [2026 FIFA World Cup — Historical Elo Ratings](https://www.kaggle.com/datasets/richardrekdal/2026-fifa-world-cup-historical-elo-ratings), published under CC BY-SA 4.0.
- Please credit both the Kaggle dataset and the upstream eloratings.net source if you reuse this data.
