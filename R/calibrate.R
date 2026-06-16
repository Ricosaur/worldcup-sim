# calibrate.R  (STUB — see HANDOVER.md)
#
# The default scoring parameters in model.R (elo_per_goal, base_total, rho) are
# reasonable starting values, but for trustworthy ABSOLUTE probabilities they
# should be calibrated against historical international results.
#
# Goal: choose params so the model's predicted match outcomes match observed
# frequencies over a large set of past internationals.
#
# Suggested approach (to implement in Claude Code):
#   1. Get a historical international results dataset with each team's Elo at
#      the time of the match (eloratings.net provides both).
#   2. For a grid of (elo_per_goal, base_total, rho), compute the model's
#      predicted P(home win)/P(draw)/P(away win) and expected goals per match.
#   3. Score each parameter set by log-loss against actual results (and compare
#      predicted vs actual goal distributions).
#   4. Pick the params minimising log-loss. Optionally fit base_total as a mild
#      function of total Elo (stronger teams' matches differ slightly).
#
# Validation targets from football reality (rough):
#   - A ~300 Elo favourite wins ~60-70% of matches, draws ~20%.
#   - Average total goals per international is ~2.5-2.8.
#   - These already hold approximately for the defaults (verified by simulation),
#     so calibration is refinement, not a prerequisite to a working demo.

# Not yet implemented -- defaults in model.R are validated and sufficient
# for a working simulator. See HANDOVER.md for the calibration plan.
