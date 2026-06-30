# app.R
# 2026 FIFA World Cup Monte Carlo simulator -- Shiny front end.
# Deploy to shinyapps.io with: rsconnect::deployApp()

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

source("R/model.R")
source("R/tournament.R")
source("R/fetch_data.R")

REACHED_RANK <- c(R32 = 1, R16 = 2, QF = 3, SF = 4, F = 5, W = 6)

# ----- UI --------------------------------------------------------------------

ui <- page_sidebar(
  title = "2026 FIFA World Cup Simulator",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),

  sidebar = sidebar(
    width = 260,
    open = list(desktop = "open", mobile = "closed"),
    sliderInput("n_sims", "Simulations",
                min = 500, max = 10000, value = 2000, step = 500),
    actionButton("run", "Run simulation",
                 class = "btn-primary w-100 mt-1"),
    hr(),
    uiOutput("last_updated"),
    helpText(
      "Elo ratings from eloratings.net (live).",
      "Match results via football-data.org API,",
      "updated 3x daily by GitHub Actions."
    )
  ),

  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Bracket",
      uiOutput("bracket_ui")
    ),
    nav_panel(
      "Team",
      div(style = "max-width: 280px; margin-bottom: 14px;",
        selectInput("selected_team", "Select team", choices = NULL)
      ),
      uiOutput("team_content_ui")
    ),
    nav_panel(
      "Probabilities",
      DTOutput("prob_table")
    ),
    nav_panel(
      "Form",
      div(class = "text-muted small", style = "margin-bottom: 8px;",
        "Expected points = 3 × P(win) + P(draw) based on current Elo. ",
        "Positive = outperforming; negative = underperforming. ",
        "Dead-rubber matches (already-qualified teams rotating squads) can inflate scores."),
      DTOutput("form_table")
    ),
    nav_panel(
      "Standings",
      uiOutput("standings_ui")
    ),
    nav_panel(
      "Head-to-Head",
      card(
        card_header("Select teams"),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("h2h_a", "Team A", choices = NULL),
          selectInput("h2h_b", "Team B", choices = NULL),
          div(
            checkboxInput("h2h_home", "Team A has home advantage", value = FALSE),
            actionButton("h2h_run", "Calculate odds",
                         class = "btn-primary w-100 mt-2")
          )
        )
      ),
      uiOutput("h2h_boxes"),
      conditionalPanel(
        "input.h2h_run > 0",
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Expected goals"),
            uiOutput("h2h_xg")
          ),
          card(
            card_header("Most likely scorelines"),
            DTOutput("h2h_scores")
          )
        )
      )
    )
  )
)

# ----- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  sim_has_run <- reactiveVal(FALSE)

  sim_notice <- function() {
    div(style = "padding: 60px; text-align: center; color: #6c757d;",
      p(style = "font-size: 1.1em; margin-bottom: 6px;",
        "Run the simulation to see results."),
      p(class = "small",
        "Set the number of simulations in the panel on the left, then click ",
        strong("Run simulation"), ".")
    )
  }

  elo_base <- reactive({
    read_elo()
  })

  groups_data <- reactive({ read_groups() })

  observe({
    teams <- sort(names(elo_base()))
    updateSelectInput(session, "h2h_a", choices = teams, selected = teams[1])
    updateSelectInput(session, "h2h_b", choices = teams, selected = teams[2])
  })

  # After sim runs, default H2H to the two most likely finalists.
  observeEvent(sim_data(), {
    finalists <- sim_data()$slot_bests[[5]]
    if (length(finalists) == 2 && !any(finalists == "?")) {
      updateSelectInput(session, "h2h_a", selected = finalists[1])
      updateSelectInput(session, "h2h_b", selected = finalists[2])
    }
  })

  # ----- Last updated ----------------------------------------------------------

  results_data <- reactive({
    res <- tryCatch(readr::read_csv(cache_bust(CACHE_URL_RESULTS), show_col_types = FALSE),
                   error = \(e) NULL)
    if (is.null(res) && file.exists("data/results.csv"))
      res <- readr::read_csv("data/results.csv", show_col_types = FALSE)
    res
  })

  output$last_updated <- renderUI({
    res <- results_data()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    last <- max(as.Date(res$date), na.rm = TRUE)
    helpText(paste("Results through:", format(last, "%d %b %Y")))
  })

  # ----- Group standings -------------------------------------------------------

  standings_tab <- reactive({
    res    <- results_data()
    groups <- groups_data()
    if (is.null(res) || nrow(res) == 0) return(NULL)

    team_group <- setNames(
      rep(names(groups), lengths(groups)),
      unlist(groups, use.names = FALSE)
    )
    pts_map <- function(gf, ga) ifelse(gf > ga, 3L, ifelse(gf == ga, 1L, 0L))

    tab <- data.frame(
      team = unlist(groups, use.names = FALSE),
      pts = 0L, gf = 0L, ga = 0L,
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(res))) {
      r    <- res[i, ]
      a_i  <- match(r$team_a, tab$team)
      b_i  <- match(r$team_b, tab$team)
      if (!is.na(a_i)) {
        tab$pts[a_i] <- tab$pts[a_i] + pts_map(r$score_a, r$score_b)
        tab$gf[a_i]  <- tab$gf[a_i]  + r$score_a
        tab$ga[a_i]  <- tab$ga[a_i]  + r$score_b
      }
      if (!is.na(b_i)) {
        tab$pts[b_i] <- tab$pts[b_i] + pts_map(r$score_b, r$score_a)
        tab$gf[b_i]  <- tab$gf[b_i]  + r$score_b
        tab$ga[b_i]  <- tab$ga[b_i]  + r$score_a
      }
    }
    tab$gd    <- tab$gf - tab$ga
    tab$group <- team_group[tab$team]
    tab[order(tab$group, -tab$pts, -tab$gd, -tab$gf), ]
  })

  output$standings_ui <- renderUI({
    groups <- groups_data()
    cards <- lapply(sort(names(groups)), function(g) {
      card(card_header(paste("Group", g)),
           tableOutput(paste0("std_", g)))
    })
    if (is.null(standings_tab()))
      return(p("No results recorded yet."))
    do.call(layout_columns, c(list(col_widths = rep(4, 12)), cards))
  })

  for (g in LETTERS[1:12]) {
    local({
      grp <- g
      output[[paste0("std_", grp)]] <- renderTable({
        tab <- standings_tab()
        if (is.null(tab)) return(NULL)
        df <- tab[!is.na(tab$group) & tab$group == grp,
                  c("team", "pts", "gf", "ga", "gd")]
        names(df) <- c("Team", "Pts", "GF", "GA", "GD")
        df
      }, striped = TRUE, hover = TRUE, align = "lrrrr", rownames = FALSE)
    })
  }

  # ----- Form (overperformance index) -----------------------------------------

  form_data <- reactive({
    res    <- results_data()
    elo    <- elo_base()
    groups <- groups_data()
    if (is.null(res) || nrow(res) == 0) return(NULL)

    team_group <- setNames(rep(names(groups), lengths(groups)),
                           unlist(groups, use.names = FALSE))
    teams <- names(team_group)

    exp_pts  <- setNames(numeric(length(teams)), teams)
    act_pts  <- setNames(numeric(length(teams)), teams)
    n_played <- setNames(integer(length(teams)), teams)

    for (i in seq_len(nrow(res))) {
      r <- res[i, ]
      a <- r$team_a; b <- r$team_b
      if (!(a %in% teams) || !(b %in% teams)) next
      if (is.na(r$score_a) || is.na(r$score_b)) next
      pr   <- match_probabilities(elo[[a]], elo[[b]])
      pa   <- if (r$score_a > r$score_b) 3L else if (r$score_a == r$score_b) 1L else 0L
      pb   <- if (r$score_b > r$score_a) 3L else if (r$score_b == r$score_a) 1L else 0L
      exp_pts[a]  <- exp_pts[a]  + 3 * pr$win_a + pr$draw
      exp_pts[b]  <- exp_pts[b]  + 3 * pr$win_b + pr$draw
      act_pts[a]  <- act_pts[a]  + pa
      act_pts[b]  <- act_pts[b]  + pb
      n_played[a] <- n_played[a] + 1L
      n_played[b] <- n_played[b] + 1L
    }

    overperf  <- act_pts - exp_pts
    per_match <- ifelse(n_played > 0, overperf / n_played, 0)

    adj       <- adjusted_elo(elo, res)
    delta_elo <- round(adj[teams] - elo[teams])

    df <- data.frame(
      Team        = teams,
      Group       = team_group[teams],
      `Raw Elo`   = round(elo[teams]),
      `Adj. Elo`  = round(adj[teams]),
      `Δ Elo`     = delta_elo,
      Played      = n_played[teams],
      `Exp Pts`   = round(exp_pts[teams], 1),
      `Act Pts`   = act_pts[teams],
      `+/-`       = round(overperf[teams], 1),
      `Per Match` = round(per_match[teams], 2),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    df[order(-df$`+/-`), ]
  })

  output$form_table <- renderDT({
    df <- form_data()
    if (is.null(df)) return(datatable(data.frame(Message = "No results recorded yet."),
                                      rownames = FALSE))
    max_abs <- max(abs(df$`+/-`), 0.1)
    datatable(
      df,
      rownames  = FALSE,
      selection = "none",
      options   = list(pageLength = 48, dom = "ft")
    ) |>
      formatStyle("+/-",
        backgroundColor = styleInterval(
          c(-1.5, -0.5, 0.5, 1.5),
          c("#f5c6cb", "#fde8cc", "#fff9c4", "#d4edda", "#b8ddb8")
        ),
        fontWeight = "bold"
      ) |>
      formatStyle("Per Match",
        color = styleInterval(0, c("#c0392b", "#27ae60")),
        fontWeight = "bold"
      ) |>
      formatStyle("Δ Elo",
        color = styleInterval(0, c("#c0392b", "#27ae60")),
        fontWeight = "bold"
      )
  })

  sim_data <- eventReactive(input$run, {
    sim_has_run(TRUE)
    elo     <- elo_base()
    groups  <- groups_data()
    results <- results_data()

    teams      <- unlist(groups, use.names = FALSE)
    champ_n    <- setNames(integer(length(teams)), teams)
    final_n    <- setNames(integer(length(teams)), teams)
    semi_n     <- setNames(integer(length(teams)), teams)
    qf_n       <- setNames(integer(length(teams)), teams)
    r16_n      <- setNames(integer(length(teams)), teams)
    r32_n      <- setNames(integer(length(teams)), teams)
    n          <- input$n_sims

    # Bracket slot counters: slot_mats[[round]][team, slot] = appearances
    n_slots_r  <- c(32L, 16L, 8L, 4L, 2L, 1L)
    slot_mats  <- lapply(n_slots_r, function(ns)
      matrix(0L, nrow = length(teams), ncol = ns, dimnames = list(teams, NULL))
    )

    # The group stage is fully played (no remaining variance) and the real R32
    # bracket is known, so build it ONCE here rather than re-deriving it every
    # Monte Carlo iteration. Falls back to the heuristic advance_48() if the
    # hardcoded bracket ever stops matching this tournament's data.
    elo_sim <- if (!is.null(results) && nrow(results) > 0)
      adjusted_elo(elo, results, params = default_params())
    else elo
    override <- if (real_bracket_2026_valid(groups)) REAL_BRACKET_2026 else NULL
    bb <- build_bracket(groups, elo_sim, hosts = c("United States", "Canada", "Mexico"),
                        advance_fn = advance_48, results_df = results,
                        bracket_override = override)
    bracket <- bb$bracket

    # Deterministic most-likely-path projection, used for the bracket plot
    # instead of Monte Carlo modal slots: guarantees the displayed winner's
    # probability is never below 50%, and flags matches already played in
    # reality so they can be shown as confirmed rather than projected.
    proj <- project_bracket(bracket, elo_sim, known_results = results,
                            params = default_params())

    withProgress(message = "Simulating...", value = 0, {
      for (s in seq_len(n)) {
        ko <- simulate_knockout(bracket, elo_sim, host = NULL,
                                params = default_params(), known_results = results)
        champ_n[ko$champion] <- champ_n[ko$champion] + 1
        for (t in names(ko$reached)) {
          rv <- REACHED_RANK[ko$reached[t]]
          r32_n[t] <- r32_n[t] + 1
          if (rv >= REACHED_RANK["R16"]) r16_n[t]  <- r16_n[t] + 1
          if (rv >= REACHED_RANK["QF"])  qf_n[t]   <- qf_n[t]  + 1
          if (rv >= REACHED_RANK["SF"])  semi_n[t] <- semi_n[t] + 1
          if (rv >= REACHED_RANK["F"])   final_n[t]<- final_n[t]+ 1
        }
        for (r in seq_along(ko$slot_history)) {
          sh_r <- ko$slot_history[[r]]
          for (sl in seq_along(sh_r)) {
            tm <- sh_r[sl]
            if (tm %in% teams)
              slot_mats[[r]][tm, sl] <- slot_mats[[r]][tm, sl] + 1L
          }
        }
        if (s %% 100 == 0) setProgress(s / n)
      }
    })

    summary <- data.frame(
      Team       = teams,
      `Win %`    = round(100 * champ_n[teams] / n, 1),
      `Final %`  = round(100 * final_n[teams] / n, 1),
      `Semi %`   = round(100 * semi_n[teams]  / n, 1),
      `QF %`     = round(100 * qf_n[teams]    / n, 1),
      `R16 %`    = round(100 * r16_n[teams]   / n, 1),
      `Group %`  = round(100 * r32_n[teams]   / n, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # ----- Split bracket visualisation data ----------------------------------
    # Left half (slots 1..n_per_half each round) progresses left → right.
    # Right half (next n_per_half slots) progresses right → left.
    # Both halves share y = 1..16. Champion shown at centre-top in gold.
    x_L <- c(0, 1.8, 3.2, 5.0, 6.2)
    x_R <- c(14.0, 12.2, 10.8, 9.0, 7.8)
    x_champ <- 7.0; y_final <- 8.5; y_champ_pos <- 10.6

    n_per_half <- c(16L, 8L, 4L, 2L, 1L)
    half_y <- local({
      y  <- seq(16L, 1L)
      ys <- list(y)
      while (length(y) > 1) {
        y  <- (y[seq(1, length(y), 2)] + y[seq(2, length(y), 2)]) / 2
        ys <- c(ys, list(y))
      }
      ys
    })

    abbrv <- function(nm) ifelse(nchar(nm) > 11, paste0(substr(nm, 1, 10), "."), nm)

    # Most likely team per slot (by simulation frequency), deduplicated so
    # each team appears at most once per round. Greedy by descending count.
    slot_bests <- lapply(1:5, function(r) {
      mat     <- slot_mats[[r]]
      n_slots <- ncol(mat)
      result  <- rep("?", n_slots)
      entries <- do.call(rbind, lapply(seq_len(n_slots), function(ci) {
        col <- mat[, ci]
        if (sum(col) == 0) return(NULL)
        ord <- order(-col)
        data.frame(slot  = ci,
                   team  = rownames(mat)[ord],
                   count = col[ord],
                   stringsAsFactors = FALSE)
      }))
      if (is.null(entries) || nrow(entries) == 0) return(result)
      entries <- entries[order(-entries$count), ]
      placed  <- character(0)
      for (i in seq_len(nrow(entries))) {
        sl <- entries$slot[i]; tm <- entries$team[i]
        if (result[sl] == "?" && !tm %in% placed) {
          result[sl] <- tm
          placed <- c(placed, tm)
        }
      }
      result
    })

    # Box colour: confirmed (already-played) matches get a fixed win/loss
    # colour; projected (not yet played) matches keep the blue gradient by
    # win probability. Projected winners are always the higher-probability
    # side (see project_bracket()), so a projected box is never shown with a
    # sub-50% probability.
    proj_color <- function(prob, confirmed, won) {
      if (confirmed) return(if (won) "#4daf4a" else "#e8a3a3")
      rgb_vals <- grDevices::colorRamp(c("white", "#2c7bb6"))(prob)
      grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
    }

    bdf_rows <- list()
    for (r in 1:5) {
      nh  <- n_per_half[r]
      occ <- proj$occupants[[r]]
      mt  <- proj$matches[[r]]
      for (side in c("L", "R")) {
        sl_off <- if (side == "L") 0L else nh
        x_val  <- if (side == "L") x_L[r] else x_R[r]
        for (sl in seq_len(nh)) {
          col_idx <- sl + sl_off
          team    <- occ[col_idx]
          mrow    <- mt[(col_idx + 1) %/% 2, ]
          won     <- team == mrow$winner
          prob    <- if (won) mrow$prob else 1 - mrow$prob
          label   <- if (mrow$confirmed) {
            paste0(abbrv(team), if (won) " ✓" else " ✗")
          } else {
            paste0(abbrv(team), " ", round(prob * 100), "%")
          }
          bdf_rows[[length(bdf_rows) + 1]] <- data.frame(
            half = side, round = r, slot = sl,
            x = x_val, y = half_y[[r]][sl],
            team = team, label = label,
            fill_color = proj_color(prob, mrow$confirmed, won),
            confirmed = mrow$confirmed, won = won,
            fsize = c(4.2, 5.2, 5.4, 6.4, 7.0)[r],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    {
      champ       <- proj$champion
      fin_mrow    <- proj$matches[[5]][1, ]
      champ_label <- if (fin_mrow$confirmed) {
        paste0(abbrv(champ), " ✓ Champions")
      } else {
        paste0(abbrv(champ), " ", round(fin_mrow$prob * 100), "%")
      }
      bdf_rows[[length(bdf_rows) + 1]] <- data.frame(
        half = "C", round = 6, slot = 1,
        x = x_champ, y = y_champ_pos,
        team = champ, label = champ_label,
        fill_color = "#FFD700",
        confirmed = fin_mrow$confirmed, won = TRUE,
        fsize = 8.5,
        stringsAsFactors = FALSE
      )
    }
    bracket_df <- do.call(rbind, bdf_rows)

    seg_rows <- list()
    for (r in 1:4) {
      arm_L  <- (x_L[r] + x_L[r + 1]) / 2
      arm_R  <- (x_R[r] + x_R[r + 1]) / 2
      y_from <- half_y[[r]]; y_to <- half_y[[r + 1]]
      for (sl in seq_along(y_to)) {
        ya <- y_from[2*sl - 1]; yb <- y_from[2*sl]; ym <- y_to[sl]
        seg_rows <- c(seg_rows, list(
          data.frame(x=x_L[r]+0.55,  y=ya, xend=arm_L,        yend=ya),
          data.frame(x=x_L[r]+0.55,  y=yb, xend=arm_L,        yend=yb),
          data.frame(x=arm_L,        y=ya, xend=arm_L,        yend=yb),
          data.frame(x=arm_L,        y=ym, xend=x_L[r+1]-0.55, yend=ym),
          data.frame(x=x_R[r]-0.55,  y=ya, xend=arm_R,        yend=ya),
          data.frame(x=x_R[r]-0.55,  y=yb, xend=arm_R,        yend=yb),
          data.frame(x=arm_R,        y=ya, xend=arm_R,        yend=yb),
          data.frame(x=arm_R,        y=ym, xend=x_R[r+1]+0.55, yend=ym)
        ))
      }
    }
    seg_rows <- c(seg_rows, list(
      data.frame(x=x_L[5]+0.55, y=y_final, xend=x_champ,       yend=y_final),
      data.frame(x=x_R[5]-0.55, y=y_final, xend=x_champ,       yend=y_final),
      data.frame(x=x_champ,     y=y_final, xend=x_champ,       yend=y_champ_pos-0.55)
    ))
    segs_df <- do.call(rbind, seg_rows)

    list(summary    = summary[order(-summary$`Win %`), ],
         bracket_df = bracket_df,
         segs_df    = segs_df,
         slot_mats  = slot_mats,
         slot_bests = slot_bests)
  })

  output$prob_table <- renderDT({
    d <- sim_data()$summary
    d <- d[d$`Group %` > 0, ]
    datatable(
      d,
      rownames  = FALSE,
      selection = "single",
      options   = list(pageLength = 48, dom = "ft",
                       order = list(list(1, "desc")))
    ) |>
      formatStyle("Win %",
                  background = styleColorBar(c(0, max(d$`Win %`)), "#2c7bb6"),
                  backgroundSize = "98% 60%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })


  # ----- Team tab --------------------------------------------------------------

  output$team_content_ui <- renderUI({
    if (!sim_has_run()) return(sim_notice())
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Stage probabilities"),
        plotOutput("team_path_plot", height = "260px")
      ),
      card(
        card_header("Probable bracket path"),
        div(class = "text-muted small", style = "margin-bottom: 6px;",
          "Most likely opponent at each stage, based on simulation results.",
          "Advance % includes extra time / penalties (50/50 on draws)."),
        tableOutput("team_path_table")
      )
    )
  })

  # Populate selector from groups (available before sim runs).
  observe({
    teams <- sort(unlist(groups_data(), use.names = FALSE))
    updateSelectInput(session, "selected_team", choices = teams)
  })

  # Clicking a row in Probabilities navigates to Team tab with that team.
  observeEvent(input$prob_table_rows_selected, {
    req(length(input$prob_table_rows_selected) > 0)
    d    <- sim_data()$summary
    d    <- d[d$`Group %` > 0, ]
    team <- d$Team[input$prob_table_rows_selected]
    updateSelectInput(session, "selected_team", selected = team)
    nav_select("main_tabs", "Team")
  })

  # Stage probability bar chart.
  output$team_path_plot <- renderPlot({
    if (!sim_has_run()) return(NULL)
    req(sim_data(), nzchar(input$selected_team))
    team <- input$selected_team
    row  <- sim_data()$summary[sim_data()$summary$Team == team, ]
    req(nrow(row) > 0)
    df <- data.frame(
      stage = factor(
        c("Advance from group", "Round of 16", "Quarter-final",
          "Semi-final", "Final", "Winner"),
        levels = c("Advance from group", "Round of 16", "Quarter-final",
                   "Semi-final", "Final", "Winner")
      ),
      prob = c(row$`Group %`, row$`R16 %`, row$`QF %`,
               row$`Semi %`, row$`Final %`, row$`Win %`)
    )
    ggplot(df, aes(x = prob, y = stage)) +
      geom_col(fill = "#2c7bb6", width = 0.55) +
      geom_text(aes(label = paste0(prob, "%")),
                hjust = -0.15, size = 4.2) +
      scale_x_continuous(limits = c(0, max(df$prob, 5) * 1.18),
                         expand  = c(0, 0)) +
      labs(x = "Probability (%)", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank())
  })

  # Probable bracket path table.
  output$team_path_table <- renderTable({
    req(sim_data(), nzchar(input$selected_team))
    team    <- input$selected_team
    sm      <- sim_data()$slot_mats
    sb      <- sim_data()$slot_bests
    elo_v   <- elo_base()
    nph     <- c(16L, 8L, 4L, 2L, 1L)
    labels  <- c("Round of 32", "Round of 16", "Quarter-final", "Semi-final", "Final")

    rows <- lapply(1:5, function(r) {
      if (!team %in% rownames(sm[[r]])) return(NULL)
      counts <- sm[[r]][team, ]
      if (sum(counts) == 0) return(NULL)
      best  <- which.max(counts)
      side  <- if (best <= nph[r]) "L" else "R"
      sl    <- if (side == "L") best else best - nph[r]
      sl_off <- if (side == "L") 0L else nph[r]
      opp_col <- if (r < 5) (if (sl %% 2 == 1) sl + 1L else sl - 1L) + sl_off
                 else       if (side == "L") 2L else 1L
      opp <- sb[[r]][opp_col]
      if (!is.na(opp) && opp != "?" && opp %in% names(elo_v)) {
        mp  <- match_probabilities(elo_v[[team]], elo_v[[opp]])
        adv <- paste0(round(100 * (mp$win_a + 0.5 * mp$draw)), "%")
      } else {
        opp <- "TBD"; adv <- "—"
      }
      data.frame(Round = labels[r], `Likely opponent` = opp,
                 `Advance %` = adv,
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  }, striped = TRUE, hover = TRUE, align = "llr", rownames = FALSE)

  # ----- Bracket visualisation -------------------------------------------------

  output$bracket_ui <- renderUI({
    if (!sim_has_run()) return(sim_notice())
    div(style = "overflow-x: auto; -webkit-overflow-scrolling: touch;",
        plotOutput("bracket_plot", height = "calc(100vh - 120px)", width = "100%"))
  })

  output$bracket_plot <- renderPlot({
    req(sim_data())
    bd  <- sim_data()$bracket_df
    sd  <- sim_data()$segs_df
    x_L <- c(0, 1.8, 3.2, 5.0, 6.2)
    x_R <- c(14.0, 12.2, 10.8, 9.0, 7.8)
    rnd <- c("R32", "R16", "QF", "SF", "Final")

    ggplot() +
      geom_segment(data = sd,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "gray55", linewidth = 0.3) +
      geom_label(data = bd[bd$half != "C", ],
                 aes(x = x, y = y, label = label, fill = fill_color, size = fsize),
                 label.padding = unit(0.15, "lines"),
                 linewidth = 0.2, hjust = 0.5, color = "black") +
      geom_label(data = bd[bd$half == "C", ],
                 aes(x = x, y = y, label = label, size = fsize),
                 fill = "#FFD700", color = "black",
                 label.padding = unit(0.3, "lines"), linewidth = 0.8) +
      scale_size_identity(guide = "none") +
      scale_fill_identity() +
      annotate("text",
               x = c(x_L, 7.0, x_R),
               y = 17.3,
               label = c(rnd, "CHAMP", rnd),
               fontface = "bold", size = 4.2, hjust = 0.5) +
      scale_x_continuous(expand = expansion(add = c(0.7, 0.7))) +
      scale_y_continuous(limits = c(0.5, 18.5),
                         expand = expansion(add = c(0, 0))) +
      labs(caption = "✓ confirmed win     ✗ eliminated     blue shade = projected advance probability vs likely opponent (darker = more likely; incl. 50/50 extra time / penalties)") +
      theme_void() +
      theme(legend.position   = "none",
            plot.caption      = element_text(size = 10, hjust = 0.5, margin = margin(t = 10)),
            plot.background   = element_rect(fill = "white", color = NA),
            plot.margin       = margin(8, 8, 8, 8))
  })

  # ----- Head-to-Head ----------------------------------------------------------

  h2h_data <- eventReactive(input$h2h_run, {
    validate(need(input$h2h_a != input$h2h_b, "Please select two different teams."))
    elo <- elo_base()
    match_probabilities(elo[[input$h2h_a]], elo[[input$h2h_b]],
                        home_a = isTRUE(input$h2h_home))
  })

  output$h2h_boxes <- renderUI({
    req(h2h_data())
    mp <- h2h_data()
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = paste(input$h2h_a, "wins"),
                value = paste0(round(100 * mp$win_a, 1), "%"),
                theme = "primary"),
      value_box(title = "Draw",
                value = paste0(round(100 * mp$draw, 1), "%"),
                theme = "secondary"),
      value_box(title = paste(input$h2h_b, "wins"),
                value = paste0(round(100 * mp$win_b, 1), "%"),
                theme = "danger")
    )
  })

  output$h2h_xg <- renderUI({
    req(h2h_data())
    mp <- h2h_data()
    tagList(
      p(strong(input$h2h_a), paste0("xG: ", round(mp$xg_a, 2))),
      p(strong(input$h2h_b), paste0("xG: ", round(mp$xg_b, 2)))
    )
  })

  output$h2h_scores <- renderDT({
    req(h2h_data())
    mp  <- h2h_data()
    gh  <- mp$goals
    P   <- mp$score_matrix
    df  <- expand.grid(a = gh, b = gh)
    df$prob <- as.vector(P) * 100
    df  <- head(df[order(-df$prob), ], 10)
    out <- data.frame(
      Score    = paste0(df$a, "-", df$b),
      `Prob (%)` = round(df$prob, 1),
      check.names = FALSE
    )
    colnames(out)[1] <- paste0(input$h2h_a, " - ", input$h2h_b)
    datatable(out, rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 10))
  })
}

shinyApp(ui, server)
