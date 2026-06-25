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
    nav_panel(
      "Probabilities",
      DTOutput("prob_table")
    ),
    nav_panel(
      "Chart",
      plotOutput("prob_chart", height = "580px")
    ),
    nav_panel(
      "Standings",
      uiOutput("standings_ui")
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
      "Group Breakdown",
      uiOutput("group_breakdown_ui")
    ),
    nav_panel(
      "Bracket",
      div(style = "overflow-x: auto; -webkit-overflow-scrolling: touch;",
        plotOutput("bracket_plot",
                   height = "calc(100vh - 120px)", width = "100%")
      )
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

  elo_base <- reactive({
    read_elo()
  })

  groups_data <- reactive({ read_groups() })

  observe({
    teams <- sort(names(elo_base()))
    updateSelectInput(session, "h2h_a", choices = teams, selected = teams[1])
    updateSelectInput(session, "h2h_b", choices = teams, selected = teams[2])
  })

  # ----- Last updated ----------------------------------------------------------

  results_data <- reactive({
    res <- tryCatch(readr::read_csv(CACHE_URL_RESULTS, show_col_types = FALSE),
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
    elo    <- elo_base()
    groups <- groups_data()

    teams      <- unlist(groups, use.names = FALSE)
    champ_n    <- setNames(integer(length(teams)), teams)
    final_n    <- setNames(integer(length(teams)), teams)
    semi_n     <- setNames(integer(length(teams)), teams)
    qf_n       <- setNames(integer(length(teams)), teams)
    r16_n      <- setNames(integer(length(teams)), teams)
    r32_n      <- setNames(integer(length(teams)), teams)
    n          <- input$n_sims

    # Group position counters: grp_pos[[group]][[rank]] = count per team
    grp_pos <- lapply(groups, function(g) {
      lapply(1:4, function(r) setNames(integer(length(g)), g))
    })

    # Bracket slot counters: slot_mats[[round]][team, slot] = appearances
    n_slots_r  <- c(32L, 16L, 8L, 4L, 2L, 1L)
    slot_mats  <- lapply(n_slots_r, function(ns)
      matrix(0L, nrow = length(teams), ncol = ns, dimnames = list(teams, NULL))
    )

    withProgress(message = "Simulating...", value = 0, {
      for (s in seq_len(n)) {
        ko <- simulate_tournament(groups, elo, advance_fn = advance_48,
                                   hosts      = c("United States", "Canada", "Mexico"),
                                   results_df = results_data())
        champ_n[ko$champion] <- champ_n[ko$champion] + 1
        for (t in names(ko$reached)) {
          rv <- REACHED_RANK[ko$reached[t]]
          r32_n[t] <- r32_n[t] + 1
          if (rv >= REACHED_RANK["R16"]) r16_n[t]  <- r16_n[t] + 1
          if (rv >= REACHED_RANK["QF"])  qf_n[t]   <- qf_n[t]  + 1
          if (rv >= REACHED_RANK["SF"])  semi_n[t] <- semi_n[t] + 1
          if (rv >= REACHED_RANK["F"])   final_n[t]<- final_n[t]+ 1
        }
        for (g in names(ko$group_tables)) {
          gt <- ko$group_tables[[g]]
          for (pos in 1:4) {
            team <- gt$team[gt$rank == pos]
            grp_pos[[g]][[pos]][team] <- grp_pos[[g]][[pos]][team] + 1
          }
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

    group_df <- do.call(rbind, lapply(names(groups), function(g) {
      g_teams <- groups[[g]]
      data.frame(
        Group  = g,
        Team   = g_teams,
        `1st %` = round(100 * vapply(g_teams, \(t) grp_pos[[g]][[1]][t], numeric(1)) / n, 1),
        `2nd %` = round(100 * vapply(g_teams, \(t) grp_pos[[g]][[2]][t], numeric(1)) / n, 1),
        `3rd %` = round(100 * vapply(g_teams, \(t) grp_pos[[g]][[3]][t], numeric(1)) / n, 1),
        `4th %` = round(100 * vapply(g_teams, \(t) grp_pos[[g]][[4]][t], numeric(1)) / n, 1),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))

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

    # Analytical head-to-head win probability (neutral venue, 90 min).
    h2h <- function(a, b) {
      if (a == "?" || b == "?") return(0)
      match_probabilities(elo[[a]], elo[[b]])$win_a
    }

    bdf_rows <- list()
    for (r in 1:5) {
      nh <- n_per_half[r]
      for (side in c("L", "R")) {
        sl_off <- if (side == "L") 0L else nh
        x_val  <- if (side == "L") x_L[r] else x_R[r]
        for (sl in seq_len(nh)) {
          col_idx <- sl + sl_off
          best    <- slot_bests[[r]][col_idx]
          # Opponent: within-half consecutive pairing (1<->2, 3<->4, ...),
          # except the Final where left slot 1 faces right slot 1.
          opp_col <- if (r < 5) {
            (if (sl %% 2 == 1) sl + 1L else sl - 1L) + sl_off
          } else {
            if (side == "L") 2L else 1L
          }
          opp  <- slot_bests[[r]][opp_col]
          prob <- h2h(best, opp)
          bdf_rows[[length(bdf_rows) + 1]] <- data.frame(
            half = side, round = r, slot = sl,
            x = x_val, y = half_y[[r]][sl],
            team = abbrv(best), prob = prob,
            fsize = c(4.2, 5.2, 5.4, 6.4, 7.0)[r],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    {
      # Champion: whichever Final team has higher 90-min win probability vs
      # the other, keeping the bracket internally consistent with the slot labels.
      left_fin  <- slot_bests[[5]][1]
      right_fin <- slot_bests[[5]][2]
      p_left    <- h2h(left_fin, right_fin)
      p_right   <- h2h(right_fin, left_fin)
      best      <- if (left_fin == "?" && right_fin == "?") "?"
                   else if (left_fin  == "?") right_fin
                   else if (right_fin == "?") left_fin
                   else if (p_left >= p_right) left_fin else right_fin
      prob      <- if (best == left_fin) p_left else p_right
      bdf_rows[[length(bdf_rows) + 1]] <- data.frame(
        half = "C", round = 6, slot = 1,
        x = x_champ, y = y_champ_pos,
        team = abbrv(best), prob = prob,
        fsize = 8.5,
        stringsAsFactors = FALSE
      )
    }
    bracket_df       <- do.call(rbind, bdf_rows)
    bracket_df$label <- paste0(bracket_df$team, " ",
                               round(bracket_df$prob * 100), "%")

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
         group_df   = group_df,
         bracket_df = bracket_df,
         segs_df    = segs_df)
  })

  output$prob_table <- renderDT({
    d <- sim_data()$summary
    datatable(
      d,
      rownames  = FALSE,
      selection = "none",
      options   = list(pageLength = 48, dom = "ft",
                       order = list(list(1, "desc")))
    ) |>
      formatStyle("Win %",
                  background = styleColorBar(c(0, max(d$`Win %`)), "#2c7bb6"),
                  backgroundSize = "98% 60%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })

  output$prob_chart <- renderPlot({
    df      <- head(sim_data()$summary, 16)
    df$Team <- factor(df$Team, levels = rev(df$Team))
    ggplot(df, aes(x = `Win %`, y = Team)) +
      geom_col(fill = "#2c7bb6", width = 0.7) +
      geom_text(aes(label = paste0(`Win %`, "%")),
                hjust = -0.15, size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Win probability -- top 16 teams",
           x = "Win probability (%)", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank())
  })

  # ----- Group breakdown -------------------------------------------------------

  output$group_breakdown_ui <- renderUI({
    req(sim_data())
    groups <- groups_data()
    cards <- lapply(sort(names(groups)), function(g) {
      card(card_header(paste("Group", g)),
           tableOutput(paste0("gbd_", g)))
    })
    do.call(layout_columns, c(list(col_widths = rep(4, 12)), cards))
  })

  for (g in LETTERS[1:12]) {
    local({
      grp <- g
      output[[paste0("gbd_", grp)]] <- renderTable({
        req(sim_data())
        gdf <- sim_data()$group_df
        gdf[gdf$Group == grp, c("Team", "1st %", "2nd %", "3rd %", "4th %")]
      }, striped = TRUE, hover = TRUE, align = "lrrrr", rownames = FALSE)
    })
  }

  # ----- Bracket visualisation -------------------------------------------------

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
                 aes(x = x, y = y, label = label, fill = prob, size = fsize),
                 label.padding = unit(0.15, "lines"),
                 linewidth = 0.2, hjust = 0.5, color = "black") +
      geom_label(data = bd[bd$half == "C", ],
                 aes(x = x, y = y, label = label, size = fsize),
                 fill = "#FFD700", color = "black",
                 label.padding = unit(0.3, "lines"), linewidth = 0.8) +
      scale_size_identity(guide = "none") +
      annotate("text",
               x = c(x_L, 7.0, x_R),
               y = 17.3,
               label = c(rnd, "CHAMP", rnd),
               fontface = "bold", size = 4.2, hjust = 0.5) +
      scale_fill_gradient(low = "white", high = "#2c7bb6",
                          limits = c(0, 1), name = "90-min win probability vs likely opponent",
                          labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(expand = expansion(add = c(0.7, 0.7))) +
      scale_y_continuous(limits = c(0.5, 18.5),
                         expand = expansion(add = c(0, 0))) +
      theme_void() +
      theme(legend.position  = "bottom",
            legend.key.width = unit(2, "cm"),
            plot.background  = element_rect(fill = "white", color = NA),
            plot.margin      = margin(8, 8, 8, 8))
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
