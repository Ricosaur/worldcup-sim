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
    sliderInput("n_sims", "Simulations",
                min = 500, max = 10000, value = 2000, step = 500),
    actionButton("run", "Run simulation",
                 class = "btn-primary w-100 mt-1"),
    hr(),
    helpText(
      "Elo ratings sourced from eloratings.net,",
      "compiled by the Kaggle dataset creator.",
      "Match results via football-data.org API,",
      "updated daily by GitHub Actions."
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
    )
  )
)

# ----- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  sim_data <- eventReactive(input$run, {
    elo    <- read_elo()
    groups <- read_groups()
    elo    <- roll_elo_through_results(elo)

    teams      <- unlist(groups, use.names = FALSE)
    champ_n    <- setNames(integer(length(teams)), teams)
    final_n    <- setNames(integer(length(teams)), teams)
    semi_n     <- setNames(integer(length(teams)), teams)
    qf_n       <- setNames(integer(length(teams)), teams)
    r16_n      <- setNames(integer(length(teams)), teams)
    r32_n      <- setNames(integer(length(teams)), teams)
    n          <- input$n_sims

    withProgress(message = "Simulating...", value = 0, {
      for (s in seq_len(n)) {
        ko <- simulate_tournament(groups, elo, advance_fn = advance_48)
        champ_n[ko$champion] <- champ_n[ko$champion] + 1
        for (t in names(ko$reached)) {
          rv <- REACHED_RANK[ko$reached[t]]
          r32_n[t] <- r32_n[t] + 1
          if (rv >= REACHED_RANK["R16"]) r16_n[t]  <- r16_n[t] + 1
          if (rv >= REACHED_RANK["QF"])  qf_n[t]   <- qf_n[t]  + 1
          if (rv >= REACHED_RANK["SF"])  semi_n[t] <- semi_n[t] + 1
          if (rv >= REACHED_RANK["F"])   final_n[t]<- final_n[t]+ 1
        }
        if (s %% 100 == 0) setProgress(s / n)
      }
    })

    data.frame(
      Team       = teams,
      `Win %`    = round(100 * champ_n[teams] / n, 1),
      `Final %`  = round(100 * final_n[teams] / n, 1),
      `Semi %`   = round(100 * semi_n[teams]  / n, 1),
      `QF %`     = round(100 * qf_n[teams]    / n, 1),
      `R16 %`    = round(100 * r16_n[teams]   / n, 1),
      `Group %`  = round(100 * r32_n[teams]   / n, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ) |> (\(d) d[order(-d$`Win %`), ])()
  })

  output$prob_table <- renderDT({
    datatable(
      sim_data(),
      rownames  = FALSE,
      selection = "none",
      options   = list(pageLength = 48, dom = "ft",
                       order = list(list(1, "desc")))
    ) |>
      formatStyle("Win %",
                  background = styleColorBar(c(0, max(sim_data()$`Win %`)),
                                             "#2c7bb6"),
                  backgroundSize = "98% 60%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })

  output$prob_chart <- renderPlot({
    df      <- head(sim_data(), 16)
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
}

shinyApp(ui, server)
