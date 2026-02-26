# app.R
# install.packages(c("shiny","dplyr","readr","lubridate","ggplot2","DT"))

library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(DT)

hist_path <- "data/stablecoins_history.csv"

load_hist <- function() {
  if (!file.exists(hist_path)) {
    stop("No history yet. Run fetch_data.R first to create data/stablecoins_history.csv")
  }
  read_csv(hist_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
}

ui <- fluidPage(
  titlePanel("Stablecoin Watchtower"),
  sidebarLayout(
    sidebarPanel(
      helpText("Tracks depegs + supply shocks using free public data."),
      sliderInput("peg_thr_bp", "Depeg threshold (bps)", min = 1, max = 300, value = 50, step = 1),
      sliderInput("supply_thr_pct", "Supply shock threshold (%)", min = 0.1, max = 10, value = 2, step = 0.1),
      uiOutput("coin_picker")
    ),
    mainPanel(
      h4("Off-Peg Leaderboard (Today)"),
      DTOutput("offpeg_tbl"),
      br(),
      h4("Biggest Supply Changes (Today vs Yesterday)"),
      DTOutput("supply_tbl"),
      br(),
      h4("Selected Stablecoin Trend"),
      plotOutput("trend_plot", height = "320px")
    )
  )
)

server <- function(input, output, session) {
  hist <- reactive({
    load_hist()
  })
  
  latest <- reactive({
    h <- hist()
    max_date <- max(h$date, na.rm = TRUE)
    h %>% filter(date == max_date)
  })
  
  output$coin_picker <- renderUI({
    syms <- latest() %>% arrange(symbol) %>% pull(symbol) %>% unique()
    selectInput("symbol", "Stablecoin", choices = syms, selected = if ("USDC" %in% syms) "USDC" else syms[1])
  })
  
  output$offpeg_tbl <- renderDT({
    df <- latest() %>%
      mutate(
        peg_bp = abs(price - 1) * 10000,
        offpeg_flag = peg_bp >= input$peg_thr_bp
      ) %>%
      arrange(desc(peg_bp)) %>%
      select(symbol, name, price, circulating, mcap_usd, peg_bp, offpeg_flag) %>%
      head(25)
    
    datatable(df, options = list(pageLength = 10))
  })
  
  output$supply_tbl <- renderDT({
    h <- hist()
    max_date <- max(h$date, na.rm = TRUE)
    prev_date <- max_date - 1
    
    today <- h %>% filter(date == max_date) %>% select(id, symbol, name, circulating, price, mcap_usd)
    yday  <- h %>% filter(date == prev_date) %>% select(id, circulating) %>% rename(circ_yday = circulating)
    
    df <- today %>%
      left_join(yday, by = "id") %>%
      mutate(
        supply_chg_pct = 100 * (circulating - circ_yday) / circ_yday,
        shock_flag = !is.na(supply_chg_pct) & abs(supply_chg_pct) >= input$supply_thr_pct
      ) %>%
      arrange(desc(abs(supply_chg_pct))) %>%
      select(symbol, name, circulating, circ_yday, supply_chg_pct, shock_flag) %>%
      head(25)
    
    datatable(df, options = list(pageLength = 10))
  })
  
  output$trend_plot <- renderPlot({
    h <- hist() %>% filter(symbol == input$symbol) %>% arrange(date)
    if (nrow(h) < 2) return(NULL)
    
    h <- h %>% mutate(peg_bp = abs(price - 1) * 10000)
    
    ggplot(h, aes(x = date, y = peg_bp)) +
      geom_line() +
      labs(x = "Date", y = "Off-peg (bps)", title = paste0(input$symbol, " peg deviation (bps)"))
  })
}

shinyApp(ui, server)