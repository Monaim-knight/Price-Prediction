library(shiny)
library(httr)
library(jsonlite)
library(alphavantager)
library(TTR)
library(dplyr)

# Finnhub API key
finnhub_key <- "d1ifg2hr01qhsrhf93lgd1ifg2hr01qhsrhf93m0"

# Alpha Vantage API key (use your previous one)
av_api_key('XKNDE3IE8B9HBFHS')

# List of DAX symbols
symbols <- c("AIR.DE", "ALV.DE", "BAS.DE", "DBK.DE", "DHL.DE", "EOAN.DE", 
             "HEI.DE", "IFX.DE", "RWE.DE", "SIE.DE", "SY1.DE", "VOW3.DE", "ZAL.DE")

get_finnhub_quote <- function(symbol) {
  url <- paste0("https://finnhub.io/api/v1/quote?symbol=", symbol, "&token=", finnhub_key)
  res <- tryCatch(GET(url), error = function(e) NULL)
  if (is.null(res) || http_error(res)) {
    return(data.frame(Symbol = symbol, Price = NA, Change = NA, PercentChange = NA))
  }
  data <- tryCatch(fromJSON(content(res, "text")), error = function(e) NULL)
  if (is.null(data) || length(data) == 0 || is.null(data$c)) {
    return(data.frame(Symbol = symbol, Price = NA, Change = NA, PercentChange = NA))
  }
  data.frame(
    Symbol = symbol,
    Price = data$c,
    Change = data$d,
    PercentChange = data$dp
  )
}

get_hist_data <- function(symbol) {
  tryCatch({
    av_get(symbol = symbol, av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "compact")
  }, error = function(e) NULL)
}

ui <- fluidPage(
  titlePanel("Live DAX Dashboard: Finnhub Quotes & Alpha Vantage Technicals"),
  sidebarLayout(
    sidebarPanel(
      selectInput("watchlist", "Select stocks for watchlist:", choices = symbols, multiple = TRUE, selected = symbols[1:3]),
      selectInput("main_symbol", "Main chart symbol:", choices = symbols, selected = symbols[1]),
      checkboxGroupInput("indicators", "Technical Indicators:", 
                         choices = c("SMA 20" = "sma", "RSI 14" = "rsi", "Bollinger Bands" = "bbands", "MACD" = "macd"),
                         selected = c("sma", "bbands")),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      h3("Watchlist (Live Quotes)"),
      tableOutput("watchlist_table"),
      h3("Price Chart with Technicals"),
      plotOutput("main_chart")
    )
  )
)

server <- function(input, output, session) {
  # Watchlist live quotes
  watchlist_data <- eventReactive(input$refresh, {
    req(input$watchlist)
    do.call(rbind, lapply(input$watchlist, get_finnhub_quote))
  }, ignoreNULL = FALSE)
  
  output$watchlist_table <- renderTable({
    watchlist_data()
  })
  
  # Main chart historical data
  main_data <- eventReactive(input$refresh, {
    get_hist_data(input$main_symbol)
  }, ignoreNULL = FALSE)
  
  output$main_chart <- renderPlot({
    d <- main_data()
    req(!is.null(d), nrow(d) > 20)
    d <- d %>% arrange(timestamp)
    plot(d$timestamp, d$adjusted_close, type = "l", col = "black", lwd = 2, xlab = "Date", ylab = "Adjusted Close", main = input$main_symbol)
    
    if ("sma" %in% input$indicators) {
      lines(d$timestamp, SMA(d$adjusted_close, n = 20), col = "blue", lwd = 2)
    }
    if ("bbands" %in% input$indicators) {
      bb <- BBands(d$adjusted_close, n = 20)
      lines(d$timestamp, bb$up, col = "red", lty = 2)
      lines(d$timestamp, bb$dn, col = "red", lty = 2)
    }
    if ("rsi" %in% input$indicators) {
      par(new = TRUE)
      plot(d$timestamp, RSI(d$adjusted_close, n = 14), type = "l", col = "purple", axes = FALSE, xlab = "", ylab = "")
      axis(4)
      mtext("RSI", side = 4, line = 3)
    }
    if ("macd" %in% input$indicators) {
      macd <- MACD(d$adjusted_close, nFast = 12, nSlow = 26, nSig = 9)
      par(new = TRUE)
      plot(d$timestamp, macd$macd, type = "l", col = "green", axes = FALSE, xlab = "", ylab = "")
      axis(4)
      mtext("MACD", side = 4, line = 3)
    }
  })
}

shinyApp(ui, server) 