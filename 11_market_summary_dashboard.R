library(shiny)
library(httr)
library(jsonlite)
library(sparkline)

# Finnhub API key
finnhub_key <- "d1ifg2hr01qhsrhf93lgd1ifg2hr01qhsrhf93m0"

# Indices for each region and their Finnhub symbols
us_indices <- list(
  "S&P 500" = "^GSPC",
  "Dow Jones" = "^DJI",
  "Nasdaq" = "^IXIC",
  "Russell 2000" = "^RUT",
  "VIX" = "^VIX",
  "Gold" = "GC=F",
  "Silver" = "SI=F",
  "Oil" = "CL=F",
  "Bitcoin" = "BINANCE:BTCUSDT"
)
europe_indices <- list(
  "DAX" = "^GDAXI",
  "FTSE 100" = "^FTSE",
  "CAC 40" = "^FCHI",
  "Euro Stoxx 50" = "^STOXX50E"
)
asia_indices <- list(
  "Nikkei 225" = "^N225",
  "Hang Seng" = "^HSI",
  "Shanghai" = "000001.SS"
)
rates_indices <- list(
  "US 10Y" = "US10Y",
  "US 2Y" = "US2Y",
  "German Bund" = "DE10Y",
  "UK Gilt" = "GB10Y"
)

# Helper to get live quote and real sparkline (last 10 closes)
get_index_data <- function(symbol) {
  # Live quote
  url <- paste0("https://finnhub.io/api/v1/quote?symbol=", symbol, "&token=", finnhub_key)
  res <- tryCatch(GET(url), error = function(e) NULL)
  data <- tryCatch(fromJSON(content(res, "text")), error = function(e) NULL)
  price <- if (!is.null(data$c)) data$c else NA
  change <- if (!is.null(data$d)) data$d else NA
  percent <- if (!is.null(data$dp)) data$dp else NA
  # Sparkline: fetch last 10 closes
  hist_url <- paste0("https://finnhub.io/api/v1/stock/candle?symbol=", symbol, "&resolution=D&count=10&token=", finnhub_key)
  hist_res <- tryCatch(GET(hist_url), error = function(e) NULL)
  hist_data <- tryCatch(fromJSON(content(hist_res, "text")), error = function(e) NULL)
  spark <- if (!is.null(hist_data$c) && length(hist_data$c) > 0) hist_data$c else rep(price, 10)
  list(
    price = price,
    change = change,
    percent = percent,
    spark = spark
  )
}

# Market open/close status (US)
get_market_status <- function() {
  now <- as.POSIXlt(Sys.time(), tz = "America/New_York")
  open <- as.POSIXlt(format(now, "%Y-%m-%d 09:30:00"), tz = "America/New_York")
  close <- as.POSIXlt(format(now, "%Y-%m-%d 16:00:00"), tz = "America/New_York")
  if (now < open) {
    mins <- as.numeric(difftime(open, now, units = "mins"))
    sprintf("U.S. markets open in %dh %dm", mins %/% 60, round(mins %% 60))
  } else if (now > close) {
    "U.S. markets closed"
  } else {
    "U.S. markets open"
  }
}

index_panel <- function(indices) {
  fluidRow(
    lapply(names(indices), function(idx) {
      data <- get_index_data(indices[[idx]])
      col <- ifelse(!is.na(data$change) && data$change >= 0, "green", "red")
      column(4,
        tags$div(
          tags$strong(idx),
          tags$div(style = sprintf("color:%s", col), 
                   paste0(
                     ifelse(is.na(data$price), "N/A", formatC(data$price, format = "f", digits = 2)),
                     ifelse(!is.na(data$change),
                       paste0(" (", ifelse(data$change >= 0, "+", ""),
                              formatC(data$change, format = "f", digits = 2), ", ",
                              ifelse(data$percent >= 0, "+", ""),
                              formatC(data$percent, format = "f", digits = 2), "%)"), "")
                   )
          ),
          sparkline::sparkline(data$spark, type = "line")
        )
      )
    })
  )
}

ui <- fluidPage(
  tags$h4(textOutput("market_status")),
  tabsetPanel(
    tabPanel("US", index_panel(us_indices)),
    tabPanel("Europe", index_panel(europe_indices)),
    tabPanel("Asia", index_panel(asia_indices)),
    tabPanel("Rates", index_panel(rates_indices))
  )
)

server <- function(input, output, session) {
  output$market_status <- renderText({
    get_market_status()
  })
}

shinyApp(ui, server) 