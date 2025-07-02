# 07_shiny_dashboard.R
# Shiny dashboard with live Yahoo Finance data using quantmod

library(shiny)
library(quantmod)

# List of DAX symbols you have in your data
dax_symbols <- c("AIR.DE", "ALV.DE", "BAS.DE", "DBK.DE", "DHL.DE", "EOAN.DE", 
                 "HEI.DE", "IFX.DE", "RWE.DE", "SIE.DE", "SY1.DE", "VOW3.DE", "ZAL.DE")

ui <- fluidPage(
  titlePanel("Live DAX Stock Price Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symbol", "Choose Symbol:", choices = dax_symbols),
      actionButton("refresh", "Refresh Price")
    ),
    mainPanel(
      h3("Live Price Data"),
      tableOutput("live_price"),
      h3("Recent Historical Chart"),
      plotOutput("price_chart")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the latest quote
  live_data <- reactiveVal()
  
  observeEvent(input$refresh, {
    # Fetch live quote when refresh is clicked
    live_data(getQuote(input$symbol))
  }, ignoreInit = FALSE)
  
  output$live_price <- renderTable({
    req(live_data())
    live_data()
  }, rownames = TRUE)
  
  output$price_chart <- renderPlot({
    # Fetch last 3 months of daily data for the selected symbol
    chart_data <- tryCatch({
      getSymbols(input$symbol, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - 90)
    }, error = function(e) NULL)
    if (!is.null(chart_data)) {
      chartSeries(chart_data, name = input$symbol, theme = chartTheme("white"))
    }
  })
}

shinyApp(ui, server) 