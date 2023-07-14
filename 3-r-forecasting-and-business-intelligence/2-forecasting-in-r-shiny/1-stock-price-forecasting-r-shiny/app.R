library(shiny)
library(shinythemes)
library(fpp2)
require(gridExtra)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forecasting-and-business-intelligence/2-forecasting-in-r-shiny/1-stock-price-forecasting-r-shiny")

ui <- fluidPage(
  
  # Application title
  titlePanel("12 Month Stock Price for HSBC"),
  
  # Set the dark theme
  theme = shinytheme("darkly"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # select time range to display 
      sliderInput("n",
                  "Number of Days",
                  value = c(153, 253),
                  min = 1,
                  max = 253
      ),
      
      # days for prediction ahead
      numericInput("h", "Days to predict", value = 10),
      
      # add options for prediction method
      radioButtons("model", "Model to select",
                   choices = c("naive", "ARIMA", "NeuralNet"),
                   choiceValues = "ARIMA")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("trendPlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$trendPlot <- renderPlot({
    
    stock <- read.csv("HSBC.csv")
    stock <- stock[c("Date", "Close")]
    stock$Date <- as.Date(stock$Date)
    
    # Plot the close prices in 12 months
    p1 <- autoplot(ts(stock[input$n[1]:input$n[2], "Close"])) +
      ggtitle("Close Prices in 12 Month")
    
    # Fit an ARIMA model to the last 100 days of data
    end = dim(stock)[1]
    start = end - 100
    
    if (input$model == "naive") {
      mod <- naive(stock[start:end, "Close"])
    } else if (input$model == "ARIMA") {
      mod <- auto.arima(stock[start:end, "Close"])
    } else {
      mod <- nnetar(stock[start:end, "Close"])
    }
    
    # Forecast the next 10 days based on the ARIMA model
    data <- forecast(mod, h = input$h)
    
    # Plot the forecast
    p2 <- autoplot(forecast(mod, h = input$h)) + 
      ggtitle("Forecast for next 10 Days based on past 100 Days Price")
    
    # Arrange the two plots in a grid
    grid.arrange(p1, p2, ncol = 1)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)