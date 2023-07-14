# Import libraries
library(shiny)
library(data.table)
library(randomForest)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forecasting-and-business-intelligence/2-forecasting-in-r-shiny/2-machine-learning-shiny-app/customer-churn-prediction")


# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Customer Churn Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("Total.Refunds", label = "Total Refunds", value = 0,
                min = min(TrainSet$Total.Refunds),
                max = max(TrainSet$Total.Refunds)
    ),
    sliderInput("Total.Extra.Data.Charges", label = "Total Extra Data Charges", value = 0,
                min = min(TrainSet$Total.Extra.Data.Charges),
                max = max(TrainSet$Total.Extra.Data.Charges)),
    sliderInput("Total.Long.Distance.Charges", label = "Total Long Distance Charges", value = 0,
                min = min(TrainSet$Total.Long.Distance.Charges),
                max = max(TrainSet$Total.Long.Distance.Charges)),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Total Refunds",
               "Total Extra Data Charges",
               "Total Long Distance Charges"),
      Value = as.character(c(input$Total.Refunds,
                             input$Total.Extra.Data.Charges,
                             input$Total.Long.Distance.Charges)),
      stringsAsFactors = FALSE)
    
    df <- rbind(df, "Customer.Status" = 0)
    input_df <- data.frame(t(df), stringsAsFactors = FALSE)
    write.csv(input_df, "input.csv", row.names = FALSE)
    
    test <- read.csv("input.csv", header = TRUE)
    
    Output <- data.frame(Prediction = predict(model, test), round(predict(model, test, type = "prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
