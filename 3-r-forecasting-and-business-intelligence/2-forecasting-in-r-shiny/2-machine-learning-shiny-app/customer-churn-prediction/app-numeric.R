# Import libraries
library(shiny)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forecasting-and-business-intelligence/2-forecasting-in-r-shiny/2-machine-learning-shiny-app/customer-churn-prediction")

# Read in the RF model
model <- readRDS("model.rds")

# User interface
ui <- fluidPage(
  
  # Page header
  headerPanel('Customer Churn Predictor'),
  
  # Input values
  sidebarLayout(
    sidebarPanel(
      tags$label(h3('Input parameters')),
      numericInput("Total_Refunds", 
                   label = "Total Refunds", 
                   value = 10),
      numericInput("Total_Extra_Data_Charges", 
                   label = "Total Extra Data Charges", 
                   value = 100),
      numericInput("Total_Long_Distance_Charges", 
                   label = "Total Long Distance Charges", 
                   value = 15),
      
      actionButton("submitbutton", "Submit", 
                   class = "btn btn-primary")
    ),
    
    mainPanel(
      tags$label(h3('Customer Status')), # Status/Output Text Box
      verbatimTextOutput('contents'),
      tableOutput('tabledata') # Prediction results table
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({
    if (input$submitbutton > 0) {
      df <- data.frame(
        Name = c("Total Refunds",
                 "Total Extra Data Charges",
                 "Total Long Distance Charges"),
        Value = as.character(c(input$Total.Refunds,
                               input$Total.Extra.Data.Charges,
                               input$Total.Long.Distance.Charges)),
        stringsAsFactors = FALSE
      )
      
      Customer_Satus <- 0
      df <- rbind(df, Customer.Status)
      input <- transpose(df)
      write.table(input[,1:2],"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      test <- test[-1,]
      
      if (!is.null(Customer_Status)) {
        Customer_Status <- as.numeric(Customer_Status)
        Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
        Output <- cbind(Output, Customer_Status)
      } else {
        Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
      }
      
      print(Output)
      
    }
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

# Create the shiny app
shinyApp(ui = ui, server = server)
