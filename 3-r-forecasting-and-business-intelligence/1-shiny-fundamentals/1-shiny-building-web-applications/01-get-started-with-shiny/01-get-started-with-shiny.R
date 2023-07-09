# Install packages 
install.packages("shiny")
install.packages("ggplot2")

library(shiny) # for building the dashboard app
library(ggplot2) # for visualization

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forecasting-and-business-intelligence/1-shiny-fundamentals/1-shiny-building-web-applications/01-get-started-with-shiny")


# Creating the user interface (UI) for the Shiny app
ui <- fluidPage(
  # Displaying the text "Hello, world!!!" in the UI
  'Hello, world!!!' 
)

# Defining the server function, which handles the server-side computations and interactions
server <- function(input, output, session) {
  # The input parameter represents the user input
  # The output parameter is used to send output to the UI
  # The session parameter represents the current Shiny session
}

# Creating the Shiny app by combining the UI and server functions
shinyApp(ui = ui, server = server)



# Creating the user interface (UI) for the Shiny app
ui <- fluidPage(
  # Displaying a text input field labeled "Enter your name:"
  textInput("name", "Enter your name:")
)

# Defining the server function, which handles the server-side computations and interactions
server <- function(input, output) {
  # The input parameter represents the user input
  # The output parameter is used to send output to the UI
}

# Creating the Shiny app by combining the UI and server functions
shinyApp(ui = ui, server = server)



# Creating the user interface (UI) for the Shiny app
ui <- fluidPage(
  # Displaying a text input field labeled "What is your name?"
  textInput("name", "What is your name?"),
  # Displaying an output area for the greeting
  textOutput("greeting")
)

# Defining the server function, which handles the server-side computations and interactions
server <- function(input, output) {
  # Rendering the output text based on the input value
  output$greeting <- renderText({  
    # Generating the greeting by concatenating "Hello," with the value of input$name
    paste("Hello,", input$name)  
  })
}

# Creating the Shiny app by combining the UI and server functions
shinyApp(ui = ui, server = server)



characters <- read.csv("Harry_Potter_Movies/Characters.csv", fileEncoding = "UTF-8")

# UI
ui <- fluidPage(
  # Dropdown to select a house
  selectInput("house", "Select House", choices = unique(characters$House))
)

# Server
server <- function(input, output, session) {
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



# UI
ui <- fluidPage(
  # Dropdown to select a house
  selectInput("house", "Select House", choices = unique(characters$House))
)

# Server
server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Placeholder for the actual plot generation code
    ggplot()  
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



# UI
ui <- fluidPage(
  titlePanel("Harry Potter Character Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select a house
      selectInput("house", "Select House", choices = unique(characters$House))
    ),
    mainPanel(
      # Plot output
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Placeholder for the actual plot generation code
    ggplot()  
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



# UI
ui <- fluidPage(
  titlePanel("Harry Potter Character Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select a house
      selectInput("house", "Select House", choices = unique(characters$House))
    ),
    mainPanel(
      # Plot output
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Filtering the data based on the selected house
    filtered_data <- subset(characters, House == input$house)
    
    # Generating the bar plot
    ggplot(filtered_data) +
      geom_bar(aes(x = Gender, fill = House), position = "dodge") +
      labs(x = "Gender", y = "Count")  # Update x-axis and y-axis labels as needed
  })
  
  output$gender <- renderText({
    # Filtering the data based on the selected house
    filtered_data <- subset(characters, House == input$house)
    
    # Retrieving unique gender values
    unique(filtered_data$Gender)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



