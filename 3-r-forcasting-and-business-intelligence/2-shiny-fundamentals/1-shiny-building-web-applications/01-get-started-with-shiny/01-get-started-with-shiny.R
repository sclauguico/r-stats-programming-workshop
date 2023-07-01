# Install packages 
install.packages("shiny")
install.packages("ggplot2")

library(shiny)
library(ggplot2) # for visualization

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forcasting-and-business-intelligence/2-shiny-fundamentals/1-shiny-building-web-applications/01-get-started-with-shiny")


# Creating the user interface (UI) for the Shiny app
ui <- fluidPage(
  'Hello, world!!!' # Displaying the text "Hello, world!!!" in the UI
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
  textInput("name", "What is your name?"),  # Displaying a text input field labeled "What is your name?"
  textOutput("greeting")  # Displaying an output area for the greeting
)

# Defining the server function, which handles the server-side computations and interactions
server <- function(input, output) {
  output$greeting <- renderText({  # Rendering the output text based on the input value
    paste("Hello,", input$name)  # Generating the greeting by concatenating "Hello," with the value of input$name
  })
}

# Creating the Shiny app by combining the UI and server functions
shinyApp(ui = ui, server = server)









characters <- read.csv("Harry_Potter_Movies/Characters.csv", fileEncoding = "UTF-8")

# Creating the user interface (UI) for the Shiny app
ui <- fluidPage(
  titlePanel("Harry Potter Character Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("house", "Select House", choices = unique(characters$House)),  # Dropdown to select a house
      selectInput("gender", "Select Gender", choices = unique(characters$Gender)),  # Dropdown to select a gender
      selectInput("species", "Select Species", choices = unique(characters$Species))  # Dropdown to select a species
    ),
    mainPanel(plotOutput("trend"))
  )
)

# Defining the server function, which handles the server-side computations and interactions
server <- function(input, output, session) {
  output$trend <- renderPlot({
    # CODE BELOW: Update to display a line plot based on the selected filters
    filtered_data <- subset(characters, House == input$house & Gender == input$gender & Species == input$species)
    ggplot(filtered_data) +
      geom_line(aes(x = x_axis, y = y_axis, color = House))  # Replace x_axis and y_axis with the appropriate fields from your dataset
  })
}

# Creating the Shiny app by combining the UI and server functions
shinyApp(ui = ui, server = server)
