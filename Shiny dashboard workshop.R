################################################################
#Shiny workshop

#Creating a dashboard

#Created by Eirik Espe
################################################################

# Call on packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Import the dataset
data <- readRDS("data/data_ready.rds")

data$Year <- format(as.Date(data$date), "%Y")

# Adding structure of the app
ui <- dashboardPage(
  dashboardHeader(title = "Energy consumption Dashboard"),
  dashboardSidebar(selectInput(inputId = "select",
                               label = "Select year for lineplot",
                               choices = list("All", 2007, 2008, 2009, 2010),
                               selected = "All"),
                   
                   sliderInput(inputId = "slider",
                   label = "Choose the number of breaks for the histogram",
                   min = 10,
                   max = 50,
                   value = 25)),
  dashboardBody(
    box(
      plotOutput(outputId = "lineplot1")
      ),
    box(
      plotOutput(outputId = "histogram1")
    )
    )
  
)

server <- function(input, output) {
  
  output$lineplot1 <- renderPlot({
    
    data %>%
      filter(if(input$select != "All") (Year == input$select) else TRUE) %>%
      
      ggplot() + geom_line(aes(x = date, y = ActiveEnergy_avg)) +
      labs(title = "Mean active energy per day", 
           x = "", y = "Mean Watt/Hours") +
      theme(plot.title = element_text(hjust = 0.5))
    })
  
  output$histogram1 <- renderPlot({
    hist(data$ActiveEnergy_avg, breaks = input$slider, 
         main = "Histogram of Active energy", 
         xlab = "Watt/Hours")
  })
  
}

shinyApp(ui, server)
