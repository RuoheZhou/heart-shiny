library(shiny)
#library(maps)
#library(mapproj)
library(ggplot2)
library(tidyverse)

heart <- read.csv("heart.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel('Heart disease'),
  sidebarLayout(
    sidebarPanel('our inputs will go here', 
                 sliderInput("ageInput", "Age1", min = 0, max = 100,
                             value = c(18, 63), pre = "year"), 
                 radioButtons('sexInput','sex', choices = c('F', 'M'), selected = 'M'), 
                 radioButtons("chestpainInput", "Chestpaintype",
                              choices = c("ATA", "NAP", "ASY", "TA"),
                              selected = "ASY"),
                 sliderInput("RestingBpInput", "RestingBP", min = 0, max = 200,
                             value = c(40, 125)), 
                 sliderInput("CholeInput", "cholesterol", min = 50, max = 300, 
                             value = c(88, 236)), 
                 selectInput("RestECGInput", "restingECG",
                             choices = c("Normal", "ST", "LVH"), 
                             selected = 'Normal')
    ), 
    
    mainPanel(radioButtons("STslopeInput", "stslope", 
                           choices = c('Up', 'Flat', 'Down'), 
                           selected = 'Up'),
              sliderInput('maxhrInput', 'Max_Heart_rate', min = 100, max = 300, 
                          value = c(150, 250)),
              'results',
              plotOutput("coolplot"),
              br(), 
              tableOutput("cooltable"))))

server <- function(input, output) {
  filtered <- reactive({
    heart %>%
      filter(Age >= input$ageInput[1],
             Age <= input$ageInput[2], 
             Sex == input$sexInput, 
             ChestPainType == input$chestpainInput, 
             RestingBP >= input$RestingBpInput[1], 
             RestingBP <= input$RestingBpInput[2], 
             Cholesterol >= input$CholeInput[1], 
             Cholesterol <= input$CholeInput[2],
             RestingECG  == input$RestECGInput,
             ST_Slope == input$STslopeInput,
             MaxHR >= input$maxhrInput[1],
             MaxHR <= input$maxhrInput[2]
             )
    
    })
  output$coolplot <- renderPlot({
    ggplot(filtered(), aes(HeartDisease)) +
      geom_histogram(color = 'black', fill = 'lightblue')
    })
  output$cooltable <- renderTable(heart)
  
}




shinyApp(ui, server)