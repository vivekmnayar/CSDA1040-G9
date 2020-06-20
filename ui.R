# Install these two packages if not already installed
#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(shinythemes)



# UI function

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                navbarPage(
                  
                  "My Book Recommender",
                  
                  tabPanel("Book Info",
                           
                           sidebarPanel(
                             
                             tags$h3("ISBN Number:"),
                             textInput("isbn1", "Book 1 ISBN:", ""),
                             textInput("isbn2", "Book 2 ISBN:", ""),
                             textInput("isbn3", "Book 3 ISBN:", ""),
                             
                             tags$h3("Your Rating:"),
                             numericInput("ratg1", "Book 1 Rating:", 1, min = 1, max = 10),
                             numericInput("ratg2", "Book 2 Rating:",  1, min = 1, max = 10),
                             numericInput("ratg3", "Book 3 Rating:",  1, min = 1, max = 10),
                             submitButton("Get Recommendations"),
                             
                           ), # sidebarPanel
                           
                           
                           mainPanel(
                             
                             h3("We Also Recommend the Following Books:"),
                             
                             verbatimTextOutput("isbnratg"),
                             
                             
                           ) # mainPanel
                  ) 
                ) # navigbarPage
) # this is fluidPage



# server function  
ShinySRV <- function(input, output) {
  
  output$isbnratg <- renderText({
    paste( input$isbn1, input$ratg1, sep = " ")
    
  })
  
} # ShinySRV



# Create Shiny object

shinyApp(ui = ui, server = ShinySRV) #Shiny call

