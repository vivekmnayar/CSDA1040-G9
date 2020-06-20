#Install these packages if not already installed
#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(proxy)
library(recommenderlab)
library(reshape2)

#Read rds and csv files exported from BookRecommender.R
BookRatingDF7 <-readRDS("BookRecommenderdf.rds") 
Books <- read.csv("BookDropDown.csv", header = TRUE, stringsAsFactors=FALSE)

# UI function

shinyUI <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Book Recommendation Engine"),
  fluidRow( 
      column(5,
             tags$h3("Choose 3 books you like in order of your preference"), 
           selectInput("select", label = "First Preference",
                       choices = as.character(Books$BookTitle[1:1000])),
           
           selectInput("select2", label = "Second Preference",
                       choices = as.character(Books$BookTitle[1:1000])),
           
           selectInput("select3", label = "Third Preference",
                       choices = as.character(Books$BookTitle[1:1000])),
           
           submitButton("Submit")
    ),
    
    column(7,
           h3("You Might Like These Too!"),
           tableOutput("table"))
  
  )
)

########Book recommendation logic

book_recommendation <- function(input,input2,input3) {
  
  row_num <- which(Books[,3] == input)
  row_num2 <- which(Books[,3] == input2)
  row_num3 <- which(Books[,3] == input3)

  userSelect <- matrix(NA,4757)
  userSelect[row_num] <- 10 #hard code first selection to rating 10
  userSelect[row_num2] <- 9 #hard code second selection to rating 9
  userSelect[row_num3] <- 8 #hard code third selection to rating 8
  userSelect <- t(userSelect)
  
  ratingmat <- BookRatingDF7
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect,ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)
  
  #Convert rating matrix into a sparse matrix
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")
  
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))
  recom <- predict(recommender_model, ratingmat2[1], n=10)
  recom_list <- as(recom, "list")
  recom_result <- data.frame(matrix(NA,10))
  for (i in c(1:10)){
    recom_result[i,1] <- unlist(subset(Books, ISBN==recom_list[[1]][i], select=c(BookTitle)))
  }
  colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
  return(recom_result)
}

############Server logic

shinyServer <- function(input, output) {
  
  output$table <- renderTable({
    book_recommendation(input$select, input$select2, input$select3)
  })
  
}


#Create Shiny object

shinyApp(ui = shinyUI, server = shinyServer) #Shiny call
