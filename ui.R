library(shiny)



# Define UI for application that predicts Balance
shinyUI(fluidPage(
  
  
  # Webpage styling commands  
  tags$head(
    tags$style(HTML("
                    
                    body {
                    font-family: 'Verdana';
                    #font-weight: 500;
                    #line-height: 1.1;
                    background-color: hsl(0, 0%, 83%);
                    color: #000000;
                    }
                    
                    "))
    ),
  
  
  
  # Application title
  titlePanel(h1(strong("Word Prediction:"))),
  
  # Sidebar with a various inputs 
  sidebarLayout(
    sidebarPanel(
  
      h3("Write your thought"),
      textInput("userstring", "A few words please:"),
      h4("Thank You for checking the application!!")
      
    ),
    
    
    # Main panel documentation text
    mainPanel(
     
      # Prediction texts
     h3(strong("What you typed : ")),
      verbatimTextOutput("inputValue"),
      h3(strong("Next word is :")),
      verbatimTextOutput("prediction"),
      
      # Model explanation    
      h3(strong("Purpose:")),
      h4("This is a webpage which I build using Shiny package for the final Capstone project 
         of Data Science course offered by John Hopkins University in co-operation with
         SwiftKey through Coursera. Here user types some word/words and my model tries 
         to predict the next possible word as suggestion."),
      
      h3(strong("Data:")),
      h4("The data to build the model is taken from the following link"),
      h5(em("Ref:https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")),
      
      h3(strong("Model Description:")),
      h4("* A data sample is formed from English language blog, twitter and news 
         source data from the given link and then it is cleaned using various packages of R."),
      h4("* Data tables are produced from this clean data using unigram, bigram, trigram
         models and these data tables are used to predict the next word." ),
      
      h3(strong("Input:")),
      h4("User typed word/words."),
      
      h3(strong("Output:")),
      h4("Predicted word.")
      )
    
      )
      ))
