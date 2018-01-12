
library(shiny)
source("newmodel.R")

  
  shinyServer(
       function(input, output) {
              output$inputValue <- renderPrint({input$userstring})
              output$prediction <- renderPrint({predfun(input$userstring)})
               
                
            }
    )
  
 