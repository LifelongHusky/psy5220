##### LIBRARIES #####
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(tidyverse)
library(tm)
library(randomForest)
library(tokenizers)
library(mclust)

# Bring in file with all the ML models & training
source("ml.R")


##### SHINY APP UI #####
ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Emotion Predictor"),
  sidebarLayout(
    sidebarPanel(
      # Select text representation / feature technique
      selectInput(inputId = "txtrep", 
                  label = strong("Text Representation/Feature Engineering"),
                  choices = c('DTM', 'Ngrams'),
                  selected = 'DTM'),
      # Select Classification Model
      selectInput(inputId = "model", 
                  label = strong("Classification Model"),
                  choices = c('Random Forest', 'MClustDA'),
                  selected = "Random Forest"),      
      # User Text Input
      textInput(inputId = 'usrtxt', label = strong('Input Text')),
      actionButton('predbtn','Predict')
      ),
    # Output: prediction text, bar chart of probabilities or accuracy?
    mainPanel(
      h3("Predicted Emotion: "),
      textOutput(outputId = "txtpred")
      )
  )
)

##### SHINY APP - SERVER #####
server <- function(input, output) {
  
  observeEvent(input$predbtn, {
    if(input$usrtxt == ''){
      # Give User warning
      show_alert(
        title = "Error",
        text = "No text provided.",
        type = "error"
      )
    } else {
      # make the prediction
      prediction <- NULL
      newText <- input$usrtxt |> str_to_lower() |> removePunctuation()
      newData <- NULL
      currModel <- NULL
      
      # Convert user inputted data into the proper representation
      if(input$txtrep == 'DTM'){
        
        # Make zero df
        newData <- data.frame(rep(0,length(train.dtm.terms))) |> 
          transpose() |> as.data.frame()
        # Give proper names to the columns
        colnames(newData) <- train.dtm.terms
        
        # Fill with proper values
        for(i in 1:ncol(newData)){
          newData[1,i] <- grepl(train.dtm.terms[i], newText)
        }
        
        # Get prediction for the proper model
        if(input$model == 'Random Forest'){
          prediction <- predict(rf.dtm, newdata = newData)
        }
        else if(input$model == 'MClustDA'){
          prediction <- predict(mcda.dtm, newdata = newData)
        }
        
      }
      else if(input$txtrep == 'Ngrams'){
        # tokenize for ngrams of n=2:4
        newText <- newText |> tokens() |> 
          tokens_ngrams(n = 2:4, concatenator = '.')
        
        # Make zero df
        newData <- matrix(0, ncol = length(train.ngram.terms), 
                          nrow = length(newText[[1]])) |> as.data.frame()
        colnames(newData) <- train.ngram.terms
        rownames(newData) <- newText[[1]]
        
        # Fill in the values
        for(i in 1:ncol(newData)){
          newdata2[1,i] <- grepl(ngrams[i], rownames(newdata2)[1])
          newdata2[2,i] <- grepl(ngrams[i], rownames(newdata2)[2])
          newdata2[3,i] <- grepl(ngrams[i], rownames(newdata2)[3])
        }
        
        # Get prediction for the proper model
        if(input$model == 'Random Forest'){
          prediction <- predict(rf.ngram, newdata = newData)
        }
        else if(input$model == 'MClustDA'){
          prediction <- predict(mcda.ngram, newdata = newData)
        }
      }

      
      # render output text
      output$txtpred <- renderText({
        paste(prediction[[1]])
      })
    }
  })
}

##### SHINY APP - CREATE OBJECT #####
shinyApp(ui = ui, server = server)