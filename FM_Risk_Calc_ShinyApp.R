############################################################
##  Author: Ashley Howard                                 
##          ashley.howard@duke.edu                        
##                                                        
##  Program: FM_Risk_Calc_ShinyApp.R                                 
##  Purpose: This program creates an R Shiny app that predicts 
##            risk for fungal or mycobacterial infection
##            based on our logistic regression model 
##
##  Input files:   File path removed for confidentiality
##  Output files:    NA
##  Activity log: 8/15/2024 - App created             
############################################################


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

# Define the User Interface (UI) of the app
# This is what users will see
ui = fluidPage(
  # Set title of app
  titlePanel("Fungal and Mycobacterial Risk Calculator"),
  
  # selectInput allows users to select either M or F
  # numericInput allows users to submit numeric values for lab data
  # set minimum to 0 to ensure no negative values are entered 
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Sex:", choices = c("M", "F")),
      numericInput("age", "Age (years):", value = NULL, min = 0),
      numericInput("haemoglobin", "Hemoglobin (g/L): ", value = NULL, min = 0),
      numericInput("platelet", "Platelet (10^9/L):", value = NULL, min = 0),
      numericInput("cd4", "CD4 (cells/mm³):", value = NULL, min = 0),
      numericInput("ast", "AST (U/L):", value = NULL, min = 0),
      numericInput("alt", "ALT (U/L):", value = NULL, min = 0),
      # Button to trigger prediction
      actionButton("predict", "Predict")
    ),
    
    mainPanel(
      verbatimTextOutput("results") # Output area for displaying results
    )
  )
)

# Define server logic
# This runs the model and calculates prediction on the back-end
server = function(input, output) {
  # Load the modeling data from the CSV file
  modeling_data = read.csv("C:/Users/aeh126/Box/CFAR Summer 2024_Le_Howard/data/Derived/modeling_data.csv",
                            header = TRUE,
                            stringsAsFactors = FALSE)
  
  # Refit the model
  logit_model_glm = glm(FUNGAL_MYCO_OUTCOME_ML ~ SEX + AGE +
                           HAEMOGLOBIN + PLAT + LOG_CD4_IMPUTED +
                           LOG_AST + LOG_ALT, 
                         family = binomial, 
                         data = modeling_data)
  
  observeEvent(input$predict, {
    new_data = data.frame(
      SEX = input$sex,  # Directly use the character input for SEX
      AGE = as.numeric(input$age),
      HAEMOGLOBIN = as.numeric(input$haemoglobin),
      PLAT = as.numeric(input$platelet),
      # log-transform CD4, AST, and ALT
      LOG_CD4_IMPUTED = log(as.numeric(input$cd4)),
      LOG_AST = log(as.numeric(input$ast)),
      LOG_ALT = log(as.numeric(input$alt))
    )
    
    # Ensure data is entered for all variables
    if (any(is.na(new_data))) {
      output$results = renderPrint({
        "Please enter valid numeric values for all inputs."
      })
      return()
    }
    
    # Predict values from logistic regression model 
    predictions = predict(logit_model_glm, new_data, type = "response")
    
    # Output prediction as a percent
    output$results = renderPrint({
      paste("Probability of fungal or mycobacterial infection:",
            round(100*predictions, 1),"%. Screening is recommended for predictions greater than 23%")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
