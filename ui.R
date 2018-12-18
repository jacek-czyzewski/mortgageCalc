#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(reshape2)
library(dplyr)
library(DT)
# library(flexdashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mortgage calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("propertyValue",
                   "Property Value in PLN:",
                   min = 100000,
                   max = 2000000,
                   step = 10000,
                   value = 500000),
       
       uiOutput("uiDownPayment"),
       
       radioButtons("equalOrDecreasing", label = "Equal/Decreasing Installments",
                    choices = list("Equal" = TRUE, "Decreasing" = FALSE), 
                    selected = TRUE),
       
       radioButtons("variableOrFixed", label = "Interest rate",
                    choices = list("Variable" = 1, "Fixed" = 2), 
                    selected = 1),
       
       sliderInput("interestRate",
                   "Interest rate:",
                   min = 0,
                   max = 10,
                   value = 3,
                   round = FALSE,
                   step = 0.25,
                   post = " %"),
       
       sliderInput("originationFee",
                   "Origination fee:",
                   min = 0,
                   max = 10,
                   value = 1.5,
                   step = 0.1,
                   post = " %"),
       
       sliderInput("loanTerm",
                   "Loan term in years:",
                   min = 1,
                   max = 30,
                   value = 15,
                   step = 1),
       
       dateInput('startDate',
                 label = 'Loan start date: yyyy-mm-dd',
                 value = Sys.Date()
       )
                   
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
      navbarPage(title="", collapsible = TRUE,
        tabPanel("Summary",
                            fluidPage(
                              fluidRow(
                                # Values should be put in ValueBox function from shinydashboard!
                                column(12, h1("Your Mortgage Summary", align="center")),
                              fluidRow(
                                column(12, h3(textOutput("contents1"))),
                                column(12, h3(textOutput("contents2"))),
                                column(12, h3(textOutput("contents3")))),
                              fluidRow(
                                column(12, plotlyOutput("barplot2")))
                              ))),
        tabPanel("Sensitivity analysis", plotOutput("testplot")),
        tabPanel("Your mortgage details", dataTableOutput("mortgageDetails"), downloadButton("dltab", "Download")),
        tabPanel("Variable vs fixed rate", plotOutput("barplot1")),
        tabPanel("Equal vs decreasing installments", plotlyOutput("barplot3"))
      )
    )

    
  )
))
