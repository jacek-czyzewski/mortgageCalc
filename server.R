#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$contents1 <- renderText({ 
    mortgageTotalCost(P=input$propertyValue-input$downPayment, I=input$interestRate, L=input$loanTerm, equal=input$equalOrDecreasing)
  })
  
  mortgageTotalCost <- function(P,I,L,equal) { 
    J <- I/(12 * 100)
    N <- 12 * L
    
    if(equal==TRUE){
      M <- P*J/(1-(1+J)^(-N))
      
      # Monthly payment is stored in monthPay
      monthPay <- M
      TotalCost <- M*N
    }else{
      index <- 1:N
      M <- c()
      M[index] <- P/N*(1 + (N - index + 1)*J)
      TotalCost <- sum(M)
    }
    return(TotalCost)
  }
  
})
