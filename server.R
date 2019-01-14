#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(plotly)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  mortgage2 <- function(P, I, L, equal, D) { 
    J <- I/(12 * 100)
    N <- 12 * L
    Pt <- (P-D) # current principal or amount of the loan
    
    if(equal==TRUE){
      M <- (P-D)*J/(1-(1+J)^(-N))
      
      # Monthly payment is stored in monthPay
      monthPay <- M
      TotalCost <- M*N
    }else{
      index <- 1:N
      M <- c()
      M[index] <- (P-D)/N*(1 + (N - index + 1)*J)
      monthPay <- M
      TotalCost <- sum(M)
    }
    
    currP <- NULL
    if(equal==TRUE){
      req(Pt)
      while(Pt>=0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P-D, currP[1:(length(currP)-1)])-currP
    }else{
      i = 1
      req(Pt)
      while(Pt>=0&&i<=N) {
        H <- Pt * J # this is the current monthly interest
        C <- M[i] - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
        i=i+1
      }
      monthP <- c(P-D, currP[1:(length(currP)-1)])-currP
    }
    
    #The amortization data for each of the N months is stored in aDFmonth
    aDFmonth <- data.frame(
      Amortization=c(P-D, currP[1:(length(currP)-1)]), 
      Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
      Monthly_Principal=monthP, 
      Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0), 
      Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
    )
    #The amortization data for each of the L years is stored in aDFyear
    aDFyear <- data.frame(
      Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max), 
      Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum), 
      Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum), 
      Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum), 
      Year=as.vector(na.omit(unique(aDFmonth$Year)))
    )
    return(list(TotalCost, aDFyear, aDFmonth))
    
  }  
   
  output$uiDownPayment <- renderUI({
    
    sliderInput(inputId = "downPayment",
                "Down payment in PLN:",
                min = 0,
                max = input$propertyValue*0.8,
                step = 10000,
                value = input$propertyValue*0.1)
  })
   
  propertyValue <- reactive({input$propertyValue})
  downPayment <- reactive({input$downPayment})
  interestRate <- reactive({input$interestRate})
  loanTerm <- reactive({input$loanTerm})
  equalOrDecreasing <- reactive({input$equalOrDecreasing})
  startDate <- reactive({input$startDate})
  
  # if(is.null(propertyValue()|downPayment()|interestRate()|loanTerm()|equalOrDecreasing())){return(NULL)}
  mortgageData <- reactive({mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(),
                                      equal=equalOrDecreasing(), D=downPayment())})

# if(is.null(mortgageData())){return(NULL)}

  output$contents1 <- renderText({
    paste("Total Amount:", format(mortgageData()[[1]], nsmall=2, digits = 2, big.mark=","), "PLN", sep=" ")
  })
  
  output$contents2 <- renderText({ 
    paste("Total Interest:", format(sum(mortgageData()[[2]]$Annual_Interest), nsmall=2, big.mark=","), "PLN", sep=" ")
  })
  
  output$contents3 <- renderText({
    paste("Date of last payment:", as.Date(startDate() + loanTerm()*365))
  })

  output$contents4 <- renderText({
    paste("Your first monthly payment:", format(mortgageData()[[3]]$Monthly_Payment[1], nsmall = 2, digits = 2, big.mark=","), "PLN", sep=" ")
  })
  
  mortgageDetailsData <- reactive({mortgageData()[[as.numeric(input$annualOrMonthly)]]})
  output$mortgageDetails <- renderDataTable(datatable(mortgageDetailsData()) %>% formatRound(1:4, 2))
  
  output$dltab<-downloadHandler(
    filename=function(){
      paste("Mortgage-", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(mortgageDetailsData(), file)
    }
  )
  
  output$piechart <- renderPlotly({
    aDFyear <- mortgageData()[[2]]
    Interest <- sum(aDFyear$Annual_Interest)
    Principal <- sum(aDFyear$Annual_Principal)
    myValues <- c(Interest, Principal)
    myLabels <- c("Total Interest", "Total Principal")
    plot_ly(labels = ~myLabels, values = ~myValues, type = 'pie', textposition = 'inside', textinfo = 'label')
})
  
  output$barplot2 <- renderPlotly({
    aDFyear <- mortgageData()[[2]]
    aDFyear$Balance <- sum(aDFyear$Annual_Payment)-cumsum(aDFyear$Annual_Payment)
    df3 <- select(aDFyear, "Annual_Principal", "Annual_Interest", "Year")
    names(df3) <- c("Annual Principal", "Annual Interest", "Year")
    df3_melted <- as.data.frame.array(melt(df3, measure.vars = c("Annual Principal", "Annual Interest")))
    df3_melted$myText <- paste("Year: ", df3_melted$Year, "\n", "Value: ", format(round(df3_melted$value, 2), big.mark = " "), "\n", "Variable: ", df3_melted$variable)
    barplot2 <- ggplot(df3_melted, aes(x=as.factor(Year), y=value, fill=variable, text=myText)) + geom_bar(stat="identity") +
      ylab("Payment Value") +
      xlab("Year") +
      scale_fill_discrete(
        name="Instalment\nComponents",
        breaks=c("Annual_Principal", "Annual_Interest"),
        labels=c("Annual Principal", "Annual Interest")
      )
    barplotly2 <- ggplotly(barplot2, tooltip="text")
    layout(barplotly2, autosize=TRUE, margin = list(l=100, t=70))
  })
 

    
})
