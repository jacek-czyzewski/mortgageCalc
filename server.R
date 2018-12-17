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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
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
  
  output$contents1 <- renderText({ 
    paste("Total Cost:", format(mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(),
                                   equal=equalOrDecreasing(), D=downPayment())[[1]], nsmall=2, big.mark=","), "PLN", sep=" ")
    # mortgage3()[[1]]
  })
  
  output$contents2 <- renderText({ 
    paste("Total Interest:", format(sum(mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(),
                                          equal=equalOrDecreasing(), D=downPayment())[[2]]$Annual_Interest), nsmall=2, big.mark=","), "PLN", sep=" ")
    # mortgage3()[[1]]
  })
  
  output$contents3 <- renderText({
    paste("Date of last payment:", as.Date(startDate() + loanTerm()*365))
  })
  
  output$XYZ <- renderText({c(input$downPayment, input$interestRate, input$propertyValue, input$loanTerm, input$equalOrDecreasing)})
  testdata <- data.frame(x=1, y=1)
  
  output$testplot <- renderPlot(ggplot(testdata, aes(x=x, y=y)) + geom_point())
  
  output$barplot1 <- renderPlot({
    aDFyear <- mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(), equal=equalOrDecreasing(), D=downPayment())[[2]]
    # aDFyear <- mortgage3()[[2]]
    barplot(t(aDFyear[,c(3,4)]), 
            col=c("blue", "red"), 
            main="Annual Interest and Principal Payments", 
            sub="The data for this plot is stored in aDFyear.",
            xlab="Years", ylab="$ Amount", 
            legend.text=c("Principal", "Interest"), 
            ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
    
  })
  
  output$barplot2 <- renderPlotly({
    aDFyear <- mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(), equal=equalOrDecreasing(), D=downPayment())[[2]]
    # aDFyear <- mortgage3()[[2]]
    aDFyear$Balance <- sum(aDFyear$Annual_Payment)-cumsum(aDFyear$Annual_Payment)
    df3 <- select(aDFyear, "Annual_Principal", "Annual_Interest", "Year")
    names(df3) <- c("Annual Principal", "Annual Interest", "Year")
    df3_melted <- as.data.frame.array(melt(df3, measure.vars = c("Annual Principal", "Annual Interest")))
    barplot2 <- ggplot(df3_melted, aes(x=Year, y=value, fill=variable)) + geom_bar(stat="identity") +
      ylab("Payment Value") +
      scale_fill_discrete(
        name="Instalment\nComponents",
        breaks=c("Annual_Principal", "Annual_Interest"),
        labels=c("Annual Principal", "Annual Interest")
      )
    barplotly2 <- ggplotly(barplot2)
    layout(barplotly2, autosize=TRUE, margin = list(l=100, t=70))
  })
  
  output$barplot3 <- renderPlotly({
    aDFyear <- mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(), equal=equalOrDecreasing(), D=downPayment())[[2]]
    # aDFyear <- mortgage3()[[2]]
    aDFyear$Balance <- sum(aDFyear$Annual_Payment)-cumsum(aDFyear$Annual_Payment)
    df3 <- select(aDFyear, "Annual_Principal", "Annual_Interest", "Year")
    names(df3) <- c("Annual Principal", "Annual Interest", "Year")
    df3_melted <- as.data.frame.array(melt(df3, measure.vars = c("Annual Principal", "Annual Interest")))
    barplot3 <- ggplot(df3_melted)  + geom_bar(aes(x=Year, y=value, fill=variable), stat="identity") + 
      ylab("Payment Value") +
      scale_fill_discrete(name="Instalment\nComponents",
        breaks=c("Annual_Principal", "Annual_Interest"),
        labels=c("Annual Principal", "Annual Interest")) + 
      geom_line(data=aDFyear, aes(x=Year, y=Balance/max(Year)))+
      # scale_color_manual(NULL, values = "black") +
      scale_y_continuous(sec.axis = sec_axis(trans = ~ . * max(aDFyear$Year), name="Mortgage Balance Value"))

    barplotly3 <- ggplotly(barplot3)
    layout(barplotly3, autosize=TRUE, margin = list(l=100, t=70))
  })
  
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
  
  mortgage3 <- reactive({
    P <- input$propertyValue
    I <- input$interestRate
    L <- input$loanTerm
    equal <- input$equalOrDecreasing
    D <- input$downPayment
    
    J <- I/(12 * 100)
    N <- 12 * L
    
    if(equal==TRUE){
      M <- (P-D)*J/(1-(1+J)^(-N))
      
      # Monthly payment is stored in monthPay
      monthPay <- M
      TotalCost <- M*N
      
      Pt <- (P-D) # current principal or amount of the loan
      currP <- NULL
      
      while(Pt>=0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P-D, currP[1:(length(currP)-1)])-currP
      
      }else{
      index <- 1:N
      M <- c()
      M[index] <- (P-D)/N*(1 + (N - index + 1)*J)
      monthPay <- M
      TotalCost <- sum(M)
      
      Pt <- (P-D)  # current principal or amount of the loan
      currP <- NULL
      
      i = 1
      while(Pt>=0) {
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
    
})

    
})
