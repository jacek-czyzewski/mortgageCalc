mortgage <- function(P=900000, I=6, L=30, amort=TRUE, equal=TRUE) { 
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
  }
  # Calculate Amortization for each Month
  if(amort==TRUE) {
    Pt <- P # current principal or amount of the loan
    currP <- NULL
    while(Pt>=0) {
      H <- Pt * J # this is the current monthly interest
      C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
      Q <- Pt - C # this is the new balance of your principal of your loan
      Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
      currP <- c(currP, Pt)
    }
    monthP <- c(P, currP[1:(length(currP)-1)])-currP
    
    #The amortization data for each of the N months is stored in aDFmonth
    aDFmonth <- data.frame(
      Amortization=c(P, currP[1:(length(currP)-1)]), 
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
    barplot(t(aDFyear[,c(3,4)]), 
            col=c("blue", "red"), 
            main="Annual Interest and Principal Payments", 
            sub="The data for this plot is stored in aDFyear.",
            xlab="Years", ylab="$ Amount", 
            legend.text=c("Principal", "Interest"), 
            ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
  }
}

mortgage()


mortgage2 <- function(P=propertyValue-downPayment, I=interestRate, L=loanTerm, equal=equalOrDecreasing) { 
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
  
  Pt <- P # current principal or amount of the loan
  currP <- NULL
  while(Pt>=0) {
    H <- Pt * J # this is the current monthly interest
    C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
    Q <- Pt - C # this is the new balance of your principal of your loan
    Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
    currP <- c(currP, Pt)
  }
  monthP <- c(P, currP[1:(length(currP)-1)])-currP
  
  #The amortization data for each of the N months is stored in aDFmonth
  aDFmonth <- data.frame(
    Amortization=c(P, currP[1:(length(currP)-1)]), 
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
  
}