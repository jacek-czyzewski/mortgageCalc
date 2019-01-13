library(XML)
url <- "http://www.nbp.pl/xml/stopy_procentowe.xml"
doc <- xmlParse(url)
data <- xmlToList(doc)
stopa <- xpathSApply(doc, '//stopy_procentowe/id', xmlValue)

lat <- xpathSApply(data, '//marker/lat', xmlValue)
rootnode <- xmlRoot(doc)
print(rootnode[1])
print(rootnode[[1]][[1]])
stopa <- xpathSApply(doc, "//id", xmlValue)
stopa1 <- grep("Referencyjna", doc, value=TRUE)

class(doc)
topxml <- xmlRoot(doc)
topxml <- xmlSApply(topxml,
                    function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(topxml),
                     row.names=NULL)


data <- xmlParse("http://www.nbp.pl/xml/stopy_procentowe.xml")
xmldf <- xmlToDataFrame(url)
tableNodes = getNodeSet(data, "//table")
tb = readHTMLTable(tableNodes[[2]])
head(data)
data

credit <- data.frame()

output$barplot3 <- renderPlotly({
  # aDFyear <- mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(), equal=equalOrDecreasing(), D=downPayment())[[2]]
  aDFyear <- mortgageData()[[2]]
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

output$barplot1 <- renderPlot({
  # aDFyear <- mortgage2(P=propertyValue(), I=interestRate(), L=loanTerm(), equal=equalOrDecreasing(), D=downPayment())[[2]]
  aDFyear <- mortgageData()[[2]]
  barplot(t(aDFyear[,c(3,4)]), 
          col=c("blue", "red"), 
          main="Annual Interest and Principal Payments", 
          xlab="Years", ylab="$ Amount", 
          legend.text=c("Principal", "Interest"), 
          ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
  
})