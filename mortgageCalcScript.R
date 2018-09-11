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