
library(arules) 	
library(arulesViz) 	
library(tidyverse) 	
library(plyr)	
library(ggplot2) 	
library(knitr) 	
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(DT)
library(ggcorrplot)


retail <- read.csv("OnlineRetail.csv")
retail <- retail[complete.cases(retail), ]




retail %>% mutate(Description = as.factor(Description))

retail %>% mutate(Country = as.factor(Country))

retail$Date <- as.Date(retail$InvoiceDate)

TransTime<- format(retail$InvoiceDate,Format = "%H:%M:%S")

InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

cbind(retail,TransTime)

cbind(retail,InvoiceNo)




transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))

transactionData$InvoiceNo <- NULL

transactionData$Date <- NULL

colnames(transactionData) <- c("items")

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)

tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')






itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")



itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")



association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))

inspect(association.rules[1:10])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))

subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) 

length(subset.rules)

subset.association.rules. <- association.rules[-subset.rules]

metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))

inspect(head(metal.association.rules))

metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))

inspect(head(metal.association.rules))

subRules<-association.rules[quality(association.rules)$confidence>0.4]

plot(subRules)



plot(subRules,method="two-key plot", jitter= 0)

plotly_arules(subRules, jitter= 0)


top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")



  

subRules2<-head(subRules, n=20, by="lift")

plot(subRules2, method="paracoord")



summary(association.rules)


