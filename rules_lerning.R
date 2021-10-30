library(arules)
library(arulesViz)
interests <- read.transactions("teen_interests.csv",sep = ",")
summary(interests)
itemFrequencyPlot(interests,topN = 20)

rules <- apriori(interests,parameter = list(support=0.006,
                                            confidence=0.5,
                                            minlen=2))
rules
inspect(head(sort(rules,by="lift")))
sort(itemFrequency(items(rules)),decreasing = TRUE)
plot(sort(itemFrequency(items(rules)),decreasing = TRUE))
plot(rules,measure = c("support","lift"),shading = "confidence",jitter = 0)
plot(rules,measure = c("confidence","lift"),jitter=0)
plot(rules,measure = c("support","confidence"),shading = "lift",jitter=0)
sel <- plot(rules,measure = c("support","lift"),shading = "confidence",jitter=0,interactive = TRUE)
subrules <- rules[quality(rules)$confidence >= 0.8]
inspect(sort(subrules,by="lift")[1:30])
plot(subrules,method = "grouped")
subrules2 <- sort(subrules,by="lift")[1:30]
plot(subrules2,method = "grouped")
plot(subrules2,method = "graph")
plot(subrules2, method="paracoord",control = list(reorder=TRUE))
inspect(subrules2)
