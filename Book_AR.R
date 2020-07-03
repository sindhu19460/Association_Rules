library(rmarkdown)
library(arules)
library(arulesViz)

book <- read.csv(file.choose())

View(book)

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
rules

inspect(head(sort(rules, by = "lift")))
head(quality(rules))
plot(rules,method = "graph")
plot(rules, method="matrix")
plot(rules, method="matrix", engine = "3d")
plot(rules, method="matrix", control = list(reorder = "none"))
plot(rules, method="matrix", control = list(reorder = "support/confidence"))
plot(rules, method="matrix", control = list(reorder = "similarity"))

plot(rules, method="grouped matrix")
plot(rules, method="grouped matrix", 
     col = grey.colors(10), 
     gp_labels = gpar(col = "blue", cex=1, fontface="italic"))
