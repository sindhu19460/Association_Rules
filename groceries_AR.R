groceries_data <- read.csv(file.choose())
View(groceries_data)
str(groceries_data)
groceries_data[] <- lapply(groceries_data,as.character)
View(groceries_data)
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}
groceries_data["new_col"] <- apply(groceries_data,1,paste_fun)
View(groceries_data)
library(tm)
x <- Corpus(VectorSource(groceries_data$new_col)) 
x <- tm_map(x,stripWhitespace)
dtm0 <- t(TermDocumentMatrix(x))
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)
library(arules)
library(arulesViz)
barplot(sapply(dtm0_df,sum),col=1:20)
grocery_rules <- apriori(as.matrix(dtm0_df), parameter = list(support = 0.01, confidence = 0.5))
inspect(grocery_rules)
plot(jitter=0,grocery_rules)
inspect(head(sort(grocery_rules, by = "confidence"), 3))

rules_conf <- sort(grocery_rules,by="confidence")
inspect(rules_conf)
rules_lift <- sort(grocery_rules,by="lift")
inspect(rules_lift)
?plot
plot(grocery_rules,method = "graph")
plot(grocery_rules, method="matrix")
plot(grocery_rules, method="matrix", engine = "3d")
plot(grocery_rules, method="matrix", control = list(reorder = "none"))
plot(grocery_rules, method="matrix", control = list(reorder = "support/confidence"))
plot(grocery_rules, method="matrix", control = list(reorder = "similarity"))

plot(grocery_rules, method="grouped matrix")
plot(grocery_rules, method="grouped matrix", 
     col = grey.colors(10), 
     gp_labels = gpar(col = "blue", cex=1, fontface="italic"))

