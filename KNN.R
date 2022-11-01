
library(FNN)


# KNN:
d = read.csv("accidents.csv")

d$INJURY = factor(ifelse(d$MAX_SEV == "fatal" | d$MAX_SEV == "non-fatal", "YES", "NO"))

d$MAX_SEV <- NULL
outcome = as.data.frame(d[ , 11])
colnames(outcome) = "Injured"
outcome$Injured = as.factor(outcome$Injured)
View(outcome)

d= d[, -11]
table(outcome)
norm.values <- preProcess(d, method=c("center", "scale"))
d.norm.df <- predict(norm.values, d)

train.rows <- sample(row.names(d.norm.df), 0.6*dim(d.norm.df)[1])  
valid.rows <- setdiff(row.names(d.norm.df), train.rows)  
train.df <- d.norm.df[train.rows, ]
valid.df <- d.norm.df[valid.rows, ]
knn9 = knn(train = train.df, test = valid.df , cl = outcome[train.rows,] , k=9, prob = TRUE)
confusionMatrix(knn9, outcome[valid.rows,], positive = "YES")

Accuracy : 0.5125 
Sensitivity : 0.6639          
Specificity : 0.3636   
