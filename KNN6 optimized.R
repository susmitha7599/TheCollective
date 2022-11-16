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

set.seed(2022)
train.rows <- sample(row.names(d.norm.df), 0.6*dim(d.norm.df)[1])  
valid.rows <- setdiff(row.names(d.norm.df), train.rows)  
train.df <- d.norm.df[train.rows, ]
valid.df <- d.norm.df[valid.rows, ]
knn6 = knn(train = train.df, test = valid.df , cl = outcome[train.rows,] , k=6, prob = TRUE)
confusionMatrix(knn6, outcome[valid.rows,], positive = "YES")

