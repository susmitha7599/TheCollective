
setwd("C:/Users/kaiva/OneDrive/Desktop")
d = read.csv("accidents.csv")

d$INJURY = factor(ifelse(d$MAX_SEV == "fatal" | d$MAX_SEV == "non-fatal", "YES", "NO"))
d$MAX_SEV <- NULL
set.seed(2022)
train.index <- sample( c(1:dim(d)[1]), dim(d)[1]*0.6 )
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]

# Decision Trees:

default.ct <- rpart(INJURY ~ ., data = train.df, method = "class")

prp(default.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)
default.ct.pred.train <- predict(default.ct, train.df, type = "class")

confusionMatrix(default.ct.pred.train, as.factor(train.df$INJURY), positive = "YES")

default.ct.pred.valid <- predict(default.ct, valid.df, type = "class")

confusionMatrix(default.ct.pred.valid, as.factor(valid.df$INJURY), positive = "YES")

#summary(default.ct)

#Performance metrics output at time of execution
#Accuracy : 0.5125
# Sensitivity : 0.5789          
#Specificity : 0.4524
