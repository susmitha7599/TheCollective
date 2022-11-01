

d = read.csv("accidents.csv")

d$INJURY = factor(ifelse(d$MAX_SEV == "fatal" | d$MAX_SEV == "non-fatal", "YES", "NO"))
d$MAX_SEV <- NULL

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

summary(default.ct)

Accuracy : 0.525 
Sensitivity : 0.5338          
Specificity : 0.5140 
