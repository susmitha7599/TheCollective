
d = read.csv("accidents.csv")


d$INJURY = factor(ifelse(d$MAX_SEV == "fatal" | d$MAX_SEV == "non-fatal", "YES", "NO"))
d$MAX_SEV <- NULL

train.index <- sample( c(1:dim(d)[1]), dim(d)[1]*0.6 )
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]


#Logistic Regression:

options(scipen=999)
logit.reg <- glm(INJURY ~ ., data = train.df, family = "binomial")

summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

logit.reg.pred.class <- factor(ifelse(logit.reg.pred >= 0.5, "YES", "NO"))
confusionMatrix(logit.reg.pred.class, as.factor(valid.df$INJURY), positive = "YES")

Accuracy : 0.5042
Sensitivity : 0.7589          
Specificity : 0.2812
