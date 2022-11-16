library(tidyverse)
#install.packages("e1071")
library(e1071)
#install.packages("caret")
library(caret)
library(rpart)
library(rpart.plot)

setwd("C:/Users/kaiva/OneDrive/Desktop")
d = read.csv("accidents.csv")
View(d)

set.seed(2022)

d$INJURY = factor(ifelse(d$MAX_SEV == "fatal" | d$MAX_SEV == "non-fatal", "YES", "NO"))
View(d)
# Naive Bayes:
d$SPD_LIM = round(d$SPD_LIM/10)

str(d)

d$RushHour = factor(d$RushHour)
d$WRK_ZONE = factor(d$WRK_ZONE)
d$WKDY = factor(d$WKDY)
d$INT_HWY = factor(d$INT_HWY)
d$LGTCON_day = factor(d$LGTCON_day)
d$LEVEL = factor(d$LEVEL)
d$SPD_LIM = factor(d$SPD_LIM)
d$SUR_COND_dry = factor(d$SUR_COND_dry)
d$TRAF_two_way = factor(d$TRAF_two_way)
d$WEATHER_adverse = factor(d$WEATHER_adverse)
str(d)


train.data <- sample(c(1:dim(d)[1]), dim(d)[1]*0.6)  
train.df <- d[train.data, ]
valid.df <- d[-train.data, ]

traffic.nb = naiveBayes(INJURY ~ . -MAX_SEV, data = train.df)
traffic.nb


boxplot(traffic.nb)

pred.prob = predict(traffic.nb, newdata = valid.df, type = "raw")

pred.class = predict(traffic.nb, newdata = valid.df, type = "class")

df = data.frame(actual = valid.df$INJURY, predicted=pred.class, probabilities = pred.prob)
table(df)

df <- df[order(df$probabilities.NO), ]
View(df)

# training
pred.train <- predict(traffic.nb, newdata = train.df)
confusionMatrix(pred.train, train.df$INJURY, positive = "YES")
# Validation
pred.valid <- predict(traffic.nb, newdata = valid.df)
confusionMatrix(pred.valid, valid.df$INJURY, positive = "YES")

Accuracy : 0.5042
Sensitivity : 0.7281          
Specificity : 0.3016