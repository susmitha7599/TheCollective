
setwd("C:/Users/kaiva/OneDrive/Desktop")
d = read.csv("accidents.csv")

d$INJURY = factor(ifelse(d$MAX_SEV == "fatal" | d$MAX_SEV == "non-fatal", "YES", "NO"))
d$MAX_SEV <- NULL

set.seed(2022)

train.index <- sample( c(1:dim(d)[1]), dim(d)[1]*0.6 )
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]

# here cp value can be used to change the depth of the tree 

full.ct <- rpart(INJURY ~ ., data = train.df, method = "class", cp=0.014, minsplit = 1)
prp(full.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)
full.ct.pred.train <- predict(full.ct, train.df, type = "class")
full.ct.pred.valid <- predict(full.ct, valid.df, type = "class")
confusionMatrix(full.ct.pred.valid, as.factor(valid.df$INJURY), positive = "YES")

#Accuracy : 0.5208    
#Sensitivity : 0.5702           
#Specificity : 0.4762  
