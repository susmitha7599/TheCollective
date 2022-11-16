library(ggplot2)
setwd("C:/Users/kaiva/OneDrive/Desktop")
d = read.csv("accidents.csv")
ggplot(d, aes(x=as.factor(MAX_SEV)))+
  geom_bar(color = "blue", fill = rgb(0.1,0.4,0.5,0.7))+
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position = "none")
ggplot(d, aes(x= SPD_LIM, fill= MAX_SEV))+
  geom_histogram(position = "dodge")
d$MAX_SEV <- NULL
d= d[, -11]
library(reshape2)
#install.packages("hrbrthemes")
library(hrbrthemes)
cormat = round(cor(d),2)
melted_cormat <- melt(cormat)   
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_distiller(palette = "RdPu") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme_ipsum()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed()
heatmap
boxplot(data = melted_cormat, value~Var1)

