library(tidyverse)
#install.packages("corrplot")
library(corrplot)

dados <- read.csv("lab8.csv")

summary(dados)

cor(dados)
dados <- dados[,-1]

corrplot(cor(dados), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
