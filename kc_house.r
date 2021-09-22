#install.packages("tidyverse")
#install.packages("ggthemes")
library(tidyverse)
library(ggthemes)
dados <- read.csv("kc_house_data.csv")

summary(dados)
mean(dados$bedrooms)
mean(dados$bathrooms)

hist(dados$bathrooms)
hist(dados$bedrooms)
hist(dados$sqft_basement)
hist(dados$sqft_living)
hist(dados$sqft_above)
hist(dados$grade)

ggplot(dados,aes(y=dados$bedrooms,x=as.factor(dados$waterfront),fill=as.factor(dados$waterfront)))+geom_boxplot()+theme_fivethirtyeight()+labs(title="Vista para lagos ou rios")+scale_x_discrete(labels=c("Não Possui","Possui"))+scale_fill_fivethirtyeight()+theme(legend.position = "none") 
#analisar outliers
