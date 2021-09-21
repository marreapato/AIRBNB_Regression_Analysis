#install.packages("tidyverse")
library(tidyverse)
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

ggplot(dados,aes(y=dados$bedrooms,x=as.factor(dados$waterfront)))+geom_boxplot()
#analisar outliers

