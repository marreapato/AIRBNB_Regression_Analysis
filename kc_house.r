#install.packages("tidyverse")
library(tidyverse)
dados <- read.csv("kc_house_data.csv")

summary(dados)
mean(dados$bedrooms)
mean(dados$bathrooms)

hist(dados$bathrooms)
hist(dados$bedrooms)
