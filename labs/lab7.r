#install.packages("tidyverse")
library(tidyverse)
library(MASS)

###########3regressao multipla
fumantes <- read.csv("lab7.csv")[,-1]

model <- lm(fumantes$charges~fumantes$bmi+fumantes$age)

plot(rstudent(model),x=model$fitted.values)

