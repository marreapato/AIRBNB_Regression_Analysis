library(tidyverse)
#install.packages("corrplot")
library(corrplot)
#install.packages("GGally")
library(GGally)
library(MASS)
#install.packages("lmtest")
library(lmtest)
#preço e tempo de construção da casa
dados <- read.csv("lab8.csv")

summary(dados)

cor(dados)
dados <- dados[,-1]

corrplot(cor(dados), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


ggpairs(dados, lower = list(continuous = "smooth"))

#################### regressao polinomial
#regressao polinomial usual
#q=2,3
#Q=2
fit <- lm(dados$preco~dados$tempo+ I(dados$tempo^2))
summary(fit)

plot(dados$tempo,dados$preco,main="Ajuste do Modelo")
lines(smooth.spline(dados$tempo,predict(fit)),col="red",lwd=1)

fit$residuals <- studres(fit)
plot(fit) 

#dados <- dados[-c(338),]
shapiro.test(fit$residuals)
#perform Breusch-Pagan Test
bptest(fit)

#Q=3
fit <- lm(dados$preco~dados$tempo+ I(dados$tempo^2)+ I(dados$tempo^3))
summary(fit)
plot(dados$tempo,dados$preco,main="Ajuste do Modelo")
lines(smooth.spline(dados$tempo,predict(fit)),col="red",lwd=1)

fit$residuals <- studres(fit)
plot(fit) 

#dados <- dados[-c(338),]
shapiro.test(fit$residuals)

#perform Breusch-Pagan Test
bptest(fit)

#regressão polinomial ortogonal
fit_ort <- lm(dados$preco~poly(dados$tempo,degree=2,raw=F))
summary(fit_ort)

plot(dados$tempo,dados$preco,main="Ajuste do Modelo")
lines(smooth.spline(dados$tempo,predict(fit_ort)),col="red",lwd=1)

fit_ort$residuals <- studres(fit_ort)
plot(fit_ort) 

#dados <- dados[-c(338),]
shapiro.test(fit_ort$residuals)

#perform Breusch-Pagan Test
bptest(fit_ort)


#regressão polinomial ortogonal q=3
fit_ort <- lm(dados$preco~poly(dados$tempo,degree=3,raw=F))
summary(fit_ort)

plot(dados$tempo,dados$preco,main="Ajuste do Modelo")
lines(smooth.spline(dados$tempo,predict(fit_ort)),col="red",lwd=1)

fit_ort$residuals <- studres(fit_ort)
plot(fit_ort) 


#dados <- dados[-c(338),]
shapiro.test(fit_ort$residuals)

#perform Breusch-Pagan Test
bptest(fit_ort)
