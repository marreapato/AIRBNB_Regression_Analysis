#install.packages("tidyverse")
#install.packages("ggthemes")
library(tidyverse)
library(ggthemes)
library(MASS)
library(lmtest)
dados <- read.csv("Dummy Data HSS.csv")
options(scipen = 999)

# Medidas descritivas

# Organizando os dados
dados <- na.omit(dados)
dados$Influencer = as.factor(dados$Influencer)

tabela1 = dados %>% summary(columns = 2:5)

# Histogramas
plot5 = ggplot(dados, aes(x = TV)) +
  geom_histogram(color = "white", fill = "lightblue") +
  theme_classic(base_size = 18) +
  #scale_x_continuous(breaks = seq(from = 4,to = 8,by = 1), limits = c(4,8)) +
  ggtitle("TV") +
  xlab("Investimento em milhões ($)") + 
  ylab("Frequência")

plot6 = ggplot(dados, aes(x = Radio)) +
  geom_histogram(color = "white", fill = "lightblue") +
  theme_classic(base_size = 18) +
  #scale_x_continuous(breaks = seq(from = 4,to = 8,by = 1), limits = c(4,8)) +
  ggtitle("Radio") +
  xlab("Investimento em milhões ($)") + 
  ylab("Frequência")

plot7 = ggplot(dados, aes(x = Social.Media)) +
  geom_histogram(color = "white", fill = "lightblue") +
  theme_classic(base_size = 18) +
  #scale_x_continuous(breaks = seq(from = 4,to = 8,by = 1), limits = c(4,8)) +
  ggtitle("Mídias Sociais") +
  xlab("Investimento em milhões ($)") + 
  ylab("Frequência")

plot8 = ggplot(dados, aes(x = Sales)) +
  geom_histogram(color = "white", fill = "lightblue") +
  theme_classic(base_size = 18) +
  #scale_x_continuous(breaks = seq(from = 4,to = 8,by = 1), limits = c(4,8)) +
  ggtitle("Vendas") +
  xlab("Investimento em milhões ($)") + 
  ylab("Frequência")

par(mfrow=c(1,4))
plot5
plot6
plot7
plot8

#Gráfico de Barras com preenchimento colorido
# gráfico de contagem

dados %>% 
  count(Influencer) %>%
  filter(!is.na(Influencer)) %>% 
  top_n(10, n) %>%
  ggplot() +
  geom_col(
    aes(x = Influencer, y = n, fill = Influencer),
    show.legend = FALSE
  )

########################################

#modelos e checagem de outliers

dados_reg_semcat <- na.omit(dados)
dados_reg_semcat$Influencer <- as.factor(dados_reg_semcat$Influencer)

modelo <- step(lm(dados_reg_semcat$Sales~.,data=dados_reg_semcat),direction="forward")
modelo$coefficients

summary(modelo)
anova(modelo)
R2_mod1 <- 0.999
aic_mod1 <- AIC(modelo)
bic_mod1 <- BIC(modelo)

modelo$residuals <- studres(modelo)

plot(modelo)

shapiro.test(modelo$residuals)
bptest(modelo)


library(car)

methods(class="lm")

olsrr::ols_plot_dffits(modelo)

olsrr::ols_plot_resid_lev(modelo)

###############################333


dados_reg_semcat <- na.omit(dados)
dados_reg_semcat$Influencer <- as.factor(dados_reg_semcat$Influencer)

modelo <- step(lm(dados_reg_semcat$Sales~.,data=dados_reg_semcat),direction="backward")
modelo$coefficients

summary(modelo)
R2_mod2 <- 0.999
anova(modelo)
aic_mod2 <- AIC(modelo)
bic_mod2 <- BIC(modelo)

modelo$residuals <- studres(modelo)
plot(modelo)

shapiro.test(modelo$residuals)
bptest(modelo)

library(car)

methods(class="lm")

olsrr::ols_plot_dffits(modelo)

olsrr::ols_plot_resid_lev(modelo)

#######################################################33



