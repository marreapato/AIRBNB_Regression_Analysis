---
title: "Prova 2 - Planejamento de Experimentos"
author: "Lucas Rabelo"
output: 
  pdf_document:
    fig_caption: yes
---

\renewcommand{\tablename}{Tabela}


# Questão 2

```{r, echo = F, warning=F, include=F}

library(tidyverse)

trat <- c(rep("Aipim bravo",each=4),rep("Milagrosa",each=4),rep("Sutina",each=4),
          rep("Salango Preta",each=4),rep("Mamão",each=4),rep("Escondida",each=4))
trat <- factor(trat)

bloco <- rep(1:4,6)
bloco <- factor(bloco)
set.seed(15)

resposta <-c(14.5, 15.8, 24.0, 17.0,5.7, 5.9, 10.5, 6.6,  5.3, 7.7, 10.2, 9.6,
             4.6, 7.1,10.4, 10.8, 14.8, 12.6, 18.8, 16.0, 8.2, 8.2, 12.7, 17.5)

dados <- data.frame(bloco,trat,resposta)
head(dados)
str(dados)


```



## Breve análise descritiva

 Inicialmente temos dados de uma competição de variedades de mandioca, em blocos ao acaso, dentre algumas de suas medidas resumo estão que em média a resposta foi de 11,44, o valor máximo encontrado foi de 24, além disso o terceiro quartil indica que até 75% das respostas estão indo até 15,05. Além disso será assumido um nível de 5% de significância estatística.
 
```{r echo=FALSE, warning=FALSE, message=FALSE, fig.show="hold", results='show'}

tabela1 = dados$resposta %>% summary()
knitr::kable(as.matrix(round(tabela1,2)), align = "c", caption = "Medidas descritivas das variáveis")
```

 É interessante que os *Boxplots* na **Figura 1**, indicam que o Aipim Bravo teve a maior mediana registrada, bem como o maior valor de resposta, apesar da mediana do Mamão estar bem próxima da do Aipim Bravo.
 
```{r echo=FALSE,warning=FALSE,message=FALSE, fig.show="hold", out.width="50%",results='hide'}

#Boxplot
boxplot(resposta~trat,main="Boxplot Resposta x Variedades")
boxplot(resposta~bloco,main="Boxplot Resposta x Blocos")


```
**Figura 1 - **Boxplots das variedades e blocos.

 Para analisar se o delineamento dos blocos casualizados está bem feito, necessitamos checar seus pressupostos na **Figura 2**, onde aparentemente vemos que a homocedasticidade parece ser garantida no primeiro gráfico, e no QQPlot alguns pontos parecem fugir a normalidade do modelo, mas o teste de Shappiro indica que os resíduos são normais, o resultado do p-valor no teste de Shappiro-Wilk foi de 0,11, o resultado do p-valor no teste para homocedasticidade de variância de oneillmathews foi de 0,51.

```{r echo=FALSE,warning=FALSE,message=FALSE, fig.show="hold", out.width="50%",results='hide'}

modelo = lm(resposta~trat+bloco)

plot(modelo)

#Teste de normalidade
shapiro.test(rstandard(modelo)) 
```
**Figura 2 - ** Gráfico de Resíduos do DBC.

A anova do modelo indicou que os blocos não foram homogêneos a um nível de 5%, e indicou que pelo menos um dos tratamentos é diferente.

```{r echo=FALSE, warning=FALSE, message=FALSE, fig.show="hold", results='show'}

tabela2=anova(modelo)
knitr::kable(tabela2, align = "c", caption = "Anova do DBC")
```

Logo o delineamento em blocos não ficou bem feito, além disso pelo teste Tukey, realizado com a função ```dbc(trat,bloco,resposta)``` o Aipim Bravo e o Mamão estavam no mesmo grupo, não havendo um grupo isolado, talvez seria melhor rever o delineamento para este experimento.

##

# Apêndice


```{r ref.label=knitr::all_labels(), echo = T, eval = F}
```
