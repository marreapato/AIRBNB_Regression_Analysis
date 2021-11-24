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

# Questão 3

Com o objetivo de estudar os efeitos de vários fatores na tintura para um tecido misto de fibra de algodão e de fibra sintética, usado na fabricação de camisas, dentre algumas medidas descritivas tivemos uma média de 31,2, e o terceiro quartil indicando que 75% das observações tem um valor de resposta até 36. este comportamento fica claro na **tabela 3** abaixo. Além disso outras medidas descritivas como a mediana indicam que até metáde está indo até 34.

```{r, echo = F, warning=F, include=F}

operador <- c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,
                  3,1,1,1,2,2,2,3,3,3)

tempo <- c(10, 10, 10, 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,15,15,15,15,15,15,15,15,15,15,15,
                 15,15,15,15,15,15,15,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20)

resposta<- c(23,24,25,27,28,26,31,32,28,24,23,28,38,36,35,34,36,39,36,35,36,34,38,39,33,34,35,37,39,
                   35,34,38,36,34,36,31,28,24,27,35,35,34,26,27,25,26,29,25,36,37,34,28,26,34)

temperatura <- c(300,300,300,300,300,300,300,300,300,350,350,350,350,350,350,350,350,350,300,300,300,300,
               300,300,300,300,300,350,350,350,350,350,350,350,350,350,300,300,300,300,300,300,300,300,
              300,350,350,350,350,350,350,350,350,350)


dados <- data.frame(operador,tempo,resposta,temperatura)


### Configurando as variaveis

#### Fator 1
fatorTempo = dados$tempo
is.factor(fatorTempo)

fatorTempo = as.factor(fatorTempo)
is.factor(fatorTempo)

#### Fator 2
fatorTemperatura = dados$temperatura
is.factor(fatorTemperatura)

fatorTemperatura = as.factor(fatorTemperatura)
is.factor(fatorTemperatura)

#### Fator 3
fatorOperador = dados$operador
is.factor(fatorOperador)

fatorOperador = as.factor(fatorOperador)
is.factor(fatorOperador)

#### Resposta
resposta = dados$resposta
resposta
```

$\ $
$\ $
$\ $
$\ $
$\ $
$\ $
$\ $
$\ $
$\ $
$\ $
$\ $
$\ $


```{r echo=FALSE, warning=FALSE, message=FALSE, fig.show="hold", results='show'}

tabela1 = dados$resposta %>% summary()
knitr::kable(as.matrix(round(tabela1,2)), align = "c", caption = "Medidas descritivas das variáveis")


```

Na contagem de fatores tivemos 3 tratamentos de cada combinação, além disso na **Figura 3**, para o fator operador tivemos três valores atípicos no segundo nível, parece haver visualmente alguma diferença neste nível.

```{r echo=FALSE,warning=FALSE,message=FALSE, fig.show="hold", out.width="50%",results='hide'}
#Fator 1
boxplot(resposta~fatorTempo)

#Fator 2
boxplot(resposta~fatorTemperatura)

#Fator 3
boxplot(resposta~fatorOperador)

```

**Figura 3 - ** Boxplots dos Fatores.

Para o delineamento tivemos na tabela Anova, após utilizar a função ```fat3.dic()```,
do pacote ExpDes.pt, n´´os tivemos interações significativas a um nível de 5% de significância entre os fatores Tempo do Ciclo e Temperatura, Tempo do Ciclo e Operador,
Tempo do Ciclo e Temperatura e Operador. Além disso, também a um nível de 5% de significãncia, de forma independente os efeitos de todos os 3 fatores tem alguma influência sobre a variável reposta. Além de passarem nos testes de normalidade dos resíduos e homocedasticidade. Na **Figura 4**, vemos que os pressupostos de homocedasticidade e normalidade dos resíduos foram atingidos.


```{r echo=FALSE,warning=FALSE,message=FALSE, fig.show="hold", out.width="90%",results='hide'}

# Usando o ExpDes
require(ExpDes.pt)

modelo <- fat3.dic(fator1 = fatorTempo,
                   fator2 = fatorTemperatura,
                   fator3 = fatorOperador,
                   resp = resposta,
                   fac.names = c("Tempo do Ciclo","Temperatura", "Operador"))


# Analise dos residuos
plotres(modelo)

```

**Figura 3 - ** Análise de Resíduos do Delineamento de Fatores.

# Apêndice


```{r ref.label=knitr::all_labels(), echo = T, eval = F}
```
