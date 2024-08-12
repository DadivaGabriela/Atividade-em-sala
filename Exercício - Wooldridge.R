# Questão 1

install.packages("wooldridge")
library(wooldridge)

dados_c1 <- wage1
?wage1

# Nível de escolaridade médio
mean(dados_c1$educ, na.rm = TRUE) # média

# Anos de educação(Maior e Menor)
max(dados_c1$educ, na.rm = TRUE) # máxima
min(dados_c1$educ, na.rm = TRUE) # minima

# Salário-hora médio
mean(dados_c1$wage, na.rm = TRUE)

# Mulheres e homens
table(dados_c1$female)

# Questão 2

install.packages("wooldridge")
library(wooldridge)

dados_c2 <- bwght
?bwght

# Mulheres Fumantes e não fumantes
table(dados_c2$cigs)
1388-1176

library(dplyr)

# Só as pessoas que fumam
dados_fumam <- dados_c2 %>% filter(cigs>0)

# Média de cigarros consumidos por dia
mean(dados_c2$cigs, na.rm = TRUE)

# Média de cigarros consumidos por dia mulheres grávidas
mean(dados_fumam$cigs, na.rm = TRUE)

# Renda média familiar
mean(dados_c2$faminc, na.rm = TRUE) # Média
sd(dados_c2$faminc, na.rm = TRUE)# Desvio-padrão

# Média e desvio em dólar
29.02666*5.65
18.73928*5.65

# Questão 5

install.packages("wooldridge")
library(wooldridge)

dados_c5 <- fertil2
?fertil2

# Valores (maiores e menores) de children
max(dados_c5$children, na.rm = TRUE) # máxima
min(dados_c5$children, na.rm = TRUE) # minima

# Média de children
mean(dados_c5$children, na.rm = TRUE)

# % de mulheres com eletricidade em casa
table(dados_c5$electric)/nrow(dados_c5)*100 #nrow faz dividir pelo total

install.packages("dplyr")
library(dplyr)

dados_eletricidades <- dados_c5 %>% filter(electric==0)
dados_eletricidadec <- dados_c5 %>% filter(electric==1)

# Média de mulheres com eletricidade
mean(dados_eletricidadec$children, na.rm = TRUE)

# Média de mulheres sem eletricidade
mean(dados_eletricidades$children, na.rm = TRUE)

# Questão 6

install.packages("wooldridge")
library(wooldridge)

dados_c6 <- countymurders
?countymurders

install.packages("dplyr")
library(dplyr)

# Total de condados(dados de 1996)
dados_ano1996 <- dados_c6 %>% filter(year== 1996)

# Zero assassinatos(dados de 1996)
ZEROMURDERS <- dados_ano1996 %>% filter(murders == 0)

# % de zero execução(dados de 1996)
table(dados_ano1996$execs)/nrow(dados_ano1996)*100

# Máximo de assassinatos
max(dados_ano1996$murders, na.rm = TRUE) # máxima

# Máximo de execução
max(dados_ano1996$execs, na.rm = TRUE) # máxima

# Média de execução
mean(dados_ano1996$execs, na.rm = TRUE)

# coeficiente de correlação
cor(dados_ano1996$murders, dados_ano1996$execs)

# Questão 7

install.packages("wooldridge")
library(wooldridge)

dados_c7 <- alcohol
?alcohol

install.packages("dplyr")
library(dplyr)

# Abuso de álcool
table(dados_c7$abuse)/nrow(dados_c7)*100

# Taxa de emprego
table(dados_c7$employ)/nrow(dados_c7)*100

# Taxa de emprego com abuso de álcool
ABUSOÁLCOOL <- dados_c7 %>% filter(abuse == 1)
table(ABUSOÁLCOOL$employ)/nrow(ABUSOÁLCOOL)*100

# Taxa de emprego sem abuso de álcool
SEMABUSO <- dados_c7 %>% filter(abuse == 0)
table(SEMABUSO$employ)/nrow(SEMABUSO)*100















