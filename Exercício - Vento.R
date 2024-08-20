install.packages("ggplot2")
library(ggplot2)

######## DIAGRAMA DE INSPERSSÃO ########

ggplot(data = vento, aes(x = t, y= vt))+
  geom_point(size = 4) +
  theme_classic() +
  xlab("dia") +
  ylab("velocidade do vento") +
  geom_smooth(method = "lm", se = FALSE)

######## COEFICIENTE DE CORRELAÇÃO LINEAR DE PERSON ########

cor(vento$t,vento$vt)

######## AJUSTE DO MODELO ########

modelo_vento <- lm(formula = vt ~ t, data = vento)
summary(modelo_vento)

######## GRÁFICO PARA VERIFICAR PONTOS DISCREPANTES ########

plot(modelo_vento)
abline(0,0)

######## ANALISE DE NORMALIDADE DOS RESIDUOS ########

qqnorm(modelo_vento$residuals)
abline(0,0)