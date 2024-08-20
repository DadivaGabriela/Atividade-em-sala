install.packages("ggplot2")
library(ggplot2)

######## DIAGRAMA DE INSPERSSÃO ########

ggplot(data = poluicao, aes(x = dia, y= CO))+
  geom_point(size = 4) +
  theme_classic() +
  xlab("Dia") +
  ylab("Concentração de CO") +
  geom_smooth(method = "lm", se = FALSE)

######## COEFICIENTE DE CORRELAÇÃO ########

cor(poluicao$dia,poluicao$CO)

######## AJUSTE DO MODELO ########

modelo_poluição <- lm(formula = CO ~ dia, data = poluicao)
summary(modelo_poluição)

######## ANALISE DOS RESIDUOS ########

plot(poluicao$dia, modelo_poluição$residuals)
abline(0,0)

# O modelo tem desempenho bastante variavel para diferentes valores de x, 
#especialmente muito baixos ou muito altos parece desobedecer o coeficiente de homocedasticidade.

######## PREDIÇÃO ########

novo_dado <- data.frame(dia = 121)
predict(modelo_poluição, novo_dado, interval = "predict")