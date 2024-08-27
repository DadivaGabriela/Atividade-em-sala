install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
library(ggplot2)
library(readr)
install.packages("dplyr")
library(dplyr)
concrete_data <- read_csv("C:/Users/DIGITAL/Downloads/concrete_data.csv")
View(concrete_data)

# Atividade - Concreto
# a) Matriz de correlações:
Matriz <- cor(concrete_data)
corrplot(Matriz, method = "number")

# b) )Faça um ranking com os coeficientes de correlação (em módulo), do maior para o menor,
# para verificar quais variáveis têm correlação linear mais forte com "strength".
# 1º - cement
# 2º - superplastic
# 3º - age
# 4º - water
# 5º - fineagg  
# 6º - coarseagg
# 7º - slag
# 8º - ash

# c) Diagrama de dispersão:
# Gráfico de dispersão: Cement
ggplot(data = concrete_data, aes( x = cement, y = strength)) +
  geom_point() +
  theme_classic() +
  xlab("Cement") +
  ylab("Strenght") +
  ggtitle("Gráfico de Dispersão: Cement") +
  geom_smooth(method = "lm", se = FALSE)
# Gráfico de dispersão: superplastic
ggplot(data = concrete_data, aes( x = superplastic, y = strength)) +
  geom_point() +
  theme_classic() +
  xlab("Superplastic") +
  ylab("Strenght") +
  ggtitle("Gráfico de Dispersão: Superplastic") +
  geom_smooth(method = "lm", se = FALSE)
# Gráfico de dispersão: age
ggplot(data = concrete_data, aes( x = age, y = strength)) +
  geom_point() +
  theme_classic() +
  xlab("Age") +
  ylab("Strenght") +
  ggtitle("Gráfico de Dispersão: Age") +
  geom_smooth(method = "lm", se = FALSE)

# d) Construa um modelo de regressão linear incluindo todas as variáveis.
#Qual a equação do modelo?
modelo_completo <- lm(formula = strength ~., data = concrete_data)
# Imprime o modelo:
modelo_completo %>% summary()

# e) Analise os resultados (coeficientes do modelo, R² ajustado, 
# significância dos coeficientes, ferramentas de diagnóstico).
qqnorm(modelo_completo$residuals)
qqline(modelo_completo$residuals)
shapiro.test(modelo_completo$residuals)
hist(modelo_completo$residuals,
     main = "Histograma de resíduos contra x (strength)",
     xlab = "Resíduo",
     ylab = "Frequência do resíduo")

# f) Agora faça um novo modelo, removendo as variáveis que não são 
# estatisticamente significativas. 
modelo_reduzido2 <- lm(formula = strength ~ cement+superplastic+age, data = concrete_data)
# Imprime o modelo:
modelo_reduzido2 %>% summary()
# Refaça as análises feitas no item 5).
qqnorm(modelo_reduzido2$residuals)
qqline(modelo_reduzido2$residuals)
shapiro.test(modelo_reduzido2$residuals)
hist(modelo_reduzido2$residuals,
     main = "Histograma de resíduos contra x (strength)",
     xlab = "Resíduo",
     ylab = "Frenquencia do resíduo")

# g) De acordo com seu modelo, quais são as variáveis que mais impactam 
# a resistência à compressão do concreto?

# As variáveis cement,  superplastic e  age são as que mais impactam a 
#resistência do concreto.

# h)  Use o modelo para prever "strength" para um material com as seguintes características:

novo_dado <- data.frame( slag = 74, ash = 54, cement = 280, age = 45, water = 182, superplastic = 6, coarseagg = 972, fineagg = 773)
predict(modelo_reduzido2, novo_dado)

# Resposta: 35.44