# Pacotes
install.packages("ggplot2")
library(ggplot2)

# Questão 1
# Dados fornecidos
dados1 <- data.frame(volume = c(656, 692, 588, 799, 766, 800, 693, 602, 737, 921, 923, 945, 816, 584, 642, 970),Peso = c(630, 745, 690, 890, 825, 960, 835, 570, 705, 955, 990, 725, 840, 640, 740, 945))

# i) Modelo de regressão linear simples
modelo1 <- lm(formula = volume ~ Peso, data = dados1)

# ii) Gráfico de dispersão
ggplot(data = dados1, aes(x = volume, y= Peso))+
  geom_point(size = 2) +
  theme_classic() +
  xlab("Volume") +
  ylab("Peso") +
  geom_smooth(method = "lm", se = FALSE)

# iii) Ajuste do modelo e diagnóstico
cor(dados1$volume, dados1$Peso)
summary(modelo1)

# iv) Intervalo de confiança dos parâmetros
confint(modelo1)

# v) Ajuste para diferentes volumes
model_600 <- predict(modelo1, newdata = data.frame(Peso = 600))
model_700 <- predict(modelo1, newdata = data.frame(Peso = 700))
model_800 <- predict(modelo1, newdata = data.frame(Peso = 800))
model_900 <- predict(modelo1, newdata = data.frame(Peso = 900))
model_1000 <- predict(modelo1, newdata = data.frame(Peso = 1000))

# Resultado
model_600
model_700
model_800
model_900
model_1000

# iv) Repetição dos Itens com Modelo Sem Intercepto
# Ajuste do modelo sem intercepto
modelo1_sem_intercepto <- lm(volume ~ Peso - 1, data = dados1)

# Gráfico de dispersão para o modelo sem intercepto
ggplot(data = dados1, aes(x = Peso, y = volume)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Peso") +
  ylab("Volume") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x - 1) +
  ggtitle("Gráfico de Dispersão com Modelo Sem Intercepto")

# Avaliação do modelo sem intercepto
summary(modelo1_sem_intercepto)

# Intervalo de confiança para o modelo sem intercepto
confint(modelo1_sem_intercepto)

# Previsões usando o modelo sem intercepto
previsoes_sem_intercepto <- predict(modelo1_sem_intercepto, newdata = volume_novos)
previsoes_sem_intercepto

# Questão 2
# Dados fornecidos
dados2 <- data.frame(particular = c(8.6, 8.6, 7.8, 6.5, 7.2, 6.6, 5.6, 5.5, 8.2), publica = c(5.8, 7.6, 8.0, 6.2, 7.6, 6.5, 5.6, 5.7, 5.8))
# Reestruturando os dados para ajuste do modelo
# Criamos uma variável 'escola' que indica se é particular (1) ou pública (-1)
escola <- c(rep(1, 9), rep(-1, 9))
notas <- c(dados2$particular, dados2$publica)
dados_reestruturados <- data.frame(escola, notas)

# i) Interpretação dos Parâmetros α e β.

# ii) Estimativa de α e β pelo Método de Mínimos Quadrados
# Ajuste do modelo de regressão
modelo2 <- lm(notas ~ escola, data = dados_reestruturados)
summary(modelo2)
# Estimativa da variância residual (sigma^2)
sigma2 <- summary(modelo2)$sigma^2
sigma2

# iii) Avaliação da Qualidade do Ajuste
# Gráfico de resíduos
plot(modelo2, which = 1)  # Resíduos vs Ajustados
qqnorm(residuals(modelo2))  # Verificação da normalidade dos resíduos
qqline(residuals(modelo2))

# iv) Intervalos de Confiança para α e β
confint(modelo2)

# v) Intervalos de Confiança
# Média esperada para escolas particulares (escola = 1)
media_particular <- predict(modelo2, newdata = data.frame(escola = 1), interval = "confidence")
# Média esperada para escolas públicas (escola = -1)
media_publica <- predict(modelo2, newdata = data.frame(escola = -1), interval = "confidence")

# Resultados
media_particular
media_publica

# Questão 3
# Dados fornecidos
# Criando o dataframe com os dados da tabela
dados3 <- data.frame(area = c(128, 400, 240, 300, 450, 500, 800, 1200, 750, 150, 600, 1350, 3000, 3000), preco = c(10000, 30000, 27000, 40000, 75000, 70000, 90000, 95000, 85000, 25000, 80000, 135000, 300000, 300000))

# i) Gráfico de dispersão
ggplot(data = dados3, aes(x = area, y= preco))+
  geom_point(size = 4) +
  theme_classic() +
  xlab("Área") +
  ylab("Preço") +
  geom_smooth(method = "lm", se = FALSE)

# ii) Ajuste do modelo de regressão linear simples
modelo <- lm(preco ~ area, data=dados3)
# Resumo do modelo
summary(modelo)
# Gráfico de resíduos
par(mfrow = c(2, 2))
plot(modelo)

# iii) Ajuste do Modelo Linearizado (Transformação Logarítmica)
# Aplicando a transformação logarítmica
dados3$log_preco <- log(dados3$preco)
# Ajuste do modelo linearizado
modelo_log <- lm(log_preco ~ area, data=dados3)
# Resumo do modelo logarítmico
summary(modelo_log)
# Gráfico de resíduos para o modelo logarítmico
par(mfrow = c(2, 2))
plot(modelo_log)

# iv) Previsão para os valores de 200 m², 500 m² e 1000 m²
novos_dados3 <- data.frame(area = c(200, 500, 1000))
# Previsões com intervalo de confiança
predicoes <- predict(modelo_log, newdata=novos_dados3, interval="confidence")
# Convertendo log para o preço original
predicoes$preco_estimado <- exp(predicoes)

predicoes


