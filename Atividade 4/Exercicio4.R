# Carregando o banco de dados que reúne as estimativas de tamanho populacional do Brasil
popBrasil <- read.csv("/home/igazev/Documentos/datasets/tabela6579.csv", sep = ",")

# Organizando o Dataset
popBrasil <- popBrasil[1:18, c("X", "Brasil")]
popBrasil <- data.frame(ano = X, populacao = Brasil)
attach(popBrasil)

# Suavização exponencial simples

library(forecast)
previsao <- ses(populacao, h = 10, fan = TRUE)
plot(previsao, main = "Variação futura da população brasileira \n por meio de Suavização exponencial simples", xlab = "Observações", ylab = "População")
previsao$model
previsao[["model"]][["fitted"]] # fitted value

# Suavização exponencial de Holt

previsao <- holt(populacao, alpha = 0.2, beta = 0.1, h = 10)
plot(previsao, main = "Suavização exponencial de Holt \npara o crescimento da população brasileira \nnos próximos 10 anos")
previsao$model
summary(previsao)

# Suavização exponencial de Holt com amortecimento

previsao <- holt(populacao, alpha = 0.2, beta = 0.1, h = 10, damped = TRUE, phi = 0.9, fan = TRUE) # (Com phi = 0.9)
plot(previsao)