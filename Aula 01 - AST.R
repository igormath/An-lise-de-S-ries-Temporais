#################################################################
# Aula 01 - Apresentação e Estatísticas Descritivas
# Curso de Análise das Séries Temporais
# Prof. Thyago Nepomuceno
################################################################

###Carregamento dos dados###

Dados <- read.csv2("G:/Meu Drive/Drive (UFPE)/1. Teaching/Eng. de Produção - UFPE/Análise das Séries Temporais/2020.1/Aula 01/HE_Dataset_UFPE.csv")
attach(Dados)
View(Dados)

###Estatísticas Descritivas###

summary(Dados) #Média, Mediana,1st Q., 3rd Q., Min, Max
summary(Dados$X.x._pub_exp_)#Média, Mediana,1st Q., 3rd Q., Min, Max para uma variável
fivenum(Dados$X.x._pub_exp_) #Tukey min, 25th Q., Mediana, 75th Q., Max

sd(Dados$X.x._pub_exp_)  #Desvio Padrão da Variável 'Gastos Públicos'
sd(Dados$X.y1.1._gr) #Desvio Padrão da Variável 'Taxa de Formandos'
sd(Dados$X.y1.2._publications) #Desvio Padrão da Variável 'Taxa de Publicação'
sd(Dados$X.y1.3._PCTpat_HE) #Desvio Padrão da Variável 'Patentes'

###Definação das variáveis###

X <- Dados$X.x._pub_exp_ #Atribuindo os Gastos Públicos para a variável X
Y1 <- Dados$X.y1.1._gr #Atribuindo a Taxa de Graduados para a variável Y1
Y2 <- Dados$X.y1.2._publications #Atribuindo a Taxa de Publicação para a variável Y2
Y3 <- Dados$X.y1.3._PCTpat_HE #Atribuindo as Patentes para a variável Y3
View(X)
View(Y1)
View(Y2)
View(Y3)

summary(X)
sd(X)

cor.test(Y1, Y2) #Teste de correlação de Pearson entre os pares das amostras Y1 e Y2
cor.test(Y1, Y3) #Teste de correlação de Pearson entre os pares das amostras Y1 e Y3
cor.test(Y2, Y3) #Teste de correlação de Pearson entre os pares das amostras Y2 e Y3

###Visualizações Gráficas###

#Boxplot: Análise visual da posição (a partir da mediana), dispersão, simetria,
          #caudas e valores discrepantes (outliers) do conjunto de dados.

boxplot(X) 
boxplot(Y1)
boxplot(Y2)
boxplot(Y3)
boxplot(X, Y1, Y2, Y3)
boxplot(X, Y1, Y2, Y3, names = c("Gastos", "Graduados", "Publicações", "Patentes"))
boxplot(X, Y1, Y2, Y3, notch = TRUE, outline = FALSE, names = c("Gastos", "Graduados", "Publicações", "Patentes"))
boxplot(Y1, Y2, Y3, notch = TRUE, outline = FALSE, names = c("Graduados", "Publicações", "Patentes"))
boxplot(Y1, Y2, notch = TRUE, outline = FALSE, names = c("Graduados", "Publicações"))
title(main = "Boxplots para Graduados e Publicações")

#Scatterplots (Gráficos de Disperção): Relação positiva, negativa ou aleatória 
                                       #entre duas variáveis

plot(Y1, Y2, xlab = "Graduados", ylab = "Publicações") #Gráfico de Disperção entre Y1 e Y2 ajustado pela linha de regressão
abline(lm(Y2~Y1), col="red")
title(main = "Gráfico de Dispersão")
title(main = "

    (cor = 0.2370649)",cex.main = 1)
plot(Y1, Y3) #Gráfico dos resíduos entre Y1 e Y3 ajustado pela linha de regressão
abline(lm(Y3~Y1), col="red")
title(main = "Gráfico de Resíduos")
title(main = "

    (cor = 0.3729648)",cex.main = 1)
plot(Y2, Y3) #Gráfico dos resíduos entre Y2 e Y3 ajustado pela linha de regressão
abline(lm(Y3~Y2), col="red")
title(main = "Gráfico de Resíduos")
title(main = "

    (cor = 0.7297745)",cex.main = 1)

#Histograma: Distribuição da frequência dos dados em barras dividida por classes 
             #uniformemente distribuídas (mais comum).

hist(X)
hist(Y1)
hist(Y2)
hist(Y3)
hist(Y3, col = "gray", density = 20, lwd=2)
hist(Y3, col = "gray", density = 20, lwd=2, xlab = "Patentes", ylab = "Frequência", breaks = 20)
hist(Y3, col = "gray", density = 20, lwd=2, xlab = "Patentes", ylab = "Frequência", breaks = 100)

#Gráfico Q-Q: comparação entre duas distribuições de probabilidade (empírica vs. normal)
              # pelo traçado e aderência de seus quantis.

qqnorm(Y3)
qqline(Y3)
qqnorm(Y2)
qqline(Y2)
qqnorm(Y1)
qqline(Y1)
qqnorm(X)
qqline(X)


###Consulta ao manual dos comandos através do comando 'help()'###

help("boxplot")
help("plot")
help("hist")
