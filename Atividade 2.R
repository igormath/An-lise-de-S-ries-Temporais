homicidios <- read.csv("/home/igazev/Downloads/homicidios_brasil_2017.csv", sep = ",")
View(homicidios)
attach(homicidios)

# Gerar e guardar o histograma do número proporcional de homicídios de cada estado
histograma <- hist(homicidiosProporcional, breaks = 10)

# Prosseguindo com a adição da linha de frequência de observações no gráfico
xfit <- seq(min(homicidiosProporcional), max(homicidiosProporcional), length = 27)
yfit <- dnorm(xfit, mean = mean(homicidiosProporcional), sd = sd(homicidiosProporcional))
yfit <- yfit * diff(histograma$mids[1:2]) * length(homicidiosProporcional)
lines(xfit, yfit, col = "blue", lwd = 2)