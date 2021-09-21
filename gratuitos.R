mode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

hist_logy <- function(x, breaks = "Sturges", plot = TRUE, ...){
  h <- hist(x, breaks = breaks, plot = FALSE)
  is.na(h$counts) <- h$count == 0
  if(plot) barplot(setNames(h$counts, h$mids), log = "y", space = 0, ...)
  invisible(h)
}

data <- read.csv("./Database/gratuitos.csv")

print("gratuitos: ")
print("")


#Numero de Incritos
mediaInscritos <- mean(data$numInscritos)
medianaInscritos <- median(data$numInscritos)
modaInscritos <- mode(data$numInscritos)
varInscritos <- var(data$numInscritos)
dpInscritos <- sd(data$numInscritos)
cfInscritos <- dpInscritos/mediaInscritos * 100
Q1Inscritos <- quantile(data$numInscritos*1000, probs = 0.25)/1000
Q2Inscritos <- quantile(data$numInscritos*1000, probs = 0.50)/1000
Q3Inscritos <- quantile(data$numInscritos*1000, probs = 0.75)/1000

print(sprintf("Inscrição: media: %.3f, mediana: %.3f, moda: %.3f, Quartis{ %.3f , %.3f , %.3f }",
              mediaInscritos, medianaInscritos, modaInscritos, Q1Inscritos, Q2Inscritos, Q3Inscritos ))
print(sprintf("Variância: %.3f, desvio padrão:%.3f , coeficiente de variação: %.3f",
              varInscritos, dpInscritos, cfInscritos))
hist_logy(data$numInscritos, main="Quantidade de inscritos")
boxplot(data$numInscritos, main="Quantidade de Inscritos")



# Avaliação
mediaAvaliacao <- mean(data$avaliacao)
medianaAvaliacao <- median(data$avaliacao)
modaAvaliacao <- mode(data$avaliacao)
varAvaliacao <- var(data$avaliacao)
dpAvaliacao <- sd(data$avaliacao)
cfAvaliacao <- varAvaliacao/mediaAvaliacao * 100
Q1Avaliacao <- quantile(data$avaliacao*1000, probs = 0.25)/1000
Q2Avaliacao <- quantile(data$avaliacao*1000, probs = 0.50)/1000
Q3Avaliacao <- quantile(data$avaliacao*1000, probs = 0.75)/1000

print(sprintf("Avaliação: media: %.3f, mediana: %.3f, moda: %.3f, Quartis{ %.3f , %.3f , %.3f }",
              mediaAvaliacao, medianaAvaliacao, modaAvaliacao, Q1Avaliacao, Q2Avaliacao, Q3Avaliacao ))
print(sprintf("Variância: %.3f, desvio padrão:%.3f , coeficiente de variação: %.3f",
              varAvaliacao, dpAvaliacao, cfAvaliacao))
hist(data$avaliacao, xlab = c(0,5), main = "Avaliação do curso")
boxplot(data$avaliacao, main="Avaliação do curso")

#Criticas
mediaCriticas <- mean(data$numCriticas)
medianaCriticas <- median(data$numCriticas)
modaCriticas <- mode(data$numCriticas)
varCriticas <- var(data$numCriticas)
dpCriticas <- sd(data$numCriticas)
cfCrticas <- dpCriticas/mediaCriticas * 100
Q1Criticas <- quantile(data$numCriticas*1000, probs = 0.25)/1000
Q2Criticas <- quantile(data$numCriticas*1000, probs = 0.50)/1000
Q3Criticas <- quantile(data$numCriticas*1000, probs = 0.75)/1000
print(sprintf("Críticas: media: %.3f, mediana: %.3f, moda: %.3f, Quartis{ %.3f, %.3f, %.3f }",
              mediaCriticas, medianaCriticas, modaCriticas, Q1Criticas, Q2Criticas, Q3Criticas))
print(sprintf("Variância: %.3f, desvio padrão:%.3f , coeficiente de variação: %.3f",
              varCriticas, dpCriticas, cfCrticas))
hist_logy(data$numCriticas, main="Quantidade de Críticas")
boxplot(data$numCriticas, main="Quantidade de Críticas")

#avaliando x na fdp
normalCriticas<- dnorm(data$numCriticas, sd = dpCriticas, mean=mediaCriticas)
normalInscritos<- dnorm(data$numInscrito, sd = dpInscritos, mean = mediaInscritos)
normalAvaliacao<- dnorm(data$avaliacao, sd = dpAvaliacao, mean = mediaAvaliacao)

#plot a fdp
plot(data$numCriticas,normalCriticas,type = "l", ylab = "densidade", xlab = "criticas", main="Grafico normalizado de Críticas" )
plot(data$numInscritos,normalInscritos,type = "l", ylab = "densidade", xlab = "Inscritos", main="Grafico normalizado de Inscritos")
plot(data$avaliacao,normalAvaliacao,xlim = c(0,5) ,type = "l", ylab = "densidade", main="Grafico normalizado de Avaliação")
