mode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
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
hist(data$numInscritos)
boxplot(data$numInscritos)



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
hist(data$avaliacao)
boxplot(data$avaliacao)

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
hist(data$numCriticas)
boxplot(data$numCriticas)

#avaliando x na fdp
normalCriticas<- dnorm(data$numCriticas, mediaCriticas, sd = 1)
normalInscritos<- dnorm(data$numInscritos, mediaInscritos, sd = varInscritos)
normalAvaliacao<- dnorm(data$avaliacao, mediaAvaliacao, sd = varAvaliacao)

#plot a fdp
plot(data$numCriticas,normalCriticas,type = "l", ylab = "densidade", xlab = "criticas")
plot(data$numInscritos,normalInscritos,type = "l", ylab = "densidade", xlab = "Inscritos")
plot(data$avaliacao,normalAvaliacao,type = "l", ylab = "densidade", xlab = "Avaliação")