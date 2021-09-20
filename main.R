library(dplyr)
df <- read.csv(file = "Database/cursosUdemy.csv")
df <- within(df, preco[is.na(preco)] <- 0 )
df <- within(df, desconto[is.na(desconto)] <- 0 )

pagos <- filter(df, pago == "True")
write.csv( pagos, file = "./Database/pagos.csv", row.names=FALSE)
source("./pagos.R")

gratuitos <- filter(df, pago == "False")
write.csv(gratuitos, file = "./Database/gratuitos.csv", row.names=FALSE)
source("./gratuitos.R")
