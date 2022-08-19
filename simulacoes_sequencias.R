#Distribuição normal simulada, o número entre parênteses indica o número de casos
dist_norm<- rnorm(100)
summary(dist_norm)

#distribuição binomial simulada, número de casos, valor que vai ser distribuído e proporção da distribuição do valor.
disti_bi<- rbinom(100, 1, 0.65)
summary(disti_bi)

#sequência, 
indexSimulacao<- seq(1, length(dist_norm))
