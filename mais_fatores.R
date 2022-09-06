install.packages('ade4')
install.packages('arules')

library(ade4)
library(arules)
library(forcats)
library(dplyr)

clube_campeao <- c(1, 1, 1, 2, 1, 1, 2, 3, 2, 4, 3) #Campeões pernambucanos desde 2011

recode <- c(Santa_Cruz = 1, Sport = 2, Nautico = 3, Salgueiro = 4)

(clube_campeao <- factor(clube_campeao, levels = recode, labels = names(recode)))

(clube_campeao <- relevel(clube_campeao, ref = "Santa_Cruz"))

divisao <- c(4, 3, 3, 1, 2, 1, 1, 3, 2, 4, 2) #divisão em que os clubes campeões estavam quando consquistaram os respectivos títulos

ano <- c(2011:2021)

teste <- sample(1:100, 11)

(clube_campeao <- reorder(clube_campeao, divisao)) 

campeonato <- data.frame(ano, clube_campeao, divisao, teste)


campeonato_hot <- acm.disjonctif(campeonato)


campeonato$teste <- discretize(campeonato$teste, method = "interval", breaks = 3, labels = c("mais frequentes", 'segundo mais frequentes', 'pouco frequentes'))