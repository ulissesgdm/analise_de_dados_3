pacman::p_load(data.table, funModeling, tidyverse, VIM) 

data()

clubes<- c("Sport", "Santa Cruz", "Náutico", "Flamengo", "Atlético-Mg", "Botafogo", "Vasco", "Palmeiras")
estadios<- c("Ilha do Retiro", "Arruda", "Aflitos", NA, NA, "Nilton Santos", "São Jenuário", "Alianz Parque")
capacidade<- c(35000, 50000, 18000, NA, NA, 40000, 25000, 42000)


futebol<- data.frame(clubes, estadios, capacidade)


## identificando e removendo valores ausentes
status(futebol) # estrutura dos dados (missing etc)

# Complete-case analysis – listwise deletion
dim(futebol<- na.omit(futebol)) # deixa apenas os casos completos

# Variação de Complete-case analysis – listwise deletion
dim(futebol <- futebol %>% filter(!is.na(capacidade)))

## estimando se o NA é MCAR, MAR ou MANR
## Shadow Matrix do livro R in Action

data(futebol, package = "VIM") # importa a base sleep

head(futebol) # observa a base

x <- as.data.frame(abs(is.na(futebol))) # cria a matrix sombra
head(x) # observa a matriz sombra

y <- x[which(sapply(x, sd) > 0)] # mantém apenas variáveis que possuem NA
cor(y) # observa a correlação entre variáveis

cor(sleep, y, use="pairwise.complete.obs") # busca padrões entre os valores específicos das variáveis e os NA

## Shadow Matrix da nossa base

futebol_NA <- as.data.frame(abs(is.na(futebol))) # cria a matriz sombra da base 

futebol_NA <- futebol_NA[which(sapply(futebol_NA, sd) > 0)] # mantém variáveis com NA
round(cor(futebol_NA), 3) # calcula correlações
