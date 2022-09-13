pacman::p_load(data.table, dplyr, tidyverse, validate)


clubes<- c("Sport", "Santa Cruz", "Náutico", "Flamengo", "Atlético-Mg", "Botafogo", "Vasco", "Palmeiras")
estadios<- c("Ilha do Retiro", "Arruda", "Aflitos", NA, NA, "Nilton Santos", "São Jenuário", "Alianz Parque")
capacidade<- c(35000, 50000, 18000, NA, NA, 40000, 25000, 42000)

futebol <- data.frame(clubes, estadios, capacidade)

pernambuco <- c("Sport", "Santa Cruz", "Náutico")

futebol_pernambuco <- futebol %>% filter(clubes %in% pernambuco) # filtra casos apenas no vetor

futebol_pernambuco <- futebol_pernambuco %>% select(estadios, capacidade)

regras_futebol_pernambuco <- validator(capacidade >= 0)

validacao_futebol_pernambuco <- confront(futebol_pernambuco, regras_futebol_pernambuco)

summary(validacao_futebol_pernambuco)

plot(validacao_futebol_pernambuco)