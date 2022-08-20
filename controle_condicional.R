
clubes<- c("Sport", "Santa Cruz", "Náutico")
estadios<- c("Ilha do Retiro", "Arruda", "Aflitos")
capacidade<- c(35000, 50000, 18000)

futebol_pernambuco <- data.frame(clubes, estadios, capacidade)

grandes_estadios<- ifelse(futebol_pernambuco$capacidade > 40000, 1, 0)

futebol_pernambuco2 <- data.frame(clubes, estadios, capacidade, grandes_estadios)
