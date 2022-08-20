#Vou utilizar uma base de dados criada em um exercício anterior para os testes de indexação

clubes<- c("Sport", "Santa Cruz", "Náutico")
estadios<- c("Ilha do Retiro", "Arruda", "Aflitos")
capacidade<- c(35000, 50000, 18000)

futebol_pernambuco <- data.frame(clubes, estadios, capacidade)

#seleção dos dados dos clubes da zona norte
zona_norte<-futebol_pernambuco[2:3,1:3]

#seleção dos dados do santa cruz
santa<-futebol_pernambuco[2,]

#seleção do vetor estádios a partir da base de dados
campos<- futebol_pernambuco$estadios
