
library(dplyr)
library(tidyverse)

clubes<- c("Sport", "Santa Cruz", "Náutico")
estadios<- c("Ilha do Retiro", "Arruda", "Aflitos")
capacidade<- c(35000, 50000, 18000)

futebol_pernambuco <- data.frame(clubes, estadios, capacidade)

times<- c("Sport", "Santa Cruz", "Náutico")
ruas <- c("Abdias", "Beberibe", "Rosa e Silva")
cores<- c("rubronegro", "tricolor", "avirrubro")

times_pernambuco <- data.frame(times, ruas, cores)


dados <- left_join(futebol_pernambuco, times_pernambuco, by = c('clubes' = 'times'))