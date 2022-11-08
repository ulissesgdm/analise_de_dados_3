install.packages('fuzzyjoin')
library(fuzzyjoin)
library(dplyr)
library(tidyverse)
library(tidyr)

#Filtrando a base de dados do TSE para obter os dados correspondentes ao comparecimento no 1 e 2 turno para as eleições presidenciais

comparecimento_1 <- comparecimento.votacao.município_2022_1_turno %>% filter(ds_cargo == "Presidente")
  
comparecimento_1 <- select (comparecimento_1, sg_uf,nm_municipio, pc_secoes_agregadas, pc_comparecimento, pc_abstencoes, qt_aptos, qt_comparecimento, qt_abstencoes)

comparecimento_2 <- comparecimento.votacao.município_2022 %>% filter(ds_cargo == "Presidente")

comparecimento_2 <- select (comparecimento_2, sg_uf,nm_municipio, pc_secoes_agregadas, pc_comparecimento, pc_abstencoes, qt_aptos, qt_comparecimento, qt_abstencoes)

#renomeando as colunas do segundo turno para diferencia-las quando houver a união das duas bases

comparecimento_2 <- rename(comparecimento_2, pc_secoes_agregadas_2 = pc_secoes_agregadas,
                           pc_comparecimento_2 = pc_comparecimento,
                           pc_abstencoes_2 = pc_abstencoes,
                           qt_aptos_2 = qt_aptos,
                           qt_comparecimento_2 = qt_comparecimento,
                           qt_abstencoes_2 = qt_abstencoes)


#unindo as duas bases a partir dos municípios

comparecimento <- left_join(comparecimento_1, comparecimento_2, by = c('nm_municipio' = 'nm_municipio'))

comparecimento_g <- comparecimento %>% filter(qt_aptos > 200000)

comparecimento_g <- comparecimento_g %>% filter(qt_aptos_2 > 200000)

