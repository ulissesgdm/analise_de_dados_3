library(data.table)
library(dplyr)
library(tidyverse)
install.packages("funModeling")
library(funModeling) 

general_data <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv") # carrega dados de covid19 no mundo

latin_america_countries <-c("Argentina", "Brazil", "Bolivia", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela") # vetor que identifica países latino americanos

latin_america <- general_data %>% filter(location %in% latin_america_countries) # filtra casos apenas no vetor

latin_america <- latin_america %>% select(location, new_cases, new_deaths)

status(latin_america) # estrutura dos dados (missing etc)
freq(latin_america) # frequência das variáveis fator
plot_num(latin_america) # exploração das variáveis numéricas
profiling_num(latin_america) # estatísticas das variáveis numéricas

latin_america %>% filter(new_cases < 0)

latin_america <- latin_america %>% filter(new_cases>=0)

#Retirando os valores NA na coluna de novos casos
latin_america <- latin_america[!is.na(latin_america$new_cases),]

#Retirando os valores NA na coluna de novos casos fatais
latin_america <- latin_america[!is.na(latin_america$new_deaths),]