pacman::p_load(dplyr)

sinistrosRecife2018Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2485590a-3b35-4ad0-b955-8dfc36b61021/download/acidentes_2018.csv', sep = ';', encoding = 'UTF-8')

sinistrosRecife2019Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/3531bafe-d47d-415e-b154-a881081ac76c/download/acidentes-2019.csv', sep = ';', encoding = 'UTF-8')

sinistrosRecife2020Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/fc1c8460-0406-4fff-b51a-e79205d1f1ab/download/acidentes_2020-novo.csv', sep = ';', encoding = 'UTF-8')

sinistrosRecife2021Raw <- read.csv2('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2caa8f41-ccd9-4ea5-906d-f66017d6e107/download/acidentes2021.csv', sep = ';', encoding = 'UTF-8')

colunas_iguais <- names(sinistrosRecife2020Raw[
  intersect(
    names(sinistrosRecife2020Raw), names(sinistrosRecife2021Raw))])

sinistrosRecife2020Raw <- sinistrosRecife2020Raw %>% select(all_of(colunas_iguais))

#mudar nome da coluna DATA

sinistrosRecife2018Raw <- sinistrosRecife2018Raw %>% rename(data = DATA)

sinistrosRecife2018Raw <- sinistrosRecife2018Raw %>%  select(all_of(colunas_iguais))

sinistrosRecife2019Raw <- sinistrosRecife2019Raw %>% rename(data = DATA)

sinistrosRecife2019Raw <- sinistrosRecife2019Raw %>%  select(all_of(colunas_iguais))


#UNIR AS BASES DE DADOS
sinistrosRecifeRaw <- rbind(sinistrosRecife2020Raw, sinistrosRecife2021Raw)

#TRANSFORMAR OS VALORES DA PRIMEIRA COLUNA EM DATA
sinistrosRecifeRaw$data <- as.Date(sinistrosRecifeRaw$data, format = "%Y-%m-%d")

#MODIFICAR O TEXTO DO BAIRRO PARA FACTOR
sinistrosRecifeRaw$bairro <- as.factor(sinistrosRecifeRaw$bairro)

# cria funçaõ para substituir not available (na) por 0
naZero <- function(x) {
  x <- ifelse(is.na(x), 0, x)
}

# aplica a função naZero a todas as colunas de contagem
sinistrosRecifeRaw[, 15:25] <- sapply(sinistrosRecifeRaw[, 15:25], naZero)

#Lista os objetos carregados
ls()


for (itm in ls()) { 
  print(formatC(c(itm, object.size(get(itm))), 
                format="d", 
                width=30), 
        quote=F)
}


rm(list = c('sinistrosRecife2018Raw','sinistrosRecife2019Raw','sinistrosRecife2020Raw', 'sinistrosRecife2021Raw'))

ls()

