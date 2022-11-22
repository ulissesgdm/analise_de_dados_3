library(fuzzyjoin)
library(dplyr)
library(tidyverse)
library(tidyr)
library(writexl)
library(ggplot2)

#Filtrando a base de dados do TSE para obter os dados correspondentes ao comparecimento no 1 e 2 turno para as eleições presidenciais

comparecimento_1 <- read.csv2("https://github.com/ulissesgdm/trabalho_final/raw/main/comparecimento-votacao-munic%C3%ADpio_2022_1_turno.csv", stringsAsFactors = T, sep = ";", encoding = "latin1") %>% filter(ds_cargo == "Presidente")

comparecimento_1 <- select (comparecimento_1, sg_uf,nm_municipio, pc_secoes_agregadas, pc_comparecimento, pc_abstencoes, qt_aptos, qt_comparecimento, qt_abstencoes)

#criar variável turno

comparecimento_1$turno <- sample(1:1, 5751, replace = T)

#Dados 2 turno

comparecimento_2 <- read.csv2("https://github.com/ulissesgdm/trabalho_final/raw/main/comparecimento-votacao-munic%C3%ADpio_2022_2_turno.csv", stringsAsFactors = T, sep = ";", encoding = "latin1") %>% filter(ds_cargo == "Presidente")

comparecimento_2 <- select (comparecimento_2, sg_uf,nm_municipio, pc_secoes_agregadas, pc_comparecimento, pc_abstencoes, qt_aptos, qt_comparecimento, qt_abstencoes)

#Criar variável turno

comparecimento_2$turno <- sample(2:2, 5751, replace = T)

#Dados sobre a aplicação do passe livre
passe_livre <- read.csv2("https://raw.githubusercontent.com/ulissesgdm/trabalho_final/main/passe_livre.csv", stringsAsFactors = T, sep = ";")

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


# unindo as bases de dados do passe livre e comparecimento

comparecimento_g <- fuzzyjoin::stringdist_join(comparecimento_g, passe_livre, by = c('nm_municipio' = 'nome_cid'), mode='left')

# Criando variável que compara o comparecimento no 1 e segundo turno, valores negativos indicam maior comparecimento no 2 turno.

comparecimento_g <- comparecimento_g %>%
  mutate(dif_turnos = (pc_comparecimento - pc_comparecimento_2))

write_csv2(comparecimento_g, 'comparecimento_pl.csv')

#filtragem para o dif e dif

#média de comparecimento nas três categorias no primeiro turno

pl_comparecimento_1 <- comparecimento_g %>%
  filter(pl_1_turno == 1 & pl_2_turno == 1) %>%
  select(pc_comparecimento) %>%
  summarise_all(mean)

sem_pl_comparecimento_1 <- comparecimento_g %>%
  filter(pl_1_turno == 0 & pl_2_turno == 0)%>%
  select(pc_comparecimento) %>%
  summarise_all(mean)

muda_pl_comparecimento_1 <- comparecimento_g %>%
  filter(pl_1_turno == 0 & pl_2_turno == 1)%>%
  select(pc_comparecimento) %>%
  summarise_all(mean)

#média de comparecimento das três categorias no segundo turno


pl_comparecimento_2 <- comparecimento_g %>%
  filter(pl_1_turno == 1 & pl_2_turno == 1) %>%
  select(pc_comparecimento_2) %>%
  summarise_all(mean)

sem_pl_comparecimento_2 <- comparecimento_g %>%
  filter(pl_1_turno == 0 & pl_2_turno == 0)%>%
  select(pc_comparecimento_2) %>%
  summarise_all(mean)

muda_pl_comparecimento_2 <- comparecimento_g %>%
  filter(pl_1_turno == 0 & pl_2_turno == 1)%>%
  select(pc_comparecimento_2) %>%
  summarise_all(mean)

#média com duas categorias primeiro turno

control <- comparecimento_g %>%
  filter(tratamento == 0) %>%
  select(pc_comparecimento) %>%
  summarise_all(mean)

tratament <- comparecimento_g %>%
  filter(tratamento == 1) %>%
  select(pc_comparecimento) %>%
  summarise_all(mean)

#médoa com duas categorias segundo turno

control_2 <- comparecimento_g %>%
  filter(tratamento == 0) %>%
  select(pc_comparecimento_2) %>%
  summarise_all(mean)

tratament_2 <- comparecimento_g %>%
  filter(tratamento == 1) %>%
  select(pc_comparecimento_2) %>%
  summarise_all(mean)


#diagramas

#três variáveis

diagrama <- data.frame(
  Time=factor(c("1º tuno", "2º turno")),
  Control1 =c(pl_comparecimento_1$pc_comparecimento, pl_comparecimento_2$pc_comparecimento_2),
  Control2 =c(sem_pl_comparecimento_1$pc_comparecimento, sem_pl_comparecimento_2$pc_comparecimento_2),
  Treatment=c(muda_pl_comparecimento_1$pc_comparecimento, muda_pl_comparecimento_2$pc_comparecimento_2)
)
diagrama


p<-ggplot(data=diagrama)+
  geom_point(aes(x=reorder(Time,desc(Time)), y=Control1),colour="red")+
  geom_point(aes(x=reorder(Time,desc(Time)), y=Control2),colour="black")+
  geom_point(aes(x=reorder(Time,desc(Time)), y=Treatment),colour="blue")+
  ylab("Comparecimento")
p

#diagrama com duas variáveis (dummie)

diagrama <- data.frame(
  Time=factor(c("1º tuno", "2º turno")),
  Control =c(control$pc_comparecimento,control_2$pc_comparecimento_2),
  Treatment=c(tratament$pc_comparecimento, tratament_2$pc_comparecimento_2)
)
diagrama


p<-ggplot(data=diagrama)+
  geom_point(aes(x=reorder(Time,desc(Time)), y= Control),colour="red")+
  geom_point(aes(x=reorder(Time,desc(Time)), y=Treatment),colour="blue")+
  ylab("Comparecimento")
p




#Tratamento de dados para a regressão

#Dados sobre a aplicação do passe livre
passe_livre_r <- read.csv2("https://raw.githubusercontent.com/ulissesgdm/trabalho_final/main/passe_livre_r.csv", stringsAsFactors = T, sep = ";")

#Filtrando a base de dados do TSE para obter os dados correspondentes ao comparecimento no 1 e 2 turno para as eleições presidenciais

comparecimento_1 <- read.csv2("https://github.com/ulissesgdm/trabalho_final/raw/main/comparecimento-votacao-munic%C3%ADpio_2022_1_turno.csv", stringsAsFactors = T, sep = ";", encoding = "latin1") %>% filter(ds_cargo == "Presidente")

comparecimento_1 <- select (comparecimento_1, sg_uf,nm_municipio, pc_secoes_agregadas, pc_comparecimento, pc_abstencoes, qt_aptos, qt_comparecimento, qt_abstencoes)

comparecimento_1 <- comparecimento_1 %>% filter(qt_aptos > 200000)

#criar variável turno

comparecimento_1$turno <- sample(1:1, 97, replace = T)

#adicionar a variável de tratamento

comparecimento_1 <- fuzzyjoin::stringdist_join(comparecimento_1, passe_livre_r, by = c('nm_municipio' = 'nome_cid'), mode='left')



#Dados 2 turno

comparecimento_2 <- read.csv2("https://github.com/ulissesgdm/trabalho_final/raw/main/comparecimento-votacao-munic%C3%ADpio_2022_2_turno.csv", stringsAsFactors = T, sep = ";", encoding = "latin1") %>% filter(ds_cargo == "Presidente")

comparecimento_2 <- select (comparecimento_2, sg_uf,nm_municipio, pc_secoes_agregadas, pc_comparecimento, pc_abstencoes, qt_aptos, qt_comparecimento, qt_abstencoes)

comparecimento_2 <- comparecimento_2 %>% filter(qt_aptos > 200000)

#criar variável turno

comparecimento_2$turno <- sample(2:2, 97, replace = T)

#adicionar a variável de tratamento

comparecimento_2 <- fuzzyjoin::stringdist_join(comparecimento_2, passe_livre_r, by = c('nm_municipio' = 'nome_cid'), mode='left')



#Unir verticalmente as bases de dados do 1 e 2 turno

comparecimento_r <- bind_rows(comparecimento_1, comparecimento_2)



# Retirando valores ausentes

comparecimento_r <- na.omit(comparecimento_r)



#Regressão

comparecimento_r$interacao <- comparecimento_r$turno*comparecimento_r$tratamento

comparecimento_r$tratamento <- as.factor(comparecimento_r$tratamento)

reg <- lm(pc_comparecimento ~ tratamento + qt_aptos +interacao + turno + uf , data = comparecimento_r )
summary(reg)



