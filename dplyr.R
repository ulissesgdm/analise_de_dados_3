library(dplyr)
library(haven)

ESEB2018 <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/ESEB2018.sav")

eseb <- ESEB2018 %>% mutate(Q12P2_B = as_factor(Q12P2_B)) 

#Sumário
count(eseb, Q12P2_B)

#Agrupamento
eseb %>% group_by(Q12P2_B) %>% summarise(avg = mean(D1A_ID))

#Manipulação
eseb_id <- arrange(eseb, D1A_ID)#Ordenar

eseb_idoso <- eseb  %>%  filter(D1A_ID >59)

count(eseb_idoso, Q12P2_B)

#Nova variável: voto de que gosta de Lula e Bolsonaro
eseb_amb <-eseb %>% filter(Q1610 < 11, Q1607 < 11) %>% mutate(ambiguidade = Q1607 + Q1610)

plot(eseb_amb$ambiguidade)
