pacman::p_load(dplyr, ggplot2, data.table, gridExtra)

# carregar dados covid19 Pernambuco
acidentes_21 <- fread('http://dados.recife.pe.gov.br/datastore/dump/2caa8f41-ccd9-4ea5-906d-f66017d6e107?bom=True')

# agrupar casos por município ajustando variáveis
acidentes_bairro <- acidentes_21 %>% count(bairro, sort = T, name = 'acidentes_bairro') %>% mutate(acidentes_bairro2 = sqrt(acidentes_bairro), casosLog = log10(acidentes_bairro))

# criar loop para os diferentes gráficoss
nomeVar <- names(acidentes_bairro)[2:4] # passar nomes das vars para vetor
listaPlots <- NULL

for(i in nomeVar) {
  plot <- acidentes_bairro %>% ggplot(aes_string(x = 'bairro', y=i)) + geom_bar(stat = "identity") + labs(x = "bairro")
  listaPlots[[length(listaPlots) + 1]] <-plot
} # criar lista com os plots

# printar todos os plots, lado a lado
grid.arrange(listaPlots[[1]], listaPlots[[2]], listaPlots[[3]], ncol=3)
