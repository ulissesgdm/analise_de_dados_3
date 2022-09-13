
# imputação de valores em outliers ou missing
pacman::p_load(data.table, Hmisc, VIM) 

eseb <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/ESEB%202002.sav")

# imputação por predição

reg_eseb <- lm(p176 ~ estrato, data = eseb) # criamos a regressão
esebNAIndex <- is.na(eseb$p176) # localizamos os NA
eseb$p176[esebNAIndex] <- predict(reg_eseb, newdata = eseb[esebNAIndex, ]) # imputamos os valores preditos


## Hot deck
# imputação aleatória

# baixar novamente o banco eseb

(eseb$p176 <- impute(eseb$p176, "random")) # fazemos a imputação aleatória

# imputação por instâncias /semelhança

eseb <- kNN(eseb)