
install.packages("multcomp")
library(multcomp)
install.packages("party")
library(party)

acidentes_21 <- read.csv2('http://dados.recife.pe.gov.br/datastore/dump/2caa8f41-ccd9-4ea5-906d-f66017d6e107?bom=True', stringsAsFactors = T, sep = ",") 

acidentes_21d <- acm.disjonctif(as.data.frame(acidentes_21$natureza_acidente))

acidentes_21 <- cbind(acidentes_21, acidentes_21d)

acidentes_21 <- rename(acidentes_21, fatal = acidentes_21$natureza_acidente.VÍTIMAFATAL)

acidentes_21$`acidentes_21$natureza_acidente.VÍTIMA FATAL` <- as.factor(acidentes_21$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
  
# Treino e Teste: Pré-processamento
particaoacidentes = createDataPartition(acidentes_21$`acidentes_21$natureza_acidente.VÍTIMA FATAL`, p=.7, list = F) # cria a partição 70-30
treinoacidentes = acidentes_21[particaoacidentes, ] # treino
testeacidentes = acidentes_21[-particaoacidentes, ] # - treino = teste

table(treinoacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)

# down / under
treinoacidentesDS <- downSample(x = treinoacidentes[, -ncol(treinoacidentes)], y = treinoacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
table(treinoacidentesDS$Class)   

# up
treinoacidentesUs <- upSample(x = treinoacidentes[, -ncol(treinoacidentes)], y = treinoacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
table(treinoacidentesUs$Class)  