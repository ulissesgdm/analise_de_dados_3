pacman::p_load(ade4, car, caret, corrplot, data.table, dplyr, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse, haven)

install.packages("multcomp")
library(multcomp)
install.packages("party")
library(party)

acidentes_21 <- read.csv2('http://dados.recife.pe.gov.br/datastore/dump/2caa8f41-ccd9-4ea5-906d-f66017d6e107?bom=True', stringsAsFactors = T, sep = ",") 

acidentes_21d <- acm.disjonctif(as.data.frame(acidentes_21$natureza_acidente))

acidentes_21 <- cbind(acidentes_21, acidentes_21d)

acidentes_21 <- rename(acidentes_21, fatal = acidentes_21$natureza_acidente.VÍTIMAFATAL)

# Treino e Teste: Pré-processamento
particaoacidentes = createDataPartition(acidentes_21$`acidentes_21$natureza_acidente.VÍTIMA FATAL`, p=.7, list = F) # cria a partição 70-30
treinoacidentes = acidentes_21[particaoacidentes, ] # treino
testeacidentes = acidentes_21[-particaoacidentes, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

prop.table(table(treinoacidentes$natureza_acidente))

#Matriz de custo

matrizCusto <- matrix(c(0,1,100,0), ncol = 2)
rownames(matrizCusto) <- levels(treinoacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
colnames(matrizCusto) <- levels(treinoacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
matrizCusto

ACIDENTES_RF_CLASS <- randomForest(`acidentes_21$natureza_acidente.VÍTIMA FATAL` ~ auto + moto + ciclom + ciclista + pedestre , data = treinoacidentes, method = "cforest", parms = list(loss = matrizCusto))
ACIDENTES_RF_CLASS

#criar predição
predicaoACIDENTES_RF_CLASS = predict(ACIDENTES_RF_CLASS, testeacidentes) 
cmACIDENTES_RF_CLASS <- confusionMatrix(predicaoACIDENTES_RF_CLASS, testeacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
cmACIDENTES_RF_CLASS



ACIDENTES_C5_CLASS <- train((`acidentes_21$natureza_acidente.VÍTIMA FATAL` ~ auto + moto + ciclom + ciclista + pedestre, data = treinoacidentes, method = "C5.0Cost", trControl = train.control)
ACIDENTES_C5_CLASS

predicaoACIDENTES_C5_CLASS = predict(ACIDENTES_C5_CLASS, testeacidentes) # criar predição
cmACIDENTES_C5_CLASS <- confusionMatrix(predicaoACIDENTES_C5_CLASS, testeacidentes$`acidentes_21$natureza_acidente.VÍTIMA FATAL`)
cmACIDENTES_C5_CLASS

#Tentei o modelo com as seguintes variáveis (Natureza_acidente + tipo + acidente_verificado + tempo_clima + condicao_via + conservacao_via)e com as aplicadas acima. O modelo Random Forest funcionou, mas a matriz de confusão apresenta erro. Já o modelo C5 não reconhece as variáveis. Busquei no Stack Overflow, que indicou que o problema poderia ser a presença de NA's nas variáveis, por isso testei as numéricas (auto, ciclista, pedestre), mas mesmo assim não deu certo.
