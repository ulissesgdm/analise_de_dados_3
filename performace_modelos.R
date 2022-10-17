
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

# Treinamentos
# Bagging com Floresta Aleatória
acidentes_RF <- train(`acidentes_21$natureza_acidente.VÍTIMA FATAL` ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "cforest", trControl = train.control)

plot(acidentes_RF) # evolução do modelo
plot(varImp(acidentes_RF)) # plot de importância

## Regressão Linear
acidentes_LM <- train(`acidentes_21$natureza_acidente.VÍTIMA FATAL` ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "lm", trControl = train.control)
summary(acidentes_LM) # sumário do modelo linear
plot(varImp(acidentes_LM))

## Árvore de Decisão
acidentes_RPART <- train(`acidentes_21$natureza_acidente.VÍTIMA FATAL` ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "rpart", trControl = train.control)

summary(acidentes_RPART)
fancyRpartPlot(acidentes_RPART$finalModel) # desenho da árvore
plot(varImp(acidentes_RPART)) # importância das variáveis


melhor_modelo <- resamples(list(LM = acidentes_LM, RPART = acidentes_RPART, RF = acidentes_RF))
melhor_modelo

summary(melhor_modelo)

#Os modelos não são bons preditores. Tanto o R² quanto os resultados obtidos a partir das regressões não indicam que as variáveis levantadas sejam importantes para a criação de modelos preditivos.
