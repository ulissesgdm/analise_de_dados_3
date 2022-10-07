pacman::p_load(ade4, car, caret, corrplot, data.table, dplyr, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse, haven, party)


acidentes_21 <- read.csv2('http://dados.recife.pe.gov.br/datastore/dump/2caa8f41-ccd9-4ea5-906d-f66017d6e107?bom=True', stringsAsFactors = T, sep = ",") 



acidentes_21d <- acm.disjonctif(as.data.frame(acidentes_21$natureza_acidente))

acidentes_21 <- cbind(acidentes_21, acidentes_21d)

acidentes_21 <- rename(acidentes_21, fatal = acidentes_21$'natureza_acidente.VÍTIMAFATAL')

# AED 
status(acidentes_21) # explorar a qualidade das variáveis
freq(acidentes_21) # explorar os fatores
plot_num(acidentes_21) # exploração das variáveis numéricas
profiling_num(acidentes_21) # estatísticas das variáveis numéricas

corrplot(cor(acidentes_21[ , c(15,16,39:42)])) # correlação entre as variáveis

# Treino e Teste: Pré-processamento
particaoacidentes = createDataPartition(acidentes_21$vitimasfatais, p=.7, list = F) # cria a partição 70-30
treinoacidentes = acidentes_21[particaoacidentes, ] # treino
testeacidentes = acidentes_21[-particaoacidentes, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

# Treinamentos
## Regressão Linear
acidentes_LM <- train(fatal ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "lm", trControl = train.control)
summary(acidentes_LM) # sumário do modelo linear
plot(varImp(acidentes_LM))

## Árvore de Decisão
acidentes_RPART <- train(vitimasfatais ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "rpart", trControl = train.control)

summary(acidentes_RPART)
fancyRpartPlot(acidentes_RPART$finalModel) # desenho da árvore
plot(varImp(acidentes_RPART)) # importância das variáveis

# Bagging com Floresta Aleatória
aidentes_RF <- train(vitimasfatais ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "cforest", trControl = train.control)

plot(acidentes_RF) # evolução do modelo
plot(varImp(acidentes_RF)) # plot de importância


# Boosting com Boosted Generalized Linear Model
acidentes_ADA <- train(vitimasfatais ~ tipo + auto + moto + ciclista + conservacao_via, data = treinoacidentes, method = "glmboost", trControl = train.control)

plot(acidentes_ADA) # evolução do modelo
print(acidentes_ADA) # modelo
summary(acidentes_ADA) # sumário

# Toda vez que tento aplicar o modelos Bagging com Floresta aleatória e Boosting o R solicita que eu paixe o pacote party (mesmo ele já estando baixado)





