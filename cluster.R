# carregar as bibliotecas
pacman::p_load(cluster, dplyr, factoextra, ggplot2)

View(USArrests)

# pré-processamento
violencia_cluster <- USArrests %>% select(Murder, Rape)
str(violencia_cluster)

# definir semente aleatória
set.seed(1)

# Método do Cotovelo
fviz_nbclust(violencia_cluster, kmeans, method = "wss")

# Agrupamento com kmeans
cls <- kmeans(x = violencia_cluster, centers = 5) # aprendizagem ns
violencia_cluster$cluster <- as.factor(cls$cluster) # passamos os clusters para a base original
head(violencia_cluster)

# plot com função própria do pacote
clusplot(violencia_cluster, cls$cluster, xlab = 'Fator1', ylab = 'Fator2', main = 'Agrupamento Estudantes', lines = 0, shade = F, color = TRUE, labels = 2)