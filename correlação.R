## CORRELAÇÃO COM R ##
# PRIMEIRO, VAMOS CARREGAR OS PACOTES
pacman::p_load(corrplot, dplyr, ggplot2)

# TABELA DE CORRELAÇÃO COM TODAS AS VARIÁVEIS #
cor(beaver2)

# GRÁFICOS DE DISPERSÃO PAREADOS DAS VARIÁVEIS #
pairs(beaver2)

# CORRPLOT DAS VARIÁVEIS #
beaverCor <- cor(beaver2)
corrplot(beaverCor, method = "number", order = 'alphabet')
corrplot(beaverCor, order = 'alphabet') 

#Há uma correlação positiva entre a atividade dos castores e a temperatura