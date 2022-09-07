## QUI-QUADRADO COM R ##
# PRIMEIRO, VAMOS CARREGAR OS PACOTES
pacman::p_load(data.table, ggplot2)


# AGORA, A BASE DE DADOS CAR EVALUATION #
breast_cancer <- fread('https://raw.githubusercontent.com/hugoavmedeiros/cp_com_r/master/bases_tratadas/breast_cancer.csv', stringsAsFactors = T)
breast_cancer <- fread('bases_tratadas/breast_cancer.csv', stringsAsFactors = T)

# TABELA DE CONTINGÊNCIA #
tabela <- table(colon$sex, colon$nodes)

# GRÁFICOS DE DISPERSÃO PAREADOS DAS VARIÁVEIS #
ggplot(colon) + aes(x = rx, fill = nodes) + geom_bar(position = "fill")

# TESTE QUI QUADRADO #
tabela1 <- chisq.test(tabela)
tabela1$observed
tabela1$expected

# CORRPLOT DAS VARIÁVEIS #
corrplot(tabela1$residuals, is.cor = FALSE)