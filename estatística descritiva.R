view(USArrests)


table(USArrests$Rape)

# Tabela de frequência relativa da variável
prop.table(table(USArrests$Rape))

#Média da variável
mean(USArrests$Rape)

## Mediana da variável 
median(USArrests$Rape)

## Separatrizes da variável
quantile(USArrests$Rape, probs=0.75)
quantile(USArrests$Rape, probs=0.10)
quantile(USArrests$Rape, probs=0.95)
boxplot(USArrests$Rape) # boxplot - gráfico que resume as sepatrizes

## Desvio-padrão da variável 
sd(USArrests$Rape)
plot(USArrests$Rape)

## Sumário descritivo básico das variáveis
summary(USArrests)

## Sumário descritivo completo das variáveis usando o pacote fBasics
pacman::p_load(fBasics)
basicStats(USArrests)
hist(USArrests$Rape) # histograma - gráfico que permite conhecer a curva dos dados
