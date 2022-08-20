#Utilizei o banco de dados iris

#histogramas
hist(iris$Petal.Length)
hist(iris$Sepal.Length)


#centralização
base_centralizada<- iris$Petal.Length - mean(iris$Petal.Length)
hist(base_centralizada)

base_centralizada2<- iris$Sepal.Length - mean(iris$Sepal.Length)
hist(base_centralizada2)

#Em ambos os testes, apesar da centralização dos dados, ocorreu a criação de novos valores, inexistentes na base de dados original