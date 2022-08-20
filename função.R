diferença_mediana <- function(x) {
  x <- x - median(x)
  return(x)
}

diferença_mediana(iris$Sepal.Length)
teste <- diferença_mediana(iris$Sepal.Length)

hist(teste)
