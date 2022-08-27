summary(ToothGrowth)
View(ToothGrowth)

apply(ToothGrowth[ ,-2], 2, median)
lapply(ToothGrowth[, -2], median)
sapply(ToothGrowth[, -2], median) 

sapply(ToothGrowth[ , -2], hist)
mapply(hist, ToothGrowth[ , -2], MoreArgs=list(main='Histograma', xlab = 'Valores', ylab = 'Frequência'))
