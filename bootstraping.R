dist_norm<- rnorm(250)
summary(dist_norm)


#amostragem
sample(dist_norm, 15, replace = TRUE)

#bootstraping
boots_teste<- sample(dist_norm, 10, replace = TRUE)

#estatística com bootstraping
media10<- replicate(10, mean(sample(dist_norm, 10, replace = TRUE)))
summary(media10)

media50<- replicate(50, mean(sample(dist_norm, 10, replace = TRUE)))
summary(media50)

media100<- replicate(100, mean(sample(dist_norm, 10, replace = TRUE)))
summary(media100)

#comparando as médias
mean(media10)
mean(media50)
mean(media100)
mean(dist_norm)

