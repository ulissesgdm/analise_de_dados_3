
clube_campeao <- c(1, 1, 1, 2, 1, 1, 2, 3, 2, 4, 3, 3) #Campeões pernambucanos desde 2011

recode <- c(Santa_Cruz = 1, Sport = 2, Nautico = 3, Salgueiro = 4)

(clube_campeao <- factor(clube_campeao, levels = recode, labels = names(recode)))

(clube_campeao <- relevel(clube_campeao, ref = "Santa_Cruz"))

divisao <- c(4, 3, 3, 1, 2, 1, 1, 3, 2, 4, 2, 2) #divisão em que os clubes campeões estavam quando consquistaram os respectivos títulos

(clube_campeao <- reorder(clube_campeao, divisao)) 