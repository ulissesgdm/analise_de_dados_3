castor <- beaver1
plantas <- iris
library(readxl)
install.packages('writexl')
library(writexl)

install.packages('microbenchmark')
library(microbenchmark)


saveRDS(plantas, "bases_tratadas/plantas.rds")

write.csv2(plantas, "bases_tratadas/plantas.csv")

write_xlsx(plantas, "bases_tratadas/plantas.xlsx")

microbenchmark(a <- saveRDS(plantas, "bases_tratadas/plantas.rds"), b <- write.csv2(plantas, "bases_tratadas/plantas.csv"), c <- write_xlsx(plantas, "bases_tratadas/plantas.xlsx")), times = 30L)

microbenchmark(a <- readRDS('bases_tratadas/plantas.rds'), b <- read.csv2('bases_tratadas/plantas.csv', sep = ';'), c <- read_excel(plantas, "bases_tratadas/plantas.xlsx", sheet=1)  times = 10L)
