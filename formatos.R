csv <- read.csv2('http://dados.recife.pe.gov.br/dataset/7ccb3816-0d62-49e1-b39a-3159870883b0/resource/9664de94-9f07-4adc-848d-b6ef56510762/download/vacinados.csv', sep = ';', encoding = 'UTF-8'
)

install.packages('rjson')
library(rjson)

json <- fromJSON(file='http://dados.recife.pe.gov.br/datastore/dump/2caa8f41-ccd9-4ea5-906d-f66017d6e107?format=json')

json <- as.data.frame(json)

install.packages('XML')
library(XML)

xml <- xmlToDataFrame('http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/reed.xml')
