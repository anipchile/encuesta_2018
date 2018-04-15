#> -------------------------------------------------------------------------------------------
#> Proyecto: Segunda Encuesta de Inserción de Doctores, ANIP 2018
#> Archivo:
#> Autor: Cristian Bravo Lillo <cristian@bravolillo.xyz>
#> -------------------------------------------------------------------------------------------

#library(stringr)
clio.root = './'
source('functions.general.r')
source('functions.graphs.r')

#> Leemos y limpiamos los datos
datos <- read.csv('../encuesta/short-answers-20180320.csv')

#> Eliminamos las respuestas de las personas que dijeron que no querian participar
datos <- datos[ datos$G1Q00002=='Y', ]
datos$G1Q00002 <- NULL

#> We relabel the degree.
datos$G1Q00001[ datos$G1Q00001=='A2' ] <- 'MSc'
datos$G1Q00001[ datos$G1Q00001=='A3' ] <- 'PhD'

#> We transform into booleans
postfix <- "\\.SQ\\d+\\."
for (i in c('G2Q00007', 'G2Q00008', 'G2Q00009', 'G2Q00010', 'G3Q00008', 'G3Q00009', 'G5Q00014', 'G5Q00022')) {
	thiscol <- paste("^", i, postfix, sep='')
	thislist <- grep(thiscol, names(datos))
	datos[thislist] <- datos[thislist]=='Y'
}

for (i in c('G4Q00001', 'G4Q00005', 'G5Q00001', 'G5Q00002', 'G5Q00012', 'G5Q00018', 'G5Q00020', 'G5Q00023')) {
	datos[,c(i)] <- datos[,c(i)]=='Y'
}
rm(i,postfix,thiscol,thislist)




