#> -------------------------------------------------------------------------------------------
#> Proyecto: Segunda Encuesta de Inserción de Doctores, ANIP 2018
#> Archivo: analyze.r
#> Autor: Cristian Bravo Lillo <cristian@bravolillo.xyz>
#> -------------------------------------------------------------------------------------------

clio.root = './'
source('functions.general.r')
source('functions.graphs.r')

#> Leemos y limpiamos los datos
datoscompletos <- read.csv('../encuesta/short-answers-20180320.csv')

#> Eliminamos las respuestas de las personas que dijeron que no querian participar
datoscompletos <- datoscompletos[ datoscompletos$G1Q00002=='Y', ]
datoscompletos$G1Q00002 <- NULL

#> Eliminamos los datos de las personas que respondieron hasta la sección 3.
datoscompletos <- datoscompletos[ datoscompletos$lastpage>3, ]
datos <- datoscompletos[ datoscompletos$lastpage==6, ]

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

for (i in c('G4Q00001', 'G4Q00005', 'G5Q00001', 'G5Q00002', 'G5Q00012',
			'G5Q00018', 'G5Q00020', 'G5Q00023', 'G6Q00005', 'G6Q00006', 'G6Q00007')) {
	datos[,c(i)] <- datos[,c(i)]=='Y'
}
rm(i,postfix,thiscol,thislist)

#> For convenience we split the phds from the mscs
phds <- datos[ datos$G1Q00001=='PhD', ]
mscs <- datos[ datos$G1Q00001=='MSc', ]

#> Comparamos con la encuesta de trayectoria de doctores de Economía
tab1 <- table(datos$G1Q00001, datos$G6Q00002)

tab2 <- table(phds$G6Q00003, phds$G6Q00002)
tab2 <- rbind(
	colSums(tab2[as.numeric(rownames(tab2))<35,]),
	colSums(tab2[as.numeric(rownames(tab2))>=35 & as.numeric(rownames(tab2))<=44,]),
	colSums(tab2[as.numeric(rownames(tab2))>=45 & as.numeric(rownames(tab2))<=54,]),
	colSums(tab2[as.numeric(rownames(tab2))>=55 & as.numeric(rownames(tab2))<=64,]),
	tab2[as.numeric(rownames(tab2))>=65,]
)
cdh <- matrix(data = c(984,1397,798,442,78,1212,2716,1326,1319,321), nrow = 5)
rownames(tab2) <- rownames(cdh) <- c('Menos de 35 años','Entre 35 y 44 años','Entre 45 y 54 años','Entre 55 y 64 años','65 años o más')
colnames(tab1) <- colnames(tab2) <- c('Mujeres','Hombres','Otro','NR')
colnames(cdh) <- c('Mujeres', 'Hombres')

#> Generamos subtotales para las tablas
tab1 <- cbind(tab1, rowSums(tab1))
tab1 <- rbind(tab1, colSums(tab1))
tab2 <- cbind(tab2, rowSums(tab2))
tab2 <- rbind(tab2, colSums(tab2))
cdh <- cbind(cdh, rowSums(cdh))
cdh <- rbind(cdh, colSums(cdh))
