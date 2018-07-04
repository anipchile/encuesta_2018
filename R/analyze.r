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

#> A bit of cleaning
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
rm(tab1, tab2, cdh)

#> ----------------------------------------------------------------------------------------
#> Seccion 1: Sobre situacion academica
#> Año de obtencion de pregrado (G2Q00002)
thisoutput <- grph.createOutputDevice('anno-pregrado-mscs.png', width=600, height=750)
grph.simpleBarplot(as.matrix(table(mscs$G2Q00002)), mar=c(2, 2.5, 0.5, 0.3), mgp=c(1, 0.5, 0),
				   filename=thisoutput, main='Año de obtención del pregrado, MScs')

thisoutput <- grph.createOutputDevice('anno-pregrado-phds.png', width=600, height=750)
grph.simpleBarplot(as.matrix(table(phds$G2Q00002)), mar=c(2, 2.5, 0.5, 0.3), mgp=c(1, 0.5, 0),
				   filename=thisoutput, main='Año de obtención del pregrado, PhDs')
rm(thisoutput)

#> Disciplina del pregrado (G2Q00001)
tmp1 <- table(mscs$G2Q00001)
tmp1 <- tmp1[order(tmp1, decreasing = T)]

tmp2 <- table(phds$G2Q00001)
tmp2 <- tmp2[order(tmp2, decreasing = T)]
rm(tmp1,tmp2)

#> Año de obtención de posgrado (G2Q00004)
thisoutput <- grph.createOutputDevice('anno-mscs.png', width=600, height=750)
grph.simpleBarplot(as.matrix(table(mscs$G2Q00004)), mar=c(2, 2.5, 0.5, 0.3), mgp=c(1, 0.5, 0), filename=thisoutput, main='Año de obtención del Máster')

thisoutput <- grph.createOutputDevice('anno-phds.png', width=600, height=750)
grph.simpleBarplot(as.matrix(table(phds$G2Q00004)), mar=c(2, 2.5, 0.5, 0.3), mgp=c(1, 0.5, 0), filename=thisoutput, main='Año de obtención del Doctorado')

#> Disciplina del posgrado (G2Q00003)
tmp1 <- table(mscs$G2Q00003)
tmp1 <- tmp1[order(tmp1, decreasing = T)]

tmp2 <- table(phds$G2Q00003)
tmp2 <- tmp2[order(tmp2, decreasing = T)]
rm(tmp1,tmp2)

#> Lugar de realización del posgrado (G2Q00005)
tmp1 <- table(mscs$G2Q00005)
tmp1 <- tmp1[order(tmp1, decreasing = T)]

tmp2 <- table(phds$G2Q00005)
tmp2 <- tmp2[order(tmp2, decreasing = T)]
rm(tmp1,tmp2)

#> Num de articulos revisados por pares en los ultimos 6 annos (G2Q00006)
thisoutput <- grph.createOutputDevice('mscs-num-peer-reviewed.png', width=600, height=750)
grph.simpleBarplot(as.matrix(table(mscs$G2Q00006)), mar=c(2, 2.5, 0.5, 0.3), mgp=c(1, 0.5, 0), filename=thisoutput, main='Número de artículos revisados por pares, MScs')

thisoutput <- grph.createOutputDevice('phds-num-peer-reviewed.png', width=600, height=750)
grph.simpleBarplot(as.matrix(table(phds$G2Q00006)), mar=c(2, 2.5, 0.5, 0.3), mgp=c(1, 0.5, 0), filename=thisoutput, main='Número de artículos revisados por pares, PhDs')
rm(thisoutput)

#> Financiamiento del posgrado (G2Q00007)
tmp <- matrix(nrow=4, data=c(
	sum(mscs$G2Q00007.SQ001.), sum(mscs$G2Q00007.SQ002.), sum(mscs$G2Q00007.SQ003.), sum(mscs$G2Q00007.SQ004.),
	sum(phds$G2Q00007.SQ001.), sum(phds$G2Q00007.SQ002.), sum(phds$G2Q00007.SQ003.), sum(phds$G2Q00007.SQ004.)
))
colnames(tmp) <- c('MSc', 'PhD')
rownames(tmp) <- c('Fondos personales', 'Trabajo durante el estudio', 'Fondos no personales nacionales', 'Fondos no personales extranjeros')

#> Que tipo de trabajo (G2Q00008)
tmp <- matrix(nrow=7, data=c(
	sum(mscs$G2Q00008.SQ001.), sum(mscs$G2Q00008.SQ002.), sum(mscs$G2Q00008.SQ003.), sum(mscs$G2Q00008.SQ004.), sum(mscs$G2Q00008.SQ005.), sum(mscs$G2Q00008.SQ006.),
	sum(mscs$G2Q00008.other.!=''),
	sum(phds$G2Q00008.SQ001.), sum(phds$G2Q00008.SQ002.), sum(phds$G2Q00008.SQ003.), sum(phds$G2Q00008.SQ004.), sum(phds$G2Q00008.SQ005.), sum(phds$G2Q00008.SQ006.),
	sum(phds$G2Q00008.other.!='')
))
colnames(tmp) <- c('MSc', 'PhD')
rownames(tmp) <- c('Ayudantías, mi área', 'Ayudantías, otras áreas', 'Pasantías','Trabajo en lugar sin relación con estudios',
				   'Trabajo fuera de lugar','Trabajo remoto','Otro')

#> Que tipo de fondos (G2Q00009)
tmp <- matrix(nrow=4, data=c(
	sum(mscs$G2Q00009.SQ001.), sum(mscs$G2Q00009.SQ002.), sum(mscs$G2Q00009.SQ003.), sum(mscs$G2Q00009.SQ004.),
	sum(phds$G2Q00009.SQ001.), sum(phds$G2Q00009.SQ002.), sum(phds$G2Q00009.SQ003.), sum(phds$G2Q00009.SQ004.)
))
colnames(tmp) <- c('MSc', 'PhD')
rownames(tmp) <- c('Beca o premio', 'Crédito de un banco', 'Financiamiento de empleador', 'Otros')


#> ----------------------------------------------------------------------------------------
#> Seccion 2: Sobre realizacion de postdoctorados

#> Realizando postdoc? (G4Q00001), más de uno? (G4Q00005)
tmp <- c(
	sum(!phds$G4Q00001), #> 0 postdocs
	sum(phds$G4Q00001 & !phds$G4Q00005),  #> 1 postdoc
	sum(phds$G4Q00005) #> More than 1
)

#> Disciplina (G4Q00002)
tmp <- table(phds$G4Q00002)
tmp <- tmp[order(tmp, decreasing = T)]

#> Lugar (G4Q00004)
tmp <- table(phds$G4Q00004)
tmp <- tmp[order(tmp, decreasing = T)]


#> ----------------------------------------------------------------------------------------
#> Seccion 3: Sobre reinsercion en Chile

#> Estas en Chile? (G3Q00001)
tmp <- table(datos$G1Q00001, datos$G3Q00001)[,2:5]
tmp <- cbind(tmp, rowSums(tmp))

#> Hace cuanto tiempo estas en Chile (G3Q00004) y cuanto tiempo demoraste en encontrar
#> tu primer trabajo (G3Q00005)
table(datos$G1Q00001, datos$G3Q00004)
table(datos$G1Q00001, datos$G3Q00005)

#> Como calificarias tu experiencia de reinsercion en Chile
scalecols = c(
	'#FF0000','#FF3333','#FF8080',
	'lightgoldenrod1',
	'#38FF38','#00EB00','#00B800'
)

tmp <- as.matrix(table(datos$G1Q00001, datos$G3Q00007))
tmp[1,] <- tmp[1,]/sum(tmp[1,])
tmp[2,] <- tmp[2,]/sum(tmp[2,])
colnames(tmp) <- c('Muy mala', '', '', 'Neutra', '', '', 'Muy buena')
thisoutput <- grph.createOutputDevice('percepcion2.pdf', width=15, height=3)
grph.stackedFullProportionBarplot(tmp, filename=thisoutput, mar=c(1.8, 2.5, 1.8, 9), cex=c(1,1.1,1),
								  thiscolor = scalecols, legendoffset = 0.13, useratiowithin = T, digits = 1, cutoff = 0.03)

#> Porque razon tienes que volver (G3Q00009)
table(datos$G1Q00001, datos$G3Q00009.SQ001.)
table(datos$G1Q00001, datos$G3Q00009.SQ002.)
table(datos$G3Q00009.other., datos$G1Q00001)

#> Porque razon no volveras (G3Q00008)
table(datos$G1Q00001, datos$G3Q00008.SQ001.)
table(datos$G1Q00001, datos$G3Q00008.SQ002.)
table(datos$G1Q00001, datos$G3Q00008.SQ003.)
table(datos$G1Q00001, datos$G3Q00008.SQ004.)
table(datos$G3Q00008.other., datos$G1Q00001)

#> Tasas de desempleo.
tmp1 <- table(mscs$G5Q00002[ mscs$G3Q00001!='A4' ], mscs$G5Q00023[ mscs$G3Q00001!='A4' ])
tmp2 <- table(phds$G5Q00002[ phds$G3Q00001!='A4' ], phds$G5Q00023[ phds$G3Q00001!='A4' ])

#> Estabilidad de empleo
tmp1 <- table(mscs$G5Q00025[ mscs$G5Q00002 ])
tmp2 <- table(phds$G5Q00025[ phds$G5Q00002 ])


#> ----------------------------------------------------------------------------------------
#> Estudio de algunas variables

#> 1:
#> Salida: Tiempo que toma encontrar trabajo (G3Q00005), ordinal.
#> Entrada:
#>		+ Si ha sido un anhelo trabajar en ambito academico (G6Q00005), Yes/No
#>		+ Genero (G6Q00002), categorical
#>		+ Edad (G6Q00003), numerical
#>		+ Grado academico (G1Q00001), categorical
#>		+ Pregrado (G2Q00001), categorical
#>		+ Anno de pregrado (G2Q00002), numerical
#>		+ Posgrado (G2Q00003), categorical
#>		+ Anno de posgrado (G2Q00004), numerical
#>		+ Num. de articulos (G2Q00006), numerical
#>		+ Realizacion de postdoc (G4Q00001), yes/no

est1 <- datos[ datos$G3Q00001=='A1' & datos$G3Q00005!='DNS', ]
est1$G3Q00005 <- ordered(est1$G3Q00005, levels = c('A0','A1','A2','A3','A4','A5','A6','A7','DNF'))
est1$G1Q00001 <- factor(est1$G1Q00001, levels = c('MSc','PhD'))
est1$G2Q00001 <- factor(est1$G2Q00001)
est1$G2Q00003 <- factor(est1$G2Q00003)
est1$G2Q00002[ est1$G2Q00002=='B1990' ] <- 1990
est1$G2Q00002 <- as.numeric(est1$G2Q00002)
est1$G2Q00004[ est1$G2Q00004=='B1990' ] <- 1990
est1$G2Q00004 <- as.numeric(est1$G2Q00004)

library(MASS)

fin1 <- polr(G3Q00005 ~ G6Q00002 + G6Q00003 + G6Q00005 + G1Q00001 + G2Q00001 + G2Q00002 + G2Q00003 + G2Q00004 + G2Q00006 + G4Q00001,
			 data = est1, Hess = T)

fin2 <- polr(G3Q00005 ~ G6Q00005 + G1Q00001 + G2Q00002 + G2Q00004 + G2Q00006 + G4Q00001,
			 data = est1, Hess = T)

summary(fin2)
ctable <- coef(summary(fin2))
p <- pnorm(abs(ctable[, 't value']), lower.tail = FALSE) * 2
ctable <- cbind(ctable, 'p value' = p)
ci <- confint(fin2)

#> Analisis sobre si el num de papers y el num de annos influye o no en el tener postdoc
est <- phds[,c('G2Q00006', 'G2Q00004', 'G4Q00001','G6Q00002','G6Q00003')]
colnames(est) <- c('NumPapers','Anno','TienePosdoc','Genero','Edad')
est$NumPapers <- as.numeric(est$NumPapers)
est$Anno[ est$Anno=='B1990' ] <- 1990
est$Anno <- as.numeric(est$Anno)
est$NumYears <- 2018 - est$Anno
est$Genero[ est$Genero=='A1' ] <- 'Female'
est$Genero[ est$Genero=='A2' ] <- 'Male'
est$Genero[ est$Genero=='A3' ] <- 'Other'
est$Genero[ est$Genero=='A4' ] <- 'NR'
est$Genero <- factor(est$Genero)

reg <- glm(formula = TienePosdoc ~ NumPapers + NumYears + Genero, data = est, family = binomial(link = 'logit'))
