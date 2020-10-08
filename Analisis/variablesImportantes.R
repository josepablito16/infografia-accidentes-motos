# Universidad del Valle de Guatemala
# Proyecto #1
# Integrantes: Oscar Ju√°rez, Jos√© Cifuentes, Luis Esturban
# Fecha: 30/03/2020

setwd("../AnalisisR")
library("ggpubr")
library(corrplot)
library(plyr)

# Leer la entrada de los datos
HechoTransito<-read.csv("../Data/HechoTransito.csv",stringsAsFactors = FALSE)
VehiculosInvolucrados<-read.csv("../Data/VehiculosInvolucrados.csv",stringsAsFactors = FALSE)
fallecidosLesionados<- read.csv("../Data/FallecidosLesionados.csv", stringsAsFactors = FALSE)
importaciones <- read.csv("../Data/importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)


# Tipo Cantidad de tipos de veh√?culsos durante todos los a√±os
cantTipoVeh <- table(importaciones[,"Tipo.de.Vehiculo"])
# Hacemos que el orden de la tabla sea descendiente
cantTipoVeh <- cantTipoVeh[order(cantTipoVeh, decreasing = TRUE)]

# Gr√°fico de barras
barplot(cantTipoVeh[1:5],
        main = "Total de veh√?culos importados desde el a√±o 2011",
        xlab = "Tipo de veh√?culo", ylab = "Cantidad de importaciones",
        col = "royalblue")

# Obtenemos total de motos y autom√≥viles importados desde
# el a√±o 2011
totalMotos <- cantTipoVeh[1]
totalCarros <- cantTipoVeh[2] + cantTipoVeh[3] + cantTipoVeh[4]
totalCamiones <- cantTipoVeh[5]

vectorTotalVeh <- c(totalMotos, totalCarros, totalCamiones)
# El tipo de veh√?culo que predomina es la moto, con un total de 1104306 unidades
# importadas, mientras que el autom√≥vil es el 32% de esa medida (347871 unidades)

# No obstante, es prudente sumar el auto, camioneta y pickup ya que estos medios de
# transporte los consideraremos como veh√?culos


# Gr√°fico de barras
barplot(vectorTotalVeh,
        main = "Total de veh√?culos importados desde el a√±o 2011",
        xlab = "Tipo de veh√?culo", ylab = "Cantidad de importaciones",
        col = "royalblue")


# Ahora, obtenemos el total de accidentes por tipo de veh√?culo
cantHechosVeh <- table(VehiculosInvolucrados[,"tipo_veh"])
# Ahora en orden descendiente
cantHechosVeh <- cantHechosVeh[order(cantHechosVeh, decreasing = TRUE)]

barp <- barplot(cantHechosVeh[1:6],
        main = "Cantidad de accidentes por tipo de vehiculo desde el a√±o 2016",
        xlab = "Tipo vehiculo", ylab = "Cantidad de accidentes",
        col = "royalblue", 
        names.arg = c("Motocicleta","Autom√≥vil","Pick Up","Ignorado","Camioneta Sport", "Cami√≥n"))
text(barp, 1, paste("total: ", vectorTotalAcc, sep="") ,cex=1, pos=3) 

# Hacemos lo mismo, sumamos el auto, pick up y la camioneta sport.
totalAccMotos <- cantHechosVeh[1]
totalAccCarros <- cantHechosVeh[2] + cantHechosVeh[3] + cantHechosVeh[5]
totalAccCamiones <- cantHechosVeh[4]

vectorTotalAcc <- c(totalAccMotos, totalAccCarros, totalAccCamiones)

# Gr√°fico de barras
barp <- barplot(vectorTotalAcc,
        main = "Comparaci√≥n de total accidentes de autom√≥viles y carros",
        xlab = "Tipo de veh√?culo", ylab = "Cantidad de accidentes",
        col = "royalblue", names.arg = c("Motocicletas", "Autom√≥vil","Cami√≥n"))
text(barp, 1, paste("total: ", vectorTotalAcc, sep="") ,cex=1, pos=3) 

# Procedemos a hacer un data frame con la informaci√≥n relevante
vehAcc <- data.frame(t(vectorTotalAcc))
vehAcc <- rbind(vehAcc, "Vehiculos"=vectorTotalVeh)
row.names(vehAcc) <- c("Cantidad de accidentes", "Total de veh√?culos")
colnames(vehAcc) <- c("Motocicletas", "Autom√≥vil","Cami√≥n")
barplot(
  as.matrix(vehAcc), col = c("royalblue", "grey"),
  main = "Proporci√≥n de cantidad de veh√?culos vs. cantidad de accidentes",
  ylab = "Proporci√≥n", xlab = "Tipo de veh√?culo")

prop.table(as.matrix(vehAcc), margin=2)





# AHORA, ANALIZAR FATALIDADES

# Creacion de tabla de frecuencias de fallecidos y lesionados
fall_les <- table(fallecidosLesionados[,c("tipo_veh", "fall_les")])


tablaTotales <- fall_les[1:5,]
row.names(tablaTotales) <- c("Autom√≥vil","Camioneta Sport","Pick Up","Motocicleta", "Cami√≥n")
colnames(tablaTotales) <- c("Cantidad de fatalidades", "Cantidad de lesiones")


ptable <- prop.table(t(fall_les[1:5,]), margin=2)
porcentajeFat <- c((format(round(ptable[1,1]*100,1),nsmall=1)),
                   (format(round(ptable[1,2]*100,1),nsmall=1)),
                   (format(round(ptable[1,3]*100,1),nsmall=1)),
                   (format(round(ptable[1,4]*100,1),nsmall=1)),
                   (format(round(ptable[1,5]*100,1),nsmall=1)))

porcentajeLes <- c((format(round(ptable[2,1]*100,1),nsmall=1)),
                   (format(round(ptable[2,2]*100,1),nsmall=1)),
                   (format(round(ptable[2,3]*100,1),nsmall=1)),
                   (format(round(ptable[2,4]*100,1),nsmall=1)),
                   (format(round(ptable[2,5]*100,1),nsmall=1)))
barp <- barplot(
  t(fall_les[1:5,]), col = c("royalblue", "grey"),
  main = "Proporci√≥n de fallecidos y lesionados totales acorde a un tipo de veh√?culo",
  xlab = "Tipo de veh√?culo", ylab = "Proporci√≥n",
  names.arg = c("Autom√≥vil","Camioneta Sport","Pick Up","Motocicleta", "Cami√≥n"),
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)
text(barp, 1, paste("lesiones: ", porcentajeLes, "%", sep="") ,cex=1, pos = 3)
text(barp, 1, paste("fatalidades: ", porcentajeFat, "%", sep="") ,cex=1)

# Vemos que la proporci√≥n de fatalidades en los veh√?culos es muy similar. No obstante,
# La cantidad de lesionados en las motos es mucho mayor.
  
  # TOTAL de lesionados en una moto: 9971
  # TOTAL de fallecidos en una moto: 1889
  # TOTAL: 11860



fall_les_modelo <- table(fallecidosLesionados[fallecidosLesionados$tipo_veh == "4"
                            & fallecidosLesionados$modelo_veh != "9999"
                            & fallecidosLesionados$modelo_veh != "2019"
                           ,c("fall_les","modelo_veh")])

vectorTotales <- c(fall_les_modelo[1,37],fall_les_modelo[1,36],fall_les_modelo[1,35],
  fall_les_modelo[1,34],fall_les_modelo[1,33],fall_les_modelo[1,32],
  fall_les_modelo[1,31])

vectorTotales2 <- c(fall_les_modelo[2,37],fall_les_modelo[2,36],fall_les_modelo[2,35],
  fall_les_modelo[2,34],fall_les_modelo[2,33],fall_les_modelo[2,32],
  fall_les_modelo[2,31])

barplot(
  fall_les_modelo[,25:ncol(fall_les_modelo)], col = c("royalblue", "grey"),
  main = "Proporci√≥n de fallecidos y lesionados totales acorde a alg√∫n grupo modelo de moto",
  xlab = "Modelo del veh√?culo", ylab = "Proporci√≥n",
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)

ptable <- prop.table(fall_les_modelo[,25:ncol(fall_les_modelo)], margin=2)

barp <- barplot(
  fall_les_modelo[,31:ncol(fall_les_modelo)], col = c("royalblue", "grey"),
  main = "Proporci√≥n de fallecidos y lesionados totales acorde a alg√∫n grupo modelo de moto",
  xlab = "Modelo del veh√?culo", ylab = "Proporci√≥n",
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)
text(barp, 1, paste("lesionados:", rev(vectorTotales2),  sep="") ,cex=1, pos = 3)
text(barp, 1, paste("fallecidos:", rev(vectorTotales),  sep="") ,cex=1)




#Proporci√≥n cant. motos - total de accidentes que involucran moto POR A√ëO

motos2011 <- count(importaciones[importaciones$Anio=="2011",], "Tipo.de.Vehiculo")
motos2011[42,]

motos2016 <- count(importaciones[importaciones$Anio=="2016",], "Tipo.de.Vehiculo")
motos2016[48,]

motos2017 <- count(importaciones[importaciones$Anio=="2017",], "Tipo.de.Vehiculo")
motos2017[47,]

motos2018 <- count(importaciones[importaciones$Anio=="2018",], "Tipo.de.Vehiculo")
motos2018[44,]

motosAccidentes <- count(VehiculosInvolucrados[VehiculosInvolucrados$tipo_veh=="4",], "aÒo_ocu")
motosAccidentes[1,]
motosAccidentes[2,]
motosAccidentes[3,]

aÒo_ocu<-c("2016","2017","2018")
accidentes<-c(motosAccidentes[1,2],motosAccidentes[2,2],motosAccidentes[3,2])
cantidad<-c(motos2016[48,2],motos2017[47,2],motos2018[44,2])
MotosFinal = data.frame(cbind(aÒo_ocu,accidentes,cantidad))


barp <- barplot(
  t(fall_les[1:5,]), col = c("royalblue", "grey"),
  main = "Proporcion de accidentes de motos en base al aÒo",
  xlab = "Tipo de veh√?culo", ylab = "Proporci√≥n",
  names.arg = c("Autom√≥vil","Camioneta Sport","Pick Up","Motocicleta", "Cami√≥n"),
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)
text(barp, 1, paste("lesiones: ", porcentajeLes, "%", sep="") ,cex=1, pos = 3)
text(barp, 1, paste("fatalidades: ", porcentajeFat, "%", sep="") ,cex=1)



barp <- barplot(
  accidentes,
  main = "Accidentes que involucran motos por aÒo",
  xlab = "AÒo de ocurrencia",
  ylab = "cantidad de accidentes",
  names.arg = c("2016","2017","2018")
)
text(barp, 1, paste("Accidentes: ", accidentes, sep="") ,cex=1, pos = 3)

barp <- barplot(
  cantidad,
  main = "Motos importadas por aÒo",
  xlab = "AÒo de importacion",
  ylab = "cantidad de motos",
  names.arg = c("2016","2017","2018")
)
text(barp, 1, paste("Motos: ", cantidad, sep="") ,cex=1, pos = 3)


PorcentajeAccidentes <- c(((4336*1000)/133195),((3111*1000)/124329),((3664*1000)/163821))
MotosFinal = data.frame(cbind(aÒo_ocu,accidentes,cantidad,PorcentajeAccidentes))



