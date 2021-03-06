# Universidad del Valle de Guatemala
# Laboratorio #8
# Integrantes: Oscar Ju�rez, Jos� Cifuentes, Paul Belches
# Fecha: 10/10/2020
library("ggpubr")
library(corrplot)
library(plyr)


setwd("./")

# Leer la entrada de los datos
HechoTransito<-read.csv("./HechoTransito.csv",stringsAsFactors = FALSE)
VehiculosInvolucrados<-read.csv("./VehiculosInvolucrados.csv",stringsAsFactors = FALSE)
fallecidosLesionados<- read.csv("./FallecidosLesionados.csv", stringsAsFactors = FALSE)
importaciones <- read.csv("./importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)

# Restricci�n de datos a solo motos
motosFallecidosLesionados <- fallecidosLesionados[fallecidosLesionados$tipo_veh == "4",]
motosInvolucradas <- VehiculosInvolucrados[VehiculosInvolucrados$tipo_veh == "4",]

# obtenemos un conteo de accidentes por departamento
municipiosAccidentes <- table(motosInvolucradas[,"depto_ocu"])
names(municipiosAccidentes) = c("Guatemala", "El Progreso", "Sacatep�quez", "Chimaltenango", "Escuintla", "Santa Rosa", "Solol�", "Totonicap�n", "Quetzaltenango", "Suchitep�quez", "Retalhuleu", "San Marcos", "Huehuetenango", "Quich�", "Baja Verapaz", "Alta Verapaz", "Pet�n", "Izabal", "Zacapa", "Chiquimula", "Jalapa", "Jutiapa")
deptoOcuDesc <- municipiosAccidentes[order(municipiosAccidentes, decreasing = TRUE)]
barplot(deptoOcuDesc[1:6], col = "royalblue",
        main = "Departamentos con m�s accidentes de motos (2016 - 2018)",
        xlab = "Departamentos", ylab = "Cantidad de accidentes",
        )

# Restricci�n a solo importaciones de moto por a�o
motosImportaciones <- importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
cantMotos <- t(as.matrix(count(motosImportaciones, "Anio")))
colnames(cantMotos) = c("2012","2013","2014","2015","2016","2017","2018","2019","2020")
barplot(cantMotos, col ="royalblue",
        main = "Cant. de importaciones de motos por a�o",
        xlab = "A�o", ylab = "Cantidad de importaciones")


# Fallecidos por a�o solo de motos
motosFallecidos <- motosFallecidosLesionados[motosFallecidosLesionados$fall_les == "1",]
fallecidosPorAnio <- table(motosFallecidos[,"a�o_ocu"])
barplot(fallecidosPorAnio, col ="royalblue",
        main = "Cant. de accidente letales de motos por a�o",
        xlab = "A�o", ylab = "Cantidad de accidentes")

# Accidentes de motos por a�o
accidentesPorAnio <- table(motosInvolucradas[,"a�o_ocu"])
barplot(accidentesPorAnio, col ="royalblue",
        main = "Cant. de accidente de motos por a�o",
        xlab = "A�o", ylab = "Cantidad de accidentes")



# CSV accidentes de motos por fecha
accidentesPorDia <- count(motosInvolucradas[,c("d�a_ocu","mes_ocu","a�o_ocu")])
colnames(accidentesPorDia) = c("Dia", "Mes", "Anio", "TotalAccidentes")
csvData <- accidentesPorDia[order(
              accidentesPorDia$a�o_ocu,
              accidentesPorDia$mes_ocu,
              accidentesPorDia$d�a_ocu),]

# Escribir al CSV
write.csv(csvData, file="../Data/AccidentesPorDia.csv", row.names = FALSE)




# CSV importaciones de motos por fecha
importacionesPorDia <- count(motosImportaciones[,c("Dia","Mes","Anio")])
colnames(importacionesPorDia) = c("Dia", "Mes", "Anio", "TotalImportaciones")
csvData <- importacionesPorDia[order(
            importacionesPorDia$Anio,
            importacionesPorDia$Mes,
            importacionesPorDia$Dia),]
# Escribir al CSV
write.csv(csvData, file="../Data/ImportacionesPorDia.csv", row.names = FALSE)



# Juntar datos
dataJunta <- merge(accidentesPorDia, importacionesPorDia, all = TRUE)
dataJunta[is.na(dataJunta)] <- 0
csvData <- dataJunta[order(
            dataJunta$Anio,
            dataJunta$Mes,
            dataJunta$Dia),]

# Escribir al CSV
write.csv(csvData, file="../Data/DatosPorDia.csv", row.names = FALSE)




# Tipo Cantidad de tipos de veh�culsos durante todos los a�os
cantTipoVeh <- table(importaciones[,"Tipo.de.Vehiculo"])
# Hacemos que el orden de la tabla sea descendiente
cantTipoVeh <- cantTipoVeh[order(cantTipoVeh, decreasing = TRUE)]

# Gr�fico de barras
barplot(cantTipoVeh[1:5],
        main = "Total de veh�culos importados desde el a�o 2011",
        xlab = "Tipo de veh�culo", ylab = "Cantidad de importaciones",
        col = "royalblue")

# Obtenemos total de motos y autom�viles importados desde
# el a�o 2011
totalMotos <- cantTipoVeh[1]
totalCarros <- cantTipoVeh[2] + cantTipoVeh[3] + cantTipoVeh[4]
totalCamiones <- cantTipoVeh[5]

vectorTotalVeh <- c(totalMotos, totalCarros, totalCamiones)
# El tipo de veh�culo que predomina es la moto, con un total de 1104306 unidades
# importadas, mientras que el autom�vil es el 32% de esa medida (347871 unidades)

# No obstante, es prudente sumar el auto, camioneta y pickup ya que estos medios de
# transporte los consideraremos como veh�culos


# Gr�fico de barras
barplot(vectorTotalVeh,
        main = "Total de veh�culos importados desde el a�o 2011",
        xlab = "Tipo de veh�culo", ylab = "Cantidad de importaciones",
        col = "royalblue")


# Ahora, obtenemos el total de accidentes por tipo de veh�culo
cantHechosVeh <- table(VehiculosInvolucrados[,"tipo_veh"])
# Ahora en orden descendiente
cantHechosVeh <- cantHechosVeh[order(cantHechosVeh, decreasing = TRUE)]

barp <- barplot(cantHechosVeh[1:6],
                main = "Cantidad de accidentes por tipo de vehiculo desde el a�o 2016",
                xlab = "Tipo vehiculo", ylab = "Cantidad de accidentes",
                col = "royalblue", 
                names.arg = c("Motocicleta","Autom�vil","Pick Up","Ignorado","Camioneta Sport", "Cami�n"))
text(barp, 1, paste("total: ", vectorTotalAcc, sep="") ,cex=1, pos=3) 

# Hacemos lo mismo, sumamos el auto, pick up y la camioneta sport.
totalAccMotos <- cantHechosVeh[1]
totalAccCarros <- cantHechosVeh[2] + cantHechosVeh[3] + cantHechosVeh[5]
totalAccCamiones <- cantHechosVeh[4]

vectorTotalAcc <- c(totalAccMotos, totalAccCarros, totalAccCamiones)

# Gr�fico de barras
barp <- barplot(vectorTotalAcc,
                main = "Comparaci�n de total accidentes de autom�viles y carros",
                xlab = "Tipo de veh�culo", ylab = "Cantidad de accidentes",
                col = "royalblue", names.arg = c("Motocicletas", "Autom�vil","Cami�n"))
text(barp, 1, paste("total: ", vectorTotalAcc, sep="") ,cex=1, pos=3) 

# Procedemos a hacer un data frame con la informaci�n relevante
vehAcc <- data.frame(t(vectorTotalAcc))
vehAcc <- rbind(vehAcc, "Vehiculos"=vectorTotalVeh)
row.names(vehAcc) <- c("Cantidad de accidentes", "Total de veh�culos")
colnames(vehAcc) <- c("Motocicletas", "Autom�vil","Cami�n")
barplot(
  as.matrix(vehAcc), col = c("royalblue", "grey"),
  main = "Proporci�n de cantidad de veh�culos vs. cantidad de accidentes",
  ylab = "Proporci�n", xlab = "Tipo de veh�culo")

prop.table(as.matrix(vehAcc), margin=2)





# AHORA, ANALIZAR FATALIDADES

# Creacion de tabla de frecuencias de fallecidos y lesionados
fall_les <- table(fallecidosLesionados[,c("tipo_veh", "fall_les")])


tablaTotales <- fall_les[1:5,]
row.names(tablaTotales) <- c("Autom�vil","Camioneta Sport","Pick Up","Motocicleta", "Cami�n")
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
  main = "Proporci�n de fallecidos y lesionados totales acorde a un tipo de veh�culo",
  xlab = "Tipo de veh�culo", ylab = "Proporci�n",
  names.arg = c("Autom�vil","Camioneta Sport","Pick Up","Motocicleta", "Cami�n"),
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)
text(barp, 1, paste("lesiones: ", porcentajeLes, "%", sep="") ,cex=1, pos = 3)
text(barp, 1, paste("fatalidades: ", porcentajeFat, "%", sep="") ,cex=1)

# Vemos que la proporci�n de fatalidades en los veh�culos es muy similar. No obstante,
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
  main = "Proporci�n de fallecidos y lesionados totales acorde a alg�n grupo modelo de moto",
  xlab = "Modelo del veh�culo", ylab = "Proporci�n",
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)

ptable <- prop.table(fall_les_modelo[,25:ncol(fall_les_modelo)], margin=2)

barp <- barplot(
  fall_les_modelo[,31:ncol(fall_les_modelo)], col = c("royalblue", "grey"),
  main = "Proporci�n de fallecidos y lesionados totales acorde a alg�n grupo modelo de moto",
  xlab = "Modelo del veh�culo", ylab = "Proporci�n",
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)
text(barp, 1, paste("lesionados:", rev(vectorTotales2),  sep="") ,cex=1, pos = 3)
text(barp, 1, paste("fallecidos:", rev(vectorTotales),  sep="") ,cex=1)




# TODO: Proporci�n cant. motos - total de accidentes que involucran moto POR A�O

motos2011 <- count(importaciones[importaciones$Anio=="2011",], "Tipo.de.Vehiculo")
motos2011[42,]

motos2011 <- count(importaciones[importaciones$Anio=="2011",], "Tipo.de.Vehiculo")

