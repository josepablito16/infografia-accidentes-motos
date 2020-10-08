# Universidad del Valle de Guatemala
# Proyecto #1
# Integrantes: Oscar Juárez, José Cifuentes, Luis Esturban
# Fecha: 30/03/2020

setwd("./")

HechoTransito<-read.csv("./HechoTransito.csv",stringsAsFactors = FALSE)
importaciones<-read.csv("./importacionesVehiculosSAT.csv",stringsAsFactors = FALSE)
VehiculosInvolucrados<-read.csv("./VehiculosInvolucrados.csv",stringsAsFactors = FALSE)
FallecidosLesionados<-read.csv("./FallecidosLesionados.csv",stringsAsFactors = FALSE)


# d. Graficos exploratorios que le de ideas del estado de los datos.

                #Hechos de transito
View(HechoTransito)
#En que años hay mas accidentes?
barplot(table(HechoTransito[,"año_ocu"]),
        main = "Cantidad de accidentes al año",
        xlab = "Años", ylab = "Cantidad de accidentes",
        col = "royalblue")

#Que municipios tiene mas accidentes?
municipiosList<-as.data.frame(table(HechoTransito[,"mupio_ocu"]))
View(head(municipiosList[order(municipiosList$Freq,decreasing = TRUE),],n=5))


#Que tipo de vehiculo tiene mas accidentes?
#En que años hay mas accidentes?
barplot(table(HechoTransito[,"tipo_veh"]),
        main = "Cantidad de accidentes por tipo de vehiculo",
        xlab = "Tipo vehiculo", ylab = "Cantidad de accidentes",
        col = "royalblue")


#En que mes hay mas accidentes
barplot(table(HechoTransito[,"mes_ocu"]),
        main = "Cantidad de accidentes por mes",
        xlab = "Mes", ylab = "Cantidad de accidentes",
        col = "royalblue")

#En que dia de la semana hay mas accidentes
barplot(table(HechoTransito[,"día_sem_ocu"]),
        main = "Cantidad de accidentes por mes",
        xlab = "Mes", ylab = "Cantidad de accidentes",
        col = "royalblue")


                #importaciones
View(importaciones)

#De que paises hay mas importaciones
paisesImportaciones<-as.data.frame(table(importaciones[,"Pais.de.Proveniencia"]))
View(head(paisesImportaciones[order(paisesImportaciones$Freq,decreasing = TRUE),],n=5))

#En que aduana ingresan mas vehiculos
aduanas<-as.data.frame(table(importaciones[,"Aduana.de.Ingreso"]))
View(head(aduanas[order(aduanas$Freq,decreasing = TRUE),],n=5))

#Que modelos de carros son los que mas ingresan por año

                #2011
barplot(table(importaciones[importaciones$Anio=="2011","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2011",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2011","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2012
barplot(table(importaciones[importaciones$Anio=="2012","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2012",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2012","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2013
barplot(table(importaciones[importaciones$Anio=="2013","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2013",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2013","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2014
barplot(table(importaciones[importaciones$Anio=="2014","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2014",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2014","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2015
barplot(table(importaciones[importaciones$Anio=="2015","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2015",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2015","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2016
barplot(table(importaciones[importaciones$Anio=="2016","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2016",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2016","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])


                #2017
barplot(table(importaciones[importaciones$Anio=="2017","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2017",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2017","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2018
barplot(table(importaciones[importaciones$Anio=="2018","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2018",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2018","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2019
barplot(table(importaciones[importaciones$Anio=="2019","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2019",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2019","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

                #2020
barplot(table(importaciones[importaciones$Anio=="2020","Modelo.del.Vehiculo"]),
        main = "Modelos importados en el 2020",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

importacionesAnio<-as.data.frame(table(importaciones[importaciones$Anio=="2020","Modelo.del.Vehiculo"]))
View(importacionesAnio[order(importacionesAnio$Freq,decreasing = TRUE),])

#Que tipo de vehiculo son los mas importados
tipoVehiculo<-as.data.frame(table(importaciones[,"Tipo.de.Vehiculo"]))
View(tipoVehiculo[order(tipoVehiculo$Freq,decreasing = TRUE),])

#Que modelos de motos son las mas importadas por año
                #2011
barplot(table(importaciones[importaciones$Anio=="2011" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2011",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2012
barplot(table(importaciones[importaciones$Anio=="2012" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2012",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2013
barplot(table(importaciones[importaciones$Anio=="2013" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2013",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2014
barplot(table(importaciones[importaciones$Anio=="2014" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2014",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2015
barplot(table(importaciones[importaciones$Anio=="2015" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2015",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2016
barplot(table(importaciones[importaciones$Anio=="2016" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2016",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2017
barplot(table(importaciones[importaciones$Anio=="2017" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2017",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2018
barplot(table(importaciones[importaciones$Anio=="2018" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2018",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2019
barplot(table(importaciones[importaciones$Anio=="2019" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2019",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

                #2020
barplot(table(importaciones[importaciones$Anio=="2020" & importaciones$Tipo.de.Vehiculo=="MOTO","Modelo.del.Vehiculo"]),
        main = "Modelos de motos importados en el 2020",
        xlab = "Año", ylab = "Cantidad de importaciones",
        col = "royalblue")

######################################################################################

# Tabla de g_hora_5
table(HechoTransito$g_hora_5)
table(VehiculosInvolucrados$g_hora_5)
table(FallecidosLesionados$g_hora_5)

# Tabla de sexo_per
table(VehiculosInvolucrados$sexo_per)
table(FallecidosLesionados$sexo_per)

# Tabla de mayor_menor
table(VehiculosInvolucrados$mayor_menor)
table(FallecidosLesionados$mayor_menor)

# Tabla de depto_ocu
table(HechoTransito$depto_ocu)
table(VehiculosInvolucrados$depto_ocu)
table(FallecidosLesionados$depto_ocu)

# Tabla de mupio_ocu
table(HechoTransito$mupio_ocu)
table(VehiculosInvolucrados$mupio_ocu)
table(FallecidosLesionados$mupio_ocu)

# Tabla de día_sem_ocu
table(HechoTransito$día_sem_ocu)
table(VehiculosInvolucrados$día_sem_ocu)
table(FallecidosLesionados$día_sem_ocu)

# Tabla de mes_ocu
table(HechoTransito$mes_ocu)
table(VehiculosInvolucrados$mes_ocu)
table(FallecidosLesionados$mes_ocu)

# Tabla de tipo_veh
table(HechoTransito$tipo_veh)
table(VehiculosInvolucrados$tipo_veh)
table(FallecidosLesionados$tipo_veh)

# Tabla de tipo_eve
table(HechoTransito$tipo_eve)
table(VehiculosInvolucrados$tipo_eve)
table(FallecidosLesionados$tipo_eve)

# Tabla de fall_les
table(FallecidosLesionados$fall_les)

# Tabla de año_ocu
table(HechoTransito$año_ocu)
table(VehiculosInvolucrados$año_ocu)
table(FallecidosLesionados$año_ocu)

# Tabla de g_hora
table(HechoTransito$g_hora)
table(VehiculosInvolucrados$g_hora)
table(FallecidosLesionados$g_hora)

# Tabla de g_edad_80ymás
table(VehiculosInvolucrados$g_edad_80ymás)
table(FallecidosLesionados$g_edad_80ymás)


# Tabla de g_edad_60ymás
table(HechoTransito$g_edad_60ymás)
table(VehiculosInvolucrados$g_edad_60ymás)
table(FallecidosLesionados$g_edad_60ymás)


#############################################################################################


# Distribucion normal de día_ocu
qqnorm(HechoTransito$día_ocu)
qqline(HechoTransito$día_ocu)


qqnorm(VehiculosInvolucrados$día_ocu)
qqline(VehiculosInvolucrados$día_ocu)


qqnorm(FallecidosLesionados$día_ocu)
qqline(FallecidosLesionados$día_ocu)

# Distribucion normal de edad_per

qqnorm(FallecidosLesionados$edad_per)
qqline(FallecidosLesionados$edad_per)


