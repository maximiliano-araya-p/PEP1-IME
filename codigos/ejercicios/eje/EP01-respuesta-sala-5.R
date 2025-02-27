#Modifcar ruta segun correponda.
datos <-read.csv("C:/Users/MAX/Desktop/eje/nuevosCasos.csv",sep = ",",stringsAsFactors = FALSE)

#Pregunta 1
#¿Qué día se produjo el mayor número de casos con sínto
#mas en la región del Maule entre el 01-mar-2021 y el 31-ago-2021?
#Aca se obtiene el valor maximo, donde 9 corresponde a Maule y los valores 365-548 a las fechas dadas.
fila<-which(datos == "Maule", arr.ind = TRUE)
maule<- fila[1,1]
fecha1<-which( colnames(datos)=="2021.03.01")# 01-mar-2021
fecha2<-which( colnames(datos)=="2021.08.31") # 31-ago-2021
#Entrega el valor maximo segun las fechas dadas.
resultado <- max(datos[maule,fecha1:fecha2])
#Se realiza la busqueda de la posicion correspondiente a columna segun el valor maximo obtenido.
columna<-which(resultado == datos[maule,fecha1:fecha2], arr.ind = TRUE)
#da el resultado: la fecha
fecha <- colnames(datos[columna])
#Pregunta 2
#¿Cuál fue el total de casos con síntomas para cada mes de este periodo?
#Marzo
rowSums(datos[maule,365:395])
#Abril
rowSums(datos[maule,396:425])
#Mayo
rowSums(datos[maule,426:456])
#Junio
rowSums(datos[maule,457:486])
#Julio
rowSums(datos[maule,487:517])
#Agosto
rowSums(datos[maule,518:548])

