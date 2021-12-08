
if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(tidyr) ) {
  install.packages("tidyr", dependencies = TRUE )
  require (tidyr)
}

if (!require(ggplot2) ) {
  install.packages("ggplot2", dependencies = TRUE )
  require (ggplot2)
}

datos <- read.csv("C:/Users/MAX/Desktop/eje 2/EP-02 Datos Casen 2017.csv", encoding = 'UTF-8')

personasPorProvincia <- datos %>%  group_by(provincia) %>% mutate(cant = 1) %>% summarise(cantPersonas = sum(cant))


print(sd(personasPorProvincia$cantPersonas))
#library(ggplot2)
plot(x=personasPorProvincia$cantPersonas, y=personasPorProvincia$provincia)
#hist(x = personasPorProvincia$cantPersonas, main = "Histograma de encuestas por provincia", 
 #    xlab = "Cantidad de personas encuestadas", ylab = "Frecuencia")
# Dataframe con los datos de la region de Magallanes
#region <- datos %>% filter ( Region == "Magallanes")

# Se obtiene la información relevante de dataframe de la Region de Magallanes
#datosLargos <- pivot_longer(region,
 #                           cols = starts_with("X"),
  #                          names_to = "Fecha",
   #                         values_to = "Contagios")


# Se le ingresa el formato de la fecha al dataframe
#datosLargos$Fecha <-as.Date(datosLargos$Fecha, format="X%Y.%m.%d")

# Se filtra por la fecha indicada
#regionFiltrada <- datosLargos %>% filter(Fecha >= "2020-06-01" & Fecha <= "2020-12-31")

# Información del día con mayor cantidad de contagios de la region de Magallanes.
# Respuesta letra a) en el dataframe, con columna "Dia"
#diaMaxContagios <-regionFiltrada %>% filter(Contagios == max(regionFiltrada$Contagios))
#diaMaxContagios$Dia <- format(diaMaxContagios$Fecha, "%d")
#diaMaxContagios

# Se obtiene la data mensual del total de  casos con sintomas en el intervalo de los meses dados.
#respuesta letra b) en el dataframe con columna "totalCasos"
#dataMensual <-  regionFiltrada  %>% 
  #              mutate(mes = format(Fecha, "%m")) %>% 
 #               group_by(mes) %>% 
#                summarise(totalCasos = sum(Contagios))
#dataMensual