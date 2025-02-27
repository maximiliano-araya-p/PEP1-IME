#Sala 7
# a) �Qu� d�a se produjo el mayor n�mero de casos con s�ntomas en la regi�n de Magallanes 
# entre el 01-jun-2020 y el 31-dic-2020?
# b) �Cu�l fue el total de casos con s�ntomas para cada mes de este periodo?
  
#library (dplyr)
#library (tidyr)

if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(tidyr) ) {
  install.packages("tidyr", dependencies = TRUE )
  require (tidyr)
}

# Importar desde un archivo de valores separados por coma en formato ingl�s
# Se agrega la parte del encoding
# Data frame con la informaci�n de la cantidad de casos con sintomas de cada d�a
# agrupados por las distintas regiones del pa�s.
datos <- read.csv("D:/USACH/2-2021/IME/EP-01/CasosNuevosConSintomas.csv", encoding = 'UTF-8')

# Dataframe con los datos de la region de Magallanes
region <- datos %>% filter ( Region == "Magallanes")

# Se obtiene la informaci�n relevante de dataframe de la Region de Magallanes
datosLargos <- pivot_longer(region,
                            cols = starts_with("X"),
                            names_to = "Fecha",
                            values_to = "Contagios")


# Se le ingresa el formato de la fecha al dataframe
datosLargos$Fecha <-as.Date(datosLargos$Fecha, format="X%Y.%m.%d")

# Se filtra por la fecha indicada
regionFiltrada <- datosLargos %>% filter(Fecha >= "2020-06-01" & Fecha <= "2020-12-31")

# Informaci�n del d�a con mayor cantidad de contagios de la region de Magallanes.
# Respuesta letra a) en el dataframe, con columna "Dia"
diaMaxContagios <-regionFiltrada %>% filter(Contagios == max(regionFiltrada$Contagios))
diaMaxContagios$Dia <- format(diaMaxContagios$Fecha, "%d")
diaMaxContagios

# Se obtiene la data mensual del total de  casos con sintomas en el intervalo de los meses dados.
#respuesta letra b) en el dataframe con columna "totalCasos"
dataMensual <-  regionFiltrada  %>% 
                mutate(mes = format(Fecha, "%m")) %>% 
                group_by(mes) %>% 
                summarise(totalCasos = sum(Contagios))
dataMensual