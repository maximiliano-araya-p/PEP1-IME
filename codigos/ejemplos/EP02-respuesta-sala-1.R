# Instalacion de paquetes a usar
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library (ggpubr)


# Se lee el archivo de entrada
datos <- read.csv(file.choose(), encoding="UTF-8")

# Se agrupan los datos por provincia y se obtiene la cantidad de personas encuestadas
personasPorProvincia <- datos %>%  group_by(provincia) %>% mutate(cant = 1) %>% summarise(cantPersonas = sum(cant))

# Se obtienen las estad�sticas descriptivas

media <- mean(personasPorProvincia[["cantPersonas"]])
desviacion <- sd(personasPorProvincia[["cantPersonas"]])
iqr <-IQR(personasPorProvincia[["cantPersonas"]])
mediana <- median(personasPorProvincia[["cantPersonas"]])

# Se usa el gr�fico de barras para reprensentar la variable categ�rica "Provincias",
# ya que este gr�fico permite saber la proporci�n de los valores observados en cada
# provincia.
gBarras <- ggbarplot ( personasPorProvincia ,
                 x = "provincia",
                 y = "cantPersonas",
                 fill = c("green", "brown", "orange","red","yellow","blue") ,
                 title = " Personas por provincia ",
                 xlab = " Provincias ",
                 ylab = " Cantidad de personas ")
print ( gBarras )

# Se usa el gr�fico de cajas para comparar la distribuci�n y dispersi�n
# que existe en los datos observados y tambi�n permite identificar los datos at�picos
gCaja <- ggboxplot ( personasPorProvincia [["cantPersonas"]] ,
                  color = " red",
                  fill = " pink ",
                  ylab = " cantidad de Personas")
print ( gCaja )

#�Se encuestaron m�s o menos la misma cantidad de gente en cada provincia de la RM?               
#Respuesta:

#   Con el gr�fico de barras podemos notar cuantas personas fueron encuestadas por provincia,
#   donde se ve que varias provincias posiblemente encuestaron una cantidad similar de gente, exceptuando
#   la de Santiago.
#   Debido a que este valor es por mucho superior al resto, la media, que determina hacia que valor se acercan los
#   datos y la desviaci�n, que permite observar que tan dispersos estan, no logran representar 
#   realmente el conjunto de datos por lo que se utiliza la mediana, que corresponde el valor central de estos y el 
#   IQR, una medida de dispersion insensible a datos irregulares pueden hacerlo mejor.
#
#   Usando el gr�fico de caja se puede confirmar esto, donde la cantidad de personas encuestadas en Santiago 
#   corresponde a un valor at�pico de la caja, adicionalmente, los valores se encuentran cercanos a la mediana
#   y al tener un IQR peque�o nos indica que los datos no se encuentran muy dispersos, por lo que en conclusi�n 
#   se encuesto una cantidad similar de personas en  el resto de las comunas.
