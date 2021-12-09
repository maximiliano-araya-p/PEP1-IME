library(tidyr)
library(RVAideMemoire)

# Integrantes
# Gonzalo Cuevas Matamala   - Rut: 19.721.859-2
# Nicolás Henríquez Turner  - Rut: 20.730.845-5
# Maximiliano Araya Poblete - Rut: 20.467.583-k
# Miguel Salinas González   - Rut: 20.215.515-4

##############################################################################                                                                            
#  ____    ____    _____    ____   _   _   _   _   _____      _        _     #
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      / |    #
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \     | |    #
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \    | |    #
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\   |_|    #                                                                    
##############################################################################   
# Se realizó un ensayo clínico para estudiar la toxina botulínica, una toxina muy potente que puede 
# ser usado como medicamento en dosis diminutas, como posible tratamiento para el dolor de espalda 
# crónico. Un total de 31 pacientes participaron del estudio, de los cuales 15 fueron asignados 
# aleatoriamente al grupo de tratamiento y los otros 16 al grupo de control (placebo). Luego de ocho
# semanas, 9 personas voluntarias del grupo de tratamiento reportaron alivio del dolor, mientras que 
# 2 personas lo hicieron en el grupo de control. ¿Qué se podría decir del tratamiento?

# Tabla de contingencia
#               alivio dolor  Total
#tratamiento       9     6     15
#control           2     14    16
#Total             11    20    31

# Tabla de valores esperados

#               alivio  dolor   Total
#tratamiento     5.32    9.67    15
#control         5.67   10.32    16
#Total            11      20     31

# Para este caso, se han definido las siguientes hip�tesis:

# Hipótesis Nula (H0)
# H0: Las personas del grupo de tratamiento y placebo reportaron los mismos resultados para el 
#     tratamiento de dolor de espalda crónico (ambas poblaciones muestran las mismas proporciones).

# Hipótesis alternativa (HA)
# HA: Las personas del grupo de tratamiento y placebo reportaron distintos resultados para el 
#     tratamiento de dolor de espalda crónico.

# Datos
n <- 31

# Datos para crear tabla de contingencia

# Tabla de contingencia   
tratamiento <- c(9,6)
control <-c(2,14)

# Se verifica que cada observación esperada sea mayor a 5 para poder realizar la prueba de 
# homogeneidad
x1 = round(((sum(tratamiento) * (tratamiento[1] + control[1])) / n), 3) 
y1 = round(((sum(tratamiento) * (tratamiento[2] + control[2])) / n), 3)
x2 = round(((sum(control) * (tratamiento[1] + control[1])) / n), 3)
y2 = round(((sum(control) * (tratamiento[2] + control[2])) / n), 3)

# Datos para crear tabla de frecuencias esperadas
tratamiento_Esperado <- c(x1, y1)
control_Esperado <- c(x2, y2)

tabla_frec_esperadas <- as.table(rbind(tratamiento_Esperado, control_Esperado))

dimnames(tabla_frec_esperadas) <- list(grupo = c("tratamiento", "control"), 
                                       respuesta = c("alivio", "dolor"))

print(tabla_frec_esperadas)

# Como se verifica que en cada caso las observaciones esperadas son mayores a 5, se prosigue a 
# calcular la prueba de homogeneidad
tabla <- as.table(rbind(tratamiento, control))

dimnames(tabla) <- list(grupo = c("tratamiento ", "control"), 
                        respuesta = c("alivio", "dolor"))

print(tabla)

# Se realiza prueba chi-cuadrado de homogeneidad
prueba <- chisq.test(tabla)
print(prueba)

# Se optó por la prueba de homogeneidad, ya que nos permite determinar si dos poblaciones presentan
# las mismas proporciones. 

# Se puede suponer razonablemente que la muestra representa menos del 10% de la población y como se 
# menciona en el enunciado estas fueron escogidas aleatoriamente, por lo que se verifica que las 
# observaciones son independientes entre si. Como se mencionó anteriormente también se cumple la 
# condición de al menos 5 observaciones por cada grupo.

# Conclusión:
# Al realizar la prueba de homogeneidad se obtiene un valor de p = 0.017, suponiendo un nivel de 
# significación de α = 0,05, se puede observar que p < α, por lo que se rechaza H0 en favor de HA. Es
# decir, las personas del grupo de tratamiento y del grupo de control reportaron diferentes resultados
# para el tratamiento de dolor de espalda crónico.




##############################################################################                                                                            
#  ____    ____    _____    ____   _   _   _   _   _____      _        ____  #
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      |___ \ #
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \       __) |#
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \     / __/ #
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\   |_____|#
##############################################################################   
# La escuela de psicología quiere evaluar el impacto de una intervención que han diseñado para ayudar
# a dejar de fumar. Con este fin, reclutaron 25 personas fumadoras que tenían la intención de dejar 
# de hacerlo y 25 personas fumadoras que no consideraban esta opción. Dos semanas después de 
# finalizada la intervención (que consistía en mirar videos emotivos que mostraban el impacto que las 
# muertes por cáncer asociado al cigarrillo tenía en las familias, seguidas de rondas de conversación),
# se les preguntó a las y los participantes si tenían intención de intentar dejar de fumar. 5 personas 
# que tenían la intención de hacerlo antes de la intervención, cambiaron de opinión y ya no quieren 
# intentarlo, mientras que 9 participantes que no pensaban en dejar de fumar, ahora lo estaban 
# considerando. ¿Qué se puede decir del impacto de la intervención?

# Tabla de contingencia

#                                       Antes
#                             fumadores    no fumadores    total
# Después      fumadores         16             9           25  
#              no fumadores      5             20           25  
#              total             21            29           50

# Para este caso, se han definido las siguientes hipótesis:
# Grupos de estudio: Hace referencia a las respuestas tanto de las personas fumadoras como no fumadoras

# Hipótesis Nula (H0)
# H0: La intervención no produce un impacto significativo en los grupos de estudio.

# Hipótesis alternativa (HA)
# HA: La intervención produce un impacto significativo en los grupos de estudio.

# Se construye la tabla de contingencia
antes <- c(rep("Fumadores", 21), rep("No fumadores", 29))   
despues <- c(rep("Fumadores", 16), rep("No fumadores", 25), rep("Fumadores", 9))
tabla <- table(despues, antes)
print (tabla)

# Se realiza la prueba de McNemar, ya que se busca probar la inexistencia de cambios en la proporción
# de sujetos que experimentan un acontemiento, en este caso la intervención llevada a cabo por la escuela
# de psicología.

prueba <- mcnemar.test(tabla)
print(prueba)

# Conclusión:
# Al obtener los resultados de la prueba, con un nivel de significanción de α = 0.05
# se obtiene un valor de p igual a 0.4227, por mucho superior al α, lo que nos
# permite fallar al rechazar la hipótesis nula, por lo que la intervención no produce un impacto
# significativo en los grupos de estudio.



##############################################################################                                                                           
#  ____    ____    _____    ____   _   _   _   _   _____      _        _____ #
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      |___ / #
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \       |_ \ #
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \     ___) |#
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\   |____/ #
##############################################################################  
# Un grupo de activistas ha denunciado racismo en la conformación de los jurados de un pequeño condado
# en Texas, EE.UU. Su denuncia se basa que, según ellos, las proporciones raciales de las personas
# seleccionadas para ser jurado el año pasado (205 blancos, 26 negros, 25 latinos y 19 de otras razas) 
# no se corresponde con las proporciones reportadas en el último censo (72% adultos blancos, 7% adultos
# negros, 12% adultos latinos y 9% adultos que se declaran de otras raza). ¿Tienen razón los 
# denunciantes?

# Datos obtenidos para la población:

# Si bien no se entregan valores concretos de la población, sino solo sus proporciones, investigando se da 
# con el dato de que el condado mas pequeño de Texas tiene una población de 1002 
# habitantes (https://es.wikipedia.org/wiki/Chico_(Texas)) y el más grande de 
# 1327407 habitantes (https://es.wikipedia.org/wiki/San_Antonio_(Texas)), la prueba se puede realizar
# con cualquiera de estas poblaciones y su conclusión es la misma.


#                           Raza:  Blanco   Negro   Latino   Otros
# Las proporciones esperadas son:  0.72     0.07    0.12     0.09
# Valores esperados de la muestra: 198.00   19.25   33.00    24.75
# Para cada grupo se esperan más de 5 observaciones, por lo que se verifica la segunda condición.

#Las hipótesis a contrastar son: 

# Hipótesis Nula (H0)
#H0: Las proporciones de las razas son las mismas para la población y la muestra.

# Hipótesis alternativa (HA)
#HA: Las proporciones de las razas son diferentes en la población que en la muestra.

# Se crea la tabla de contingencia 
poblacion <-c(720000, 70000, 120000, 90000) # Poblacion del condado segun las proporciones entregadas
muestra <-c(205, 26, 25, 19) # cantidad de jurados de cada tipo.

tabla <-as.table(rbind(poblacion, muestra)) 

dimnames(tabla) <-list(grupo = c("Nómina", "Muestra"), 
                       tipo = c("Blancos", "Negros", "Latinos", "Otros")) 

# Se verifica si existen mas de 5 observaciones por cada grupo.     
n_poblacion <-sum(poblacion) 
n_muestra <-275
proporciones <-round(poblacion/n_poblacion, 3)
esperados <-round(proporciones * n_muestra, 3) 
cat("Valores esperados: ")
print(esperados) 

# Se realiza prueba Chi-cuadrado de bondad de ajuste.
print(tabla)
prueba <-chisq.test(tabla, correct = FALSE) 
print(prueba)

# Conclusión:
# La verificacion de más de 5 observaciones por cada grupo se cumple para ambas y los p-valor obtenidos son:

#    Para 1000 habitantes-> c(720, 70, 120, 90) 
#    p-valor =  0.2013 y α = 0.05
#    p > α

#    Para 1000000 habitantes-> c(720000, 70000, 120000, 90000) 
#    p-valor = 0.1172 y α = 0.05
#    p > α 

# A simple vista parece que si aumenta la población disminuye el p-valor, la realidad es que este tiende
# a 0.1171 por lo que no importa que tanto aumente la población, se seguira manteniendo la condición de 
# p-valor > alfa

# Como se mencionó el valor p resultante es p = 0.1172, el cual es mayor al alfa establecido (α = 0.05),
# por lo que se falla al rechazar la hipótesis nula y se concluye con un 95% de confianza que las proporciones
# de las razas son las mismas para la población y la muestra, es decir, la denuncia esta errada.



##############################################################################
#  ____    ____    _____    ____   _   _   _   _   _____      _        _  _   
# |  _ \  |  _ \  | ____|  / ___| | | | | | \ | | |_   _|    / \      | || |  
# | |_) | | |_) | |  _|   | |  _  | | | | |  \| |   | |     / _ \     | || |_ 
# |  __/  |  _ <  | |___  | |_| | | |_| | | |\  |   | |    / ___ \    |__   _|
# |_|     |_| \_\ |_____|  \____|  \___/  |_| \_|   |_|   /_/   \_\      |_|                                                                                                        
##############################################################################
# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las 
# preferencias de los chilenos de las diferentes candidaturas después de los tres debates 
# presidenciales de octubre 2021 que requiera utilizar una prueba Q de Cochran. Identifique las 
# variables involucradas y las hipótesis a contrastar.


# El experimento requiere de encuestar a un gran número de personas luego de los debates
# (teniendo datos anteriores de la cantidad de gente que apoya a los candidatos),
# consultando que candidato apoyan tomando en cuenta el debate ocurrido, se busca en primer lugar
# cuantificar las nuevas cifras de personas que apoyan a los candidatos y registrar si dicha cantidad
# disminuyo o por el contrario si se mantuvo o subió.
# Se espera luego aplicar la prueba Q de Cochran para verificar si las proporciones de éxito o fallo
# de los candidatos es la misma para o todos o si por el contrario hay un candidato que resalte.



# variable independiente: Los debates
# variable de respuestas: Dicotomia, 0 si disminuyo el numero de personas que apoyan a x candidato (fallo),
#                         1 si se mantuvo o aumentó (exito)

# Instacias       
# (debates)   El mismisimo y basadisimo artes    boric   yasna   kkast   Cheezel  MEO  parisi  
#   1                       1                      0       1       1       0       1     0
#   2                       1                      1       0       0       0       0     0
#   3                       1                      1       0       0       0       0     0

#Las hipótesis a contrastar son: 

# Hipótesis Nula (H0)
#H0: La proporción de “éxitos” es la misma para todos los candidatos.

# Hipótesis alternativa (HA)
#HA: La proporción de “éxitos” es distinta para al menos un candidato.

# Se crea la matriz de datos
instancia <- c(1,2,3)
artes <- c(1,1,1)
boric <- c(0,1,1)
yasna <- c(1,0,0)
kast <-c(1,0,0)
sichel <- c(0,0,0)
MEO <-c(1,0,0)
parisi <- c(0,0,0)
datos <- data.frame(instancia,artes,boric,yasna,kast,sichel,MEO,parisi)

# Se pasa la matriz de datos a formato largo
datos <- datos %>% pivot_longer(c("artes", "boric", "yasna", "kast", "sichel", "MEO", "parisi"),   
                                names_to = "candidatos",
                                values_to = "resultado")

datos[["instancia"]] <- factor(datos[["instancia"]])
datos[["candidatos"]] <- factor(datos[["candidatos"]])

# Se realiza prueba Q de Cochran 
prueba <- cochran.qtest(resultado ~ candidatos | instancia, data = datos , alpha = 0.05)

print(prueba)

#Conclusión:
# Con un nivel de significación de α = 0.05 , al realizar la prueba de Q de Cochran se
# obtiene un valor de p = 0.1736, superior a nuestro α , por lo que se falla al rechazar
# la hipótesis nula, por ende, la proporción de "éxitos" es distinta para cada candidato, lo
# que no indica que con esta información, no se identifican preferencias por algún candidato.





