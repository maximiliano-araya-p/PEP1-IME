# Nombres:
# Estefan�a Alvarez 20.371.287-1
# Stephan Silva 20.298.778-8
# Francisco Moreno 19.892.183-1
# Maximiliano Araya 20.467.583-k
# Daniel Jara 20.113.716-0

# se importan librer�as
require(ez)
library(ggpubr)
library(tidyr)




#   ____                                          _               _ 
#  |  _ \   _ __    ___    __ _   _   _   _ __   | |_    __ _    / |
#  | |_) | | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |   | |
#  |  __/  | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |   | |
#  |_|     |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_|
#                         |___/                                     



# En trabajo de t�tulo de un estudiante del DIINF, se reportan los siguientes tiempos de ejecuci�n 
# ('Tpo' en milisegundos) medidos para dos versiones de un algoritmo gen�tico (A6 y B12) para resolver instancias
# del problema del vendedor viajero disponibles en repositorios p�blicos. 
# �Es uno de los algoritmos m�s r�pido que el otro?


texto <- ("
Instancia  'Tpo A6'  'Tpo B12'
'pcb1173'  1782610   811502
'pr2392'   832610    638915
'u574'     6426737   8327055
'u2152'    318828    373552
'fl1577'   129449    154668
'u1060'    321078    409211
'rat783'   2919857   3090752
'rat575'   41184     50061
'rl1889'   31682     30822
'u1817'    3072845   3032611
'vm1084'   392988    516054
'fl1400'   61822     73014
'rl1304'   106710    117283
'nrw1379'  52275     60818
'u1432'    378720    448967
'u724'     325205    219691
'd657'     3280517   411232
'd1291'    231495    126021
'pr1002'   288452    222925
'rl1323'   255516    318788
")
datos <- read.table(textConnection(texto), header = TRUE)


# datos en formato ancho

data_ancho <- read.table(
  file = textConnection(texto),
  header = TRUE,
  stringsAsFactors = FALSE
)
names(data_ancho) <- c("Instancia", "A6", "B12")



# datos en formato largo

data_largo <- gather(
  data = data_ancho,
  key = "Algoritmo",
  value = "Tiempo",
  -"Instancia"
)

data_largo[["Instancia"]] <-factor(data_largo[["Instancia"]])
data_largo[["Algoritmo"]] <-factor(data_largo[["Algoritmo"]])



# se realiza un histograma para ver mejor los datos
g <- gghistogram(
  data_largo, 
  x = "Tiempo",
  xlab = "Algoritmo",
  color = "Algoritmo", 
  fill = "Algoritmo",
  bins = 5
)
g <- g + facet_grid(~Algoritmo)
print(g)



# A partir del histograma realizado anteriormente, se puede ver que ambas muestras presentan una distribuci�n del tipo
# exponencial. Hay que tener en cuenta que las observaciones corresponden a 2 tipos de algoritmos,
# pudiendo realizarse una prueba T Student, pero las muestras NO presentan distribuciones normales, por eso
# se requiere una prueba no param�trica, utiliz�ndose entonces la prueba de rangos con signo de Wilcoxon.


# Para esto debemos verificar si se cumplen las condiciones:
# 1.- Las observaciones, al provenir de un trabajo de t�tulo de un estudiante del DIINF, 
# se puede asumir entonces que son independientes entre ellas
# 2.- La escala de medici�n utilizada para las mediciones es instr�nsecamente cont�nua
# 3.- La escala de medici�n es del tipo ordinal, pues se est� tratando de averiguar si un algoritmo es
# "mejor que el otro" o no.


# Se definen las hip�tesis:
# H0: No existen diferencias significativas en el tiempo de ejecuci�n para los algoritmos gen�ticos A6 y B12
# Ha: El algoritmo gen�tico A6 es m�s r�pido que su otra versi�n B12


# nivel de significancia
alfa = 0.05



# Una vez verificadas las condiciones, se procede a realizar la prueba de rangos con signo de
# Wilcoxon

test1 <- wilcox.test(data_ancho[["A6"]], data_ancho[["B12"]], alternative = "greater", paired = TRUE, conf.level = 1 - alfa)
test2 <- wilcox.test(Tiempo~Algoritmo, data_largo, alternative = "greater", paired = TRUE, conf.level = 1 - alfa)

print(test2)


# Wilcoxon signed rank exact test

# data:  Tiempo by Algoritmo
# V = 100, p-value = 0.5796
# alternative hypothesis: true location shift is greater than 0


# Conclusi�n: Tras realizar la prueba de rangos con signo de Wilcoxon, se obtuvo un valor p mayor que el nivel
# se significaci�n alfa (p > 0.05). Por lo tanto, se rechaza la hip�tesis alternativa (Ha) en favor de la 
# hip�tesis nula (H0). Es decir, se afirma con un 95% de confianza que, efectivamente no existen diferencias
# significativas en el tiempo de ejecuci�n para los algoritmos gen�ticos A6 y B12.




#                                                _               ____  
#  _ __    _ __    ___    __ _   _   _   _ __   | |_    __ _    |___ \ 
# | '_ \  | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |     __) |
# | |_) | | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |    / __/ 
# | .__/  |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_____|
# |_|                    |___/                                         



# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de inter�s de
# los cr�ditos en Chile, necesite utilizar una prueba de suma de rangos de Wilcoxon (tambi�n llamada
# prueba de Mann-Whitney-Wilcoxon o prueba U de Mann-Whitney), debido a problemas con la escala de
# la variable dependiente en estudio. Indique cu�les ser�n las variables/niveles involucrados en su ejemplo
# y las hip�tesis nula y alternativa a contrastar. 



# Respuesta:

# En Chile, se ha observado un alza siginificante en las tasas de inter�s de los cr�ditos que otorgan ciertos bancos,
# con lo cual varios clientes se cuestionan la posibilidad de cambiar su banco de preferencia al momento de pedir un cr�dito,
# por lo que se detienen  a analizar la situaci�n sobre 2 bancos en espec�fico, siendo Banco Estado la primera posibilidad y
# un banco del sector privado con el nombre de "Pepito Paga Doble" (nombre inventado). Para esto, se realizan los
# tr�mites necesarios para comparar las tasas de inter�s para diferentes escalas de cr�ditos y se toma en cuenta a distintas personas
# para llevarlo a cabo y concluir al respecto del cambio de banco.

# Hip�tesis nula (H0): No existe diferencia entre las tasas de inter�s de los cr�ditos otorgados por ambos bancos (Banco Estado y Pepito Paga Doble)
# Hip�tesis Alternativa (HA): Las tasas de inter�s de los cr�ditos otorgados por un banco, son menores con respecto a su competencia (Tasas de Banco Estado < Tasas de Pepito Paga Doble).

# Variables: tasas de inter�s de los cr�ditos de ambos bancos.
# Niveles involucrados: Pisos de cr�ditos que otorgan ambos bancos (cantidad de dinero que ofrecen seg�n el cr�dito).







#                                                _               _____ 
#  _ __    _ __    ___    __ _   _   _   _ __   | |_    __ _    |___ / 
# | '_ \  | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |     |_ \ 
# | |_) | | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |    ___) |
# | .__/  |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |____/ 
# |_|                    |___/                                         


# Una compa��a de cosm�ticos hizo una prueba preliminar de su nueva crema quitamanchas, en que 30
# personas fueron separadas aleatoriamente en tres grupos de 10 voluntarios/as: uno de control, a quienes
# se les entreg� una crema placebo (humectante solamente); otro que usaron la crema quitamanchas que la
# compa��a comercializa actualmente; y el �ltimo que usaron el nuevo producto. A todos se les dijo que
# usaban la crema nueva de �ltima generaci�n. Dos personas del grupo de control y una del grupo con la
# crema existente abandonaron el estudio. Para el resto, se reportaron los siguientes n�meros de manchas
# removidas al finalizar el tiempo de prueba: entre tareas y g�nero?



texto <- ("
 Nueva  Actual  Control
 35     27      15
 46     45      30
 34     28      19
 38     22      45
 29     12      16
 39     37      46
 59     27      14
 44     19      21
 41     29      --
 78     --      --
")

datos2 <- read.table(textConnection(texto), header = TRUE, na.strings = "--")

# Se formulan las hip�tesis:
# H0: no existen diferencias significativas en los tiempos entre tareas
# Ha: existe al menos 1 tarea que presente una diferencia significativa en tiempo

# Se comienza por definir un nuevo data.frame con los datos acomodados 

a <- datos2$Nueva  
b <- datos2$Actual 
c <- datos2$Control 

Cantidad <- c(a, b, c)

Tarea <- c(rep("Nueva", length(a)),
           rep("Actual" , length(b)),
           rep("Control", length(c)))

Tarea <- factor(Tarea)
Instancia2 <- factor(seq(1, 30, by = 1))
datos3 <- data.frame(Instancia2, Tarea, Cantidad)


# se define el nivel de significaci�n 
alfa = 0.05


# Comprobaci�n de normalidad a trav�s de un gr?fico QQ
g1 <- ggqqplot (datos3,
                x = "Cantidad",
                y = "Tarea",
                color = "Tarea")

g1 <- g1 + facet_wrap (~Tarea)
g1 <- g1 + rremove ( "x.ticks" ) + rremove ("x.text")
g1 <- g1 + rremove ( "y.ticks" ) + rremove ("y.text")
g1 <- g1 + rremove ("axis.title")
print (g1)




# Por el tipo de ejercicio donde nos piden identificar si existen diferencias en los tiempos entre tareas,
# es que se puede determinar que para resolver esta duda es necesario aplicar una prueba 
# ANOVA de una v�a para muestras correlacionadas.


# Es por esto que se verificar� si se cumplen las condiciones para realizar este tipo de prueba:
# 1.- Se puede apreciar mediante el gr�fico QQ que la variable dependiente presenta intervalos 
# iguales para cada muestra 
# 2.- Se puede decir que las muestras son independientes, ya que las observaciones se obtuvieron de voluntarios escogidos aleatoriamente
# 3.- A partir del gr�fico QQ se puede ver que en las muestras no existen valores at�picos, 
# siendo por esto que se puede concluir que las muestras siguen una distribuci�n normal.
# 4.- Esta no puede ser comprobada ya que en esta ocasi�n se utilizar� la funci�n aov de R



# Ahora, se realizar� la prueba ANOVA con aov
prueba1 <- aov (Cantidad~Tarea,
                data = datos3)
cat ( " \ nResultado de la prueba ANOVA para muestras correlacionadas con aov \ n " )
print (summary (prueba1))



# Procedimiento post-hoc de holm 
holm <- pairwise.t.test( Cantidad,
                         Tarea,
                         p.adj = "holm" ,
                         paired = TRUE )

cat(" Correcci?n de holm \n ")
print(holm)


# Pairwise comparisons using paired t tests 

#data:  Cantidad and Tarea 
#               Actual Control
#       Control 0.778  -      
#       Nueva   0.016  0.083  

#P value adjustment method: holm


# Conclusi�n: Como se puede ver en los resultados del procedimiento post-hoc de holm, los resultados entre
# Control-Actual y Nueva-Control  son mayores al nivel de significancia estipulado (alfa = 0.05),
# esto significando que se falla en rechazar la hip�tesis nula (H0),
# es decir, no existen pruebas de que exista al menos 1 tarea que presente una diferencia significativa en tiempo.





#                                                 _               _  _   
#   _ __    _ __    ___    __ _   _   _   _ __   | |_    __ _    | || |  
#  | '_ \  | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |   | || |_ 
#  | |_) | | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |   |__   _|
#  | .__/  |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|      |_|  
#  |_|                    |___/                                          

# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las
# lecturas dadas) en donde un estudio o experimento, relacionado con el alza 
# que han experimentado las tasas de inter�s de los cr�ditos en Chile, necesite
# utilizar una prueba de suma de Friedman, debido a problemas con la normalidad 
# de los datos. Indique cu�les ser�n las variables/niveles involucrados en su 
# ejemplo y las hip�tesis nula y alternativa a contrastar.



# Un estudio mediante una encuesta a una muestra representativa aleatoria de 
# personas de distintas comunas de Santiago que est�n ad-portas de tomar un 
# cr�dito bancario, este estudio desea establecer qu� banco (Santander, 
# Banco de Chile o Banco Estado) resulta mejor al momento de tomar un cr�dito, 
# considerando el alza en la tasa de inter�s.

# A estos les han solicitado evaluar qu� tanta seguridad tienen de tomar 
# el cr�dito con cada uno de estos bancos, esto con una escala Likert 
# de 5 puntos, donde 1 corresponde a muy poca seguridad y 5 corresponde
# a una total seguridad.

# variables involucradas: 
# Preferencia de Bancos: Santander, Banco de Chile y Banco Estado

# Hip�tesis a contrastar:
# H0: Los bancos tienen preferencias similares.
# HA: Al menos un banco obtiene una preferencia distinta a las dem�s.

# Tabla que ejemplifica las preferencias de banco
# ----------------------------------------------------------
#                   Bancos
# Personas  Santander       Banco de Chile    Banco Estado
# 1         5               1                 3
# 2         2               1                 2
# 3         2               3                 5
# 4         3               1                 4
# 5         4               5                 2
# 6         2               2                 4
# 7         1               3                 5
# 8         2               4                 3
# 10        3               2                 5
# 11        2               3                 2
# 12        4               2                 4
# 13        3               1                 3
# 14        5               2                 2
# 15        5               1                 3

#-------------------------------------------------------------------------------

































