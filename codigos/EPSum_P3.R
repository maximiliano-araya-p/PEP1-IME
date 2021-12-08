library ( ggpubr )
library ( ggplot2 )

#Pregunta 3
#Como no se conoce la verdadera dureza media, genere un gráfico del poder estadístico con las
#condiciones anteriores, pero suponiendo que las verdaderas durezas medias podrían variar de 162 a 178[kgf mm-2].

# Valores conocidos
sd <- 10
n <- 25
mediaTeorica <- 170
alfa <- 0.05
mediaInferior <- 162
mediaSuperior <- 178


deltaInf <- mediaInferior- mediaTeorica
deltaSup <- mediaSuperior- mediaTeorica

intervaloDelta <- seq(deltaInf,deltaSup,0.01)
intervaloDurezas <- seq(mediaInferior,mediaSuperior,0.01)


# Se calcula el poder con que se detecta para cada dureza media 
poder <- power.t.test (n ,intervaloDelta ,sd ,alfa,NULL,"one.sample" ,"two.sided")$power

# Crear un data frame .
datos <- data.frame(intervaloDurezas, poder )

# Graficar la curva de poder .
g <- ggplot( datos , aes (intervaloDurezas , poder ) )
g <- g + geom_line ( colour = "blue")
g <- g + ylab (" Poder estadístico ")
g <- g + xlab (" Dureza media ")
g <- g + theme_pubr ()
g <- g + geom_vline( xintercept = 170 , linetype = "dashed")
g <- g + scale_x_continuous ( breaks = seq ( mediaInferior , mediaSuperior , 2) )
g <- g + ggtitle (" Relación entre el poder y la dureza media ")
print ( g )


#Conclusión
#En el gráfico se observa que, a medida que la dureza media se aleja de la media teórica (170), el poder de la 
#prueba comienza a aumentar, disminuyendo así la probabilidad de cometer errores del tipo 2.
