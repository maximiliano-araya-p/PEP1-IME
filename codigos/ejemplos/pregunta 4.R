n=25 #tamaño de la muestra
diferencia=12 #tamaño del efecto
desviacion=10 #desviacion estandar
alfa=0.05 #nivel significacion
poder=0.8 #nivel de poder que se desea conseguir
cat("Cálculo del tamaño de la muestra con power.t.test()\n")
resultado<-power.t.test(n=NULL,
                        delta=diferencia,
                        sd=desviacion,
                        sig.level=alfa,
                        power = poder, type = "paired", alternative = "two.sided")
n <-ceiling(resultado[["n"]])
cat("n = ", n, "\n")




