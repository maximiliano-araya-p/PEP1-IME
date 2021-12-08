


#Fijar valores conocidos
n1<-200 #n del intervalo 10-44
n2<-349 #n del intervalo 45 o mas
exito1<-109
exito2<-242
alfa<-0.05
valor_nulo<-0
n3<-c(c(n1,n2))
exitos<-c(exito1,exito2)
#Prueba de Wilson.
prueba<-prop.test(exitos,n=n3,alternative="two.sided",conf.level=1-alfa)
#Proporciones de éxito.
p1<-prueba$estimate[1] 
p2<-prueba$estimate[2]
#Diferencia.
diferencia<-p1-p2
#Intervalo de confianza.
inferior<-prueba$conf.int[1]
superior<-prueba$conf.int[2]
cat("Intervalo de confianza = [",inferior,",",superior,"]\n",sep="")

#Prueba de hipótesis.
error_est_hip <- prueba$statistic #error estandar
Z <-(diferencia -valor_nulo) / error_est_hip 
p <-prueba$p.value
cat("Hipótesis alternativa bilateral\n") 
cat("p =", p)
cat("\nZ =", Z)