#adapatacion del script 7.2 y 7.5 del capitulo 7
#Fijarvaloresconocidos
n1<-48 #n del intervalo 10-44
n2<-42 #n del intervalo 45 o mas
exito1<-26
exito2<-20
alfa<-0.05
valor_nulo<-0
#Calcularprobabilidadesdeéxito.
p1<-exito1/n1
p2<-exito2/n2
#Estimarladiferencia.
diferencia<-p1-p2
#Construccióndelintervalodeconfianza.
error1<-(p1*(1-p1))/n1
error2<-(p2*(1-p2))/n2
error_est<-sqrt(error1+error2)
Z_critico<-qnorm(alfa/2,lower.tail=FALSE)
inferior<-diferencia-Z_critico*error_est
superior<-diferencia+Z_critico*error_est
cat("Intervalodeconfianza=[",inferior,",",superior,"]\n",sep="")
#Pruebadehipótesis.5
p_agrupada <-(exito1 + exito2) / (n1 + n2) 
error1 <-(p_agrupada * (1 -p_agrupada)) / n1 
error2 <-(p_agrupada * (1 -p_agrupada)) / n2 
error_est_hip <-sqrt(error1 + error2) 
cat(error_est_hip)
Z <-(diferencia -valor_nulo) / error_est_hip 
p <-2 * pnorm(Z, lower.tail = FALSE) 
cat("Hipótesis alternativa bilateral\n") 
cat("Z =", Z, "\n") 
cat("p =", p)