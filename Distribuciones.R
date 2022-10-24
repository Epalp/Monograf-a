##Análisis de datos de irradiación
#Importar datos
library(readxl)
datos <- read_excel("Datos.xlsx", sheet = "DatosIFecha", 
                    col_types = c("text", "text", "text", 
                                  "text", "numeric"))

#Resumen de datos
sum<-summary(datos)
meses<-split(datos, f = datos$Mes)
#Enero
enero<-meses$`1`
horasenero<-split(enero,f = enero$Hora)
#distribución estadística por hora
#install.packages("gamlss")
library(gamlss)
res<-NULL
res<-rbind(res,c("Fam","mu","sigma","nu","tau","aic"))

enero06<-horasenero$`6`
enero06<-enero06$Valor
fitenero06<-fitDist(enero06, type ="realplus")
resenero06<-c(fitenero06$family[1], fitenero06$mu,fitenero06$sigma,
              fitenero06$nu,fitenero06$tau,
              fitenero06$aic)

res<-rbind(res,resenero06)


enero07<-horasenero$`7`
enero07<-enero07$Valor
fitenero07<-fitDist(enero07, type ="realplus")
resenero07<-c(fitenero07$family[1], fitenero07$mu,fitenero07$sigma,
       fitenero07$nu,fitenero07$tau,
       fitenero07$aic)
res<-rbind(res,resenero07)


enero08<-horasenero$`8`
enero08<-enero08$Valor
fitenero08<-fitDist(enero08, type ="realplus")
resenero08<-c(fitenero08$family[1], fitenero08$mu,fitenero08$sigma,
              fitenero08$nu,fitenero08$tau,
              fitenero08$aic)
res<-rbind(res,resenero08)

enero09<-horasenero$`9`
enero09<-enero09$Valor
fitenero09<-fitDist(enero09, type ="realplus")
resenero09<-c(fitenero09$family[1], fitenero09$mu,fitenero09$sigma,
              fitenero09$nu,fitenero09$tau,
              fitenero09$aic)
res<-rbind(res,resenero09)

enero10<-horasenero$`10`
enero10<-enero10$Valor
fitenero10<-fitDist(enero10, type ="realplus")
resenero10<-c(fitenero10$family[1], fitenero10$mu,fitenero10$sigma,
              fitenero10$nu,fitenero10$tau,
              fitenero10$aic)
res<-rbind(res,resenero10)

enero11<-horasenero$`11`
enero11<-enero11$Valor
fitenero11<-fitDist(enero11, type ="realplus")
resenero11<-c(fitenero11$family[1], fitenero11$mu,fitenero11$sigma,
              fitenero11$nu,fitenero11$tau,
              fitenero11$aic)
res<-rbind(res,resenero11)

enero12<-horasenero$`12`
enero12<-enero12$Valor
fitDist(enero12, type ="realplus")
fitenero12<-fitDist(enero12, type ="realplus")
resenero12<-c(fitenero12$family[1], fitenero12$mu,fitenero12$sigma,
              fitenero12$nu,fitenero12$tau,
              fitenero12$aic)
res<-rbind(res,resenero12)

enero13<-horasenero$`13`
enero13<-enero13$Valor
fitDist(enero13, type ="realplus")
fitenero13<-fitDist(enero13, type ="realplus")
resenero13<-c(fitenero13$family[1], fitenero13$mu,fitenero13$sigma,
              fitenero13$nu,fitenero13$tau,
              fitenero13$aic)
res<-rbind(res,resenero13)

enero14<-horasenero$`14`
enero14<-enero14$Valor
fitDist(enero14, type ="realplus")
fitenero14<-fitDist(enero14, type ="realplus")
resenero14<-c(fitenero14$family[1], fitenero14$mu,fitenero14$sigma,
              fitenero14$nu,fitenero14$tau,
              fitenero14$aic)
res<-rbind(res,resenero14)

enero15<-horasenero$`15`
enero15<-enero15$Valor
fitDist(enero15, type ="realplus")
fitenero15<-fitDist(enero15, type ="realplus")
resenero15<-c(fitenero15$family[1], fitenero15$mu,fitenero15$sigma,
              fitenero15$nu,fitenero15$tau,
              fitenero15$aic)
res<-rbind(res,resenero15)

enero16<-horasenero$`16`
enero16<-enero16$Valor
fitDist(enero16, type ="realplus")
fitenero16<-fitDist(enero16, type ="realplus")
resenero16<-c(fitenero16$family[1], fitenero16$mu,fitenero16$sigma,
              fitenero16$nu,fitenero16$tau,
              fitenero16$aic)
res<-rbind(res,resenero16)

enero17<-horasenero$`17`
enero17<-enero17$Valor
fitDist(enero17, type ="realplus")
fitenero17<-fitDist(enero17, type ="realplus")
resenero17<-c(fitenero17$family[1], fitenero17$mu,fitenero17$sigma,
              fitenero17$nu,fitenero17$tau,
              fitenero17$aic)
res<-rbind(res,resenero17)



enero18<-horasenero$`18`
enero18<-enero18$Valor
fitDist(enero18, type ="realplus")
fitenero18<-fitDist(enero18, type ="realplus")
resenero18<-c(fitenero18$family[1], fitenero18$mu,fitenero18$sigma,
              fitenero18$nu,fitenero18$tau,
              fitenero18$aic)
res<-rbind(res,resenero18)

enero19<-horasenero$`19`
enero19<-enero19$Valor
fitDist(enero19, type ="realplus")
fitenero19<-fitDist(enero19, type ="realplus")
resenero19<-c(fitenero19$family[1], fitenero19$mu,fitenero19$sigma,
              fitenero19$nu,fitenero19$tau,
              fitenero19$aic)
res<-rbind(res,resenero19)
write.csv(res,file = "respuestaenero.csv")

