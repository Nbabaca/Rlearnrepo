#PL1 GRUPO BARBIES
#Regressão linear
#26/06/2026
#__________________________
library(dplyr)
#__________________________
dados_finais <-  PL1_VISITAÇÃO_def
head(dados_finais)
attach(dados_finais)
plot(`VISITAS APIS`,LUXIMETRO,pch=21,cl="blue",bg="green")     
abline(lm(LUXIMETRO~`VISITAS APIS`),col="red")
hist(`VISITAS APIS`)
modelo_sol <- lm((LUXIMETRO[1:9])~`VISITAS APIS`)
summary(modelo)
summary.aov(modelo)

dados_finais_medidas = dados_finais%>%
  group_by(PARCELA, TRANSECTO)%>%
  summarise(n=n(),
    media=mean(`VISITAS APIS`),
    mediana=median(`VISITAS APIS`),
    desvio_padrao=sd(`VISITAS APIS`))
    
    