#BIO DE CAMPO GRUPO 1
#RBNLY
#TESTE DE GLM
#PL1 POLINIZADORES
#_______________________________________
pak::pak("r-lib/devtools")
install.packages("remotes")
install.packages("pak")
install.packages("visdat")
install.packages("lattice")
install.packages("RVAideMemoire")
install.packages("DHARMa")
install.packages("performance")
install.packages("MuMIn")
install.packages("piecewiseSEM")
install.packages("MASS")
install.packages("ggExtra")
install.packages("Rmisc")
install.packages("emmeans")
install.packages("sjPlot")
install.packages("bbmle")
install.packages("glmmTMB")
install.packages("ordinal")
install.packages("ecolottery")
install.packages("naniar")
install.packages("vcd")
install.packages("generalhoslem")
devtools::install_github("paternogbc/ecodados")
FORinstall.packages("devtools")
#__________________________________
#Pacotes
#__________________________________
library(devtools)
library(ecodados)
library(visdat)
library(tidyverse)
library(lattice)
library(RVAideMemoire)
library(DHARMa)
library(performance)
library(MuMIn)
library(piecewiseSEM)
library(MASS)
library(ggExtra)
library(Rmisc)
library(emmeans)
library(sjPlot)
library(bbmle)
library(glmmTMB)
library(ordinal)
library(car)
library(ecolottery)
library(naniar)
library(vcd)
library(generalhoslem)
library(remotes)
library(pak)
library(viridis)
#_______________________________
#Analise de Poisson
#_______________________________
attach(fragmentos)
fragmentos<- ecodados::fragmentos
## Explorar os dados
glimpse(fragmentos)
## Grafico
ggplot(fragmentos, aes(dfrag,Riqueza_obs,size = 4))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x= "Distancia para o fragmento mais proximo", y = "riqueza observada")+
  tema_livro()
## Modelo
mod_pois <- glm(Riqueza_obs ~ dfrag, family = poisson(link = "log"),data = fragmentos)
## Diagnose basica
par(mfrow = c(2,2))
plot(mod_pois)

## Diagnose avançada
SimulationOutput <- simulateResiduals(fittedModel = mod_pois, plot = TRUE)

## Overdispersion
par(mfrow = c(1,1))
testDispersion(mod_pois)

## Testar a presença de overdispersion
check_overdispersion(mod_pois)

## Dispersion parameter
deviance(mod_pois) / df.residu (mod_pois)

## Ajuste do modelo
mod_nb <- glm.nb(Riqueza_obs ~ dfrag,data= fragmentos)
par(mfrow = c(2,2))
#diagnose
plot(mod_nb)
par(mfrow = c(1,1))   
(chat <- deviance(mod_nb)/ df.residual(mod_nb)) #Dispersion parameter   

## Diagnose avançada
SimulationOutput <- simulateResiduals(fittedModel = mod_nb, plot = TRUE)
summary(mod_nb)
## Dados de contagem: a distribuição binomial
uv_cells <- ecodados::uv_cells

goodfit(`VISITAS APIS`)
Ord_plot(LUXIMETRO)

#Grafico de luminosidade por tratamento
dados_finais<- PL1_VISITAÇÃO_def
head(dados_finais)

ggplot(dados_finais,aes(x = LUX_SCALE, y = `VISITAS APIS`,fill = TRATAMENTO,alpha = TRATAMENTO))+
         geom_point(size=4,color="darkred")+
  scale_fill_viridis(discrete = T)+
  geom_smooth(method = glmmPQL(family = poisson(

dados_finais$LUX_SCALE= scale(dados_finais$LUXIMETRO)#como criar uma nova coluna  

#grafico tratamento por densidade boxplot
ggplot(dados_finais,aes( x = TRATAMENTO, y = DENSIDADE_po))+
         geom_boxplot(color="black",fill = "yellow")+
  ggtitle("Boxplot de Densidade de Aspilia por Tratamento na Parcela")
       