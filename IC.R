#14/10/26
#Trabalho IC (Corpo das vespas)
# GFP
#___________________________________________


#___________________________________________
#Colocando os pacotes no R
#___________________________________________
library(dplyr)
library(tidyverse)
library(carData)
library(viridis)
library(glmmTMB)
library(DHARMa)
library(gridExtra)
library(MuMIn)
#___________________________________________
#Dados 
#___________________________________________

dados <- read.csv("IC/medidas vespas amanda - medidas vespas amanda.csv.csv")
# Ou especifica nomes de colunas específicos:da
dados <- dados %>%
  mutate(across(c(Dist.Intertegula, Larg.Clipeo, Tam.Cabeca, Dist.Min.Olhos), as.numeric))
attach(dados)
head(dados)
par(mfrow=c(2,2))


#___________________________________________
#Densidade das medidas
#___________________________________________

p1 <- ggplot(dados, aes(x= Dist.Intertegula, fill= Ambiente))+
  geom_density(alpha=0.6)

p2 <- ggplot(dados, aes(x= Larg.Clipeo, fill= Ambiente))+
  geom_density(alpha=0.6)


p3 <-ggplot(dados, aes(x= Tam.Cabeca, fill= Ambiente))+
  geom_density(alpha=0.6)


p4 <- ggplot(dados, aes(x= Dist.Min.Olhos, fill= Ambiente))+
  geom_density(alpha=0.6)

grid.arrange(p1,p2,p3,p4,nrow=2)

#___________________________________________
#Densidade celulas, pupas e larvas
#___________________________________________

p5 <- ggplot(dados, aes(x= celulas, fill= Ambiente))+
  geom_density(alpha=0.6)

p6 <- ggplot(dados, aes(x= pupas, fill= Ambiente))+
  geom_density(alpha=0.6)


p7 <-ggplot(dados, aes(x= larvas, y= Ambiente))+
  geom_density(alpha=0.6)

p8 <- ggplot(dados, aes(x= celulas, y= Local))+
  geom_boxplot(alpha=0.6)+
  geom_jitter()

p9 <- ggplot(dados, aes(x= pupas, y= Local))+
  geom_boxplot(alpha=0.6)+
  geom_jitter()


p10 <-ggplot(dados, aes(x= larvas, y= Local))+
  geom_boxplot(alpha=0.6)+
  geom_jitter()

p11 <- ggplot(dados, aes(x=Colonia,y=pupas,))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p12 <- ggplot(dados, aes(x=Colonia,y=celulas,))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p13 <- ggplot(dados, aes(x=Colonia,y=larvas,))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

grid.arrange(p11,p12,p13,nrow=2)

grid.arrange(p8,p9,p10,nrow=2)

grid.arrange(p5,p6,p7,nrow=2)


table(dados$pupas)






#___________________________________________
#Boxplot das Medidas por Local
#___________________________________________

ggplot(dados, aes(x=Local, y=Dist.Intertegula))+
  geom_boxplot()+
  geom_jitter()

ggplot(dados, aes(x=Local, y=Larg.Clipeo))+
  geom_boxplot()

ggplot(dados, aes(x=Local, y=Tam.Cabeca))+
  geom_boxplot()

ggplot(dados, aes(x=Local, y=Dist.Min.Olhos))+
  geom_boxplot()

#___________________________________________
#Correlação e GLMM
#___________________________________________
cor(dados[,5:8])
table(dados$Ambiente)
table(dados$Local,dados$Coleta)
dados$Coleta
#Dist.Intertegula por ambiente e larvas 
glmmTMB(data=dados,Dist.Intertegula~Ambiente*larvas+Coleta+(1|Local)) -> Mix1
summary(Mix1)
plot(simulateResiduals(Mix1))
table(dados$Local)
options(na.action = na.fail)
dredge(Mix1)->dmix1
dmix1

##Dist.Intertegula por ambiente e pupas
glmmTMB(data=dados,Dist.Intertegula~Ambiente*pupas+Coleta+celulas+(1|Local))-> mix2
summary(mix2)
dredge(mix2)->dmix2
dmix2
model.avg(dmix2,subset=delta<4)->avgmix2
summary(avgmix2)

glmmTMB(data=dados[dados$Ambiente=="Urbano",],Dist.Intertegula~pupas+(1|Local/Coleta))-> mix3


head(dados)
summary(mix3)




gglot(dados, aes(y=Dist.Intertegula,x=pupas,colour=Ambiente))+
  geom_jitter()

ggplot(dados, aes(y=pupas,x=Coleta,colour=Ambiente))+
  geom_jitter()

ggplot(dados, aes(y=pupas,x=celulas,colour=Ambiente))+
  geom_jitter()

 #Dist.Intertegula por local de coleta
glmmTMB(data=dados,Dist.Intertegula~Local*pupas+Coleta+celulas) ->m2
summary(m2)
  plot(simulateResiduals(m2))
  
glmmTMB(data=dados,Dist.Intertegula~Local*+Coleta+celulas) ->m3
  summary(m3) 
  
  
  
   p20 <- ggplot(dados, aes(y=Dist.Intertegula,x=pupas,colour=Local))+
    geom_jitter()
  
  p21<- ggplot(dados, aes(y=pupas,x=Coleta,colour=Local))+
    geom_jitter()
  
  p22 <- ggplot(dados, aes(y=pupas,x=celulas,colour=Local))+
    geom_jitter()
  
  grid.arrange(p20,p21,p22,nrow=2)
  

#__________________________________________
#Agrupar dados
#___________________________________________
dados_resumidos=dados%>%
  group_by(Colonia)%>%
  summarise(n=n(),
            Pupas=mean(pupas),
            Celulas=mean(celulas),
            Larvas=mean(larvas),
            coleta=mean(Coleta))

p14 <- ggplot(dados_resumidos, aes(x=Colonia,y=Pupas,fill = coleta))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_viridis(option = "turbo")

p15 <- ggplot(dados_resumidos, aes(x=Colonia,y=Celulas,fill = coleta))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_viridis(option = "turbo")

p16 <- ggplot(dados_resumidos, aes(x=Colonia,y=Larvas,fill = coleta))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_viridis(option = "turbo")

grid.arrange(p14,p15,p16,nrow=2)


p17 <- ggplot(dados_resumidos, aes(x=coleta,y=Pupas,fill = coleta))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_viridis(option = "magma")

p18 <- ggplot(dados_resumidos, aes(x=coleta,y=Celulas,fill = coleta))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_viridis(option = "magma")

p19 <- ggplot(dados_resumidos, aes(x=coleta,y=Larvas,fill = coleta))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_viridis(option = "magma")

grid.arrange(p17,p18,p19,nrow=2)




cor(dados_resumidos[,3:6])

head(dados_resumidos)            
            
View(dados_resumidos)
attach(dados_resumidos)

#__________________________________________
#testes de normalidade
#__________________________________________
hist(Dist.Intertegula)
shapiro.test(Dist.Intertegula)

 modelo1 <- aov(Dist.Intertegula ~ Local)
summary(modelo1)
plot(modelo1)

modelo <- aov(media ~ Ambiente)
summary(modelo)


 