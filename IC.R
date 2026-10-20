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

p11 <- ggplot(dados, aes(x=Local,y=pupas,))+
  geom_bar(stat = "identity")

p12 <- ggplot(dados, aes(x=Local,y=celulas,))+
  geom_bar(stat = "identity")

p13 <- ggplot(dados, aes(x=Local,y=larvas,))+
  geom_bar(stat = "identity")

grid.arrange(p11,p12,p13,nrow=2)

grid.arrange(p8,p9,p10,nrow=2)

grid.arrange(p5,p6,p7,nrow=2)









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

#Dist.Intertegula por ambiente
glmmTMB(data=dados,Dist.Intertegula~Ambiente+(1|Local/Coleta)) -> Mix1
summary(Mix1)
Mix1->m1
plot(simulateResiduals(m1))
table(dados$Local)


#Dist.Intertegula por local de coleta
glmmTMB(data=dados,Dist.Intertegula~Local+(1|Ambiente/Colonia)) ->m2
summary(m2)
  plot(simulateResiduals(m2))

#__________________________________________
#Agrupar dados
#___________________________________________
dados_resumidos=dados%>%
  group_by(Colonia,Ambiente)%>%
  summarise(n=n(),
            media=mean(Dist.Intertegula),
            
            Desvio_padrao=sd(Dist.Intertegula,na.rm = T),
            Erro_padrao=Desvio_padrao/sqrt(n))
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


 