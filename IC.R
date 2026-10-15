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
#___________________________________________
#Dados 
#___________________________________________

dados <- read.csv2("C:/Users/Evolab/Desktop/Gabriel Farias/medidas vespas amanda.csv")
# Ou especifica nomes de colunas específicos:da
dados <- dados %>%
  mutate(across(c(Dist.Intertegula, Larg.Clipeo, Tam.Cabeca, Dist.Min.Olhos), as.numeric))
attach(dados)
head(dados)
par(mfrow=c(2,2))


#___________________________________________
#Densidade das medidas
#___________________________________________

ggplot(dados, aes(x= Dist.Intertegula, fill= Ambiente))+
  geom_density(alpha=0.6)

ggplot(dados, aes(x= Larg.Clipeo, fill= Ambiente))+
  geom_density(alpha=0.6)


ggplot(dados, aes(x= Tam.Cabeca, fill= Ambiente))+
  geom_density(alpha=0.6)


ggplot(dados, aes(x= Dist.Min.Olhos, fill= Ambiente))+
  geom_density(alpha=0.6)


#___________________________________________
#Boxplot das Medidas
#___________________________________________

ggplot(dados, aes(x=Local, y=Dist.Intertegula))+
  geom_boxplot()

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

glmmTMB(data=dados,Dist.Intertegula~Ambiente+(1|Local/Colonia)) -> Mix1
summary(Mix1)

view(dados)
summary(dados)

table(dados$Ambiente)


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

 modelo1 <- aov(Dist.Intertegula ~ Ambiente)
summary(modelo1)

modelo <- aov(media ~ Ambiente)
summary(modelo)


 