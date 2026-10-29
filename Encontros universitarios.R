


library(dplyr)
library(tidyverse)
library(carData)
library(viridis)
library(glmmTMB)
library(DHARMa)
library(gridExtra)
library(MuMIn)
library(readr)
library(tidyr)
library(ggplot2)


data <- read.csv("/home/lorenzo-zanette/Downloads/Bio de campo EU (respostas) - Página1(1).csv")
(data)
head(data)
attach(data)
p25 <-ggplot(data, aes(x=modalidade, y = resposta,fill = qualidade.da.matriz))+
  geom_boxplot()+
  geom_jitter(alpha=0.8)

p26 <-ggplot(data, aes(x=aluno, y = resposta, fill = modalidade))+
  geom_bar(stat = "identity")+
  facet_wrap(~pergunta)

p26  

p25

grid.arrange(p25,p25,nrow=2)


hist(resposta)

shapiro.test(resposta)



table(data$resposta, data$pergunta)
