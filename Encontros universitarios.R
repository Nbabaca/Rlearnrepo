


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


data <- read.csv("/home/lorenzo-zanette/Downloads/Bio de campo EU (respostas) - Página1(3).csv")
View(data)
head(data)
attach(data)
p25 <-ggplot(data, aes(x=modalidade, y = resposta,fill = qualidade.da.matriz))+
  geom_boxplot()+
  geom_jitter(alpha=0.8)

p26 <-ggplot(data[1:40,], aes(x=aluno, y = resposta , colour = modalidade))+
  geom_bar(stat = "identity", fill= "salmon")+
  facet_wrap(~pergunta)+
  theme_bw()+
  ggtitle("Gráfico de barras das respostas dos alunos por pergunta(Afinidade)")


p27 <-ggplot(data[41:80,], aes(x=aluno, y = resposta ,colour =  modalidade))+
  geom_bar(stat = "identity",fill = "lightblue")+
  facet_wrap(~pergunta)+
  theme_bw()+
  ggtitle("Gráfico de barras das respostas dos alunos por pergunta(Dificuldade)")

p25
  
p26  

p27

grid.arrange(p26,p27,nrow=2)


hist(resposta)

shapiro.test(resposta)

kruskal.test(resposta~modalidade)

table(data$resposta, data$pergunta)



install.packages("usethis")
usethis::create_github_token()
install.packages("gitcreds")
gitcreds::gitcreds_set()
