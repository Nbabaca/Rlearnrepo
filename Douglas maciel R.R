#GFP
#Curso Quinta semana do R
#_________________________________________

#Pacotes e Dados
library(dplyr)
library(tidyverse)
dados <- read.csv("C:/Users/Evolab/Desktop/Gabriel Farias/medidas vespas amanda PDFFFFF.csv")
attach(dados)
head(dados)
view(dados)


dados1 <- read.csv("C:/Users/Evolab/Desktop/Gabriel Farias/medidas vespas amanda.csv")
attach(dados1)
head(dados1)
  #_______________________________________________________________
#Resumir dados por media, mediana, desvio padrão e erro padrão
#_______________________________________________________________
dados_resumidos=dados%>%
  group_by(especies,ano,ilha)%>%
  summarise(n=n(),
            media=mean(comprimento_bico,na.rm=T),
            mediana=median(comprimento_bico,na.rm = T),
            Desvio_padrao=sd(comprimento_bico,na.rm = T),
            Erro_padrao=Desvio_padrao/sqrt(n))
View(dados_resumidos)


dados_resumidos=dados1%>%
  group_by(Colonia,Ambiente)%>%
  summarise(n=n(),
            media=mean(Dist.Intertegula),
            mediana=median(Dist.Intertegula),
            Desvio_padrao=sd(Dist.Intertegula,na.rm = T),
            Erro_padrao=Desvio_padrao/sqrt(n))
View(dados_resumidos)










#muito util para bio de campo, gostei
attach(dados_resumidos)
#_______________________________________________________________
#Analise de Dados
#_______________________________________________________________
par(mfrow=c(2,2))
hist(media)
shapiro.test(media) #teste para identificar se o vetor se encontra em uma distribuição normal
shapiro.test(mediana)#tanto media quanto mediana fogem de uma distribuição normal, por isso anova não podera ser utilizada
kruskal.test(media,n) #logo é preciso utilizar um teste que não segue a normal, esse sendo o Kruskal-Wallis 
kruskal.test(mediana,n)#comparar por numero de amostras é pouco significativo

kruskal.test(media,especies)
pairwise.wilcox.test(media,mediana,p.adjust.method= "BH")

t.test(media ~ Ambiente, data= dados_resumidos)

ggplot(dados_resumidos, aes(x=Ambiente, y=media))+
geom_boxplot()

ggplot(dados_resumidos, aes(x=Ambiente, y=mediana))+
  geom_boxplot()
