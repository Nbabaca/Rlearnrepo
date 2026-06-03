#GFP
#Curso Quinta semana do R
#_________________________________________

#Pacotes e Dados
library(dplyr)
dados <- read.csv2("dados amanda/pinguins.csv")
attach(dados)
head(dados)
view(dados)
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
#muito util para bio de campo, gostei
attach(dados_resumidos)
#_______________________________________________________________
#Analise de Dados
#_______________________________________________________________
par(mfrow=c(2,2))

shapiro.test(media) #teste para identificar se o vetor se encontra em uma distribuição normal
shapiro.test(mediana)#tanto media quanto mediana fogem de uma distribuição normal, por isso anova não podera ser utilizada
kruskal.test(media,n) #logo é preciso utilizar um teste que não segue a normal, esse sendo o Kruskal-Wallis 
kruskal.test(mediana,n)#comparar por numero de amostras é pouco significativo

kruskal.test(media,especies)
pairwise.wilcox.test(media,mediana,p.adjust.method= "BH")
