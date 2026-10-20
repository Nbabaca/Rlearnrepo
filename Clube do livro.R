#_________________________________________________________
#Pacotes
#_________________________________________________________
dados <- read.csv("dados amanda/Clube do livro.csv")
attach(dados)
library(tidyverse)
library(viridis)
library(forcats)
library(dplyr)
head(dados)
summary(Nota)
#_________________________________________________________
#Boxplot
#_________________________________________________________
ggplot(dados, aes(x = Livro, y = Nota))+
  geom_violin(alpha=0.6,color= "white", fill = "orange", outlier.color = "black",outlier.fill="black", notch = F,notchwidth = 0.8)+
  geom_jitter(color="black",size=2.0,alpha=0.5)+
  ggtitle("boxplot dos livros")+
  theme_bw()+
  ylim(0,5)
  
#_________________________________________________________
#Grafico de Barras
#_________________________________________________________
dados %>%
ggplot(aes(x=Leitores,y=Nota,color = "white",fill="orange"))+
  geom_bar(stat = "identity",alpha=0.8,color="white",fill="orange")+
  theme_bw()+
  facet_wrap(~Livro)+
  ylim(0,5)+
  ggtitle("Grafico de barras das notas")

