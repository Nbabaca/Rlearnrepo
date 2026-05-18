#_________________________________________________________
#Pacotes
#_________________________________________________________
dados <- read.csv("dados amanda/Notas All tomorrows.csv")
attach(dados)
library(tidyverse)
library(viridis)
library(forcats)
library(dplyr)
head(dados)
summary(Nota[1:8])
#_________________________________________________________
#Boxplot
#_________________________________________________________
ggplot(dados, aes(x = Livro, y = Nota))+
  geom_boxplot(alpha=0.6,color= "orange", fill = "orange", outlier.color = "blue",outlier.fill="blue", notch = T,notchwidth = 0.8)+
  geom_jitter(color="blue",size=2.0,alpha=0.5)+
  ggtitle("boxplot do livro All Tomorrows")+
  theme_minimal()
  
#_________________________________________________________
#Grafico de Barras
#_________________________________________________________
dados[c(1:8,10),] %>%
  mutate(Leitores = fct_reorder(Leitores,Nota)) %>%
ggplot( aes(x=Leitores,y=Nota,color = "orange",fill="orange",alpha=0.6))+
  geom_bar(stat = "identity")+
  theme_minimal()+
 ggtitle("Grafico de barras das notas")
