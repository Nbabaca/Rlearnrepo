#08/05/26
#Aprendendo a usar pacotes do R
# GFP
#___________________________________________

#___________________________________________
#Colocando os pacotes no R
#___________________________________________
install.packages("tidyverse")
install.packages("car")
install.packages("viridis")
library(dplyr)
library(tidyverse)
library(carData)
library(viridis)
#___________________________________________
#ggplot2 testes (o basico)
#___________________________________________
data <- read.csv("dados amanda/modelo.csv")
head(data)
attach(data)
ggplot(data, aes(x = ambiente, y = Explored.areas))+
  geom_boxplot(
    
    # custom boxes
    color="orange",
    fill="orange",
    alpha=0.2,
    
    # Notch?
    notch=F,
    notchwidth=0.8,
    
    # custom outliers
    outilier.colour="blue",
    outlier.fill = "blue",
    outlier.size=4) 
#____________________________________________________
#Como mudar fazer separação de cor de outra variavel
#____________________________________________________
ggplot(data,aes(x = coleta, y = exploration_rate, fill = tratamento))+
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none") 

#____________________________________________________
#como mudar a paleta
#____________________________________________________
ggplot(data,aes(x = coleta, y = exploration_rate, fill = tratamento))+
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none") +
  scale_fill_brewer(palette = "BuPu")

#____________________________________________________
#Como separar por subgrupo
#____________________________________________________
p1 <- ggplot(data, aes(x=coleta, y= Explored.areas, fill = tratamento))+
  geom_boxplot(alpha=0.4)+
  facet_wrap(~coleta)+
  scale_fill_brewer(palette = "BuPu")
#separação por coleta
p2 <- ggplot(data, aes(x=coleta, y=Explored.areas, fill = tratamento))+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~tratamento)
#separação por tratamento

#___________________________________________________
#Boxplot com pontos de dados individuais
#___________________________________________________
ggplot(data, aes(x=coleta, y=Explored.areas, fill = tratamento))+
  geom_boxplot()+
  scale_fill_viridis(discrete = T,alpha = 0.6)
  geom_jitter(color="black",size=0.8, alpha=0.9)+ #adiciona pontos
  theme_ipsum()+
    theme(legend.position = "none",plot.title = element_text(size=12))+
  ggtitle("Um boxplot com pontos")+ #Como colocar titulo
  xlab("coleta")

#__________________________________________________
#Reorganizando variaveis com o ggplot
#__________________________________________________
data %>%
  mutate(colonia =fct_reorder(colonia,Explored.areas, .fun = mean)) %>%
  ggplot(aes(x=reorder(colonia,Explored.areas),y=Explored.areas,fill = colonia))+
  geom_jitter(color="black",size=0.8,alpha=0.9)+         
  geom_boxplot()+
           xlab("class")+
           theme(legend.position = "none",plot.title = element_text(size=12))+
           ggtitle("Um boxplot organizado por media")+
           xlab("")




