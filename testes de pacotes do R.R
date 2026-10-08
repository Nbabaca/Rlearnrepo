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
data <- read.csv("dados amanda/NOTAS EVOLUÇÃO 2026.1.csv")
dados <- read.csv("dados amanda/NOTAS EVOLUÇÃO 2026.1Q.csv")
head(dados)
attach(dados)
dados_arrange <- dados %>%
  dplyr::arrange(Nota)
head(dados_arrange)  

ggplot(data, aes(x = Individuo, y = Total))+
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
ggplot(data,aes(x = Matricula, y = , fill = tratamento))+
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
ggplot(data, aes(x=Individuo, y=Total))+
  geom_boxplot(color="cyan",fill="cyan",alpha=0.5)+
  scale_fill_viridis(discrete = T,alpha = 0.6)+
  geom_jitter(color="black",size=2.0, alpha=0.9)+ #adiciona pontos
  theme_dark()+
    theme(legend.position = "none",plot.title = element_text(size=12))+
  ggtitle("Um boxplot com pontos")+ #Como colocar titulo
  xlab("alunos")

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


#______________________________________________________
#Dados Evolução
#______________________________________________________
par(mfrow=c(1,1))
ggplot(data, aes(x=Individuo, y=Total,ymax=10,ymin=0))+
  geom_boxplot(color="black",fill="cyan",alpha=0.3)+
  scale_fill_viridis(discrete = T)+
  geom_jitter(color="black",size=2.0, alpha=0.9)+ #adiciona pontos
  theme_minimal()+
  theme(legend.position = "none",plot.title = element_text(size=12))+
  ggtitle("Um boxplot com pontos")+ #Como colocar titulo
  xlab("alunos")+
  ylab("Notas")

ggplot(dados, aes(x=Questao, y=Nota,fill = Questao))+
  geom_boxplot(alpha=0.8,varwidth = T)+
  scale_fill_viridis(discrete = T,alpha = 0.6)+
  geom_jitter(color="black",size=1.5,alpha=0.9)+ #adiciona pontos
  theme_minimal()+
  theme(legend.position = "none",plot.title = element_text(size=12))+
  ggtitle("Boxplot das pontuações das questões da prova de evolução")+ #Como colocar titulo
  xlab("Questões")+
  ylab("pontuação")+
  scale_fill_brewer(palette = "YlOrBr")




