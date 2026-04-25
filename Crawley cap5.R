#14/04/2026
#Single Samples 5:cap/Crawley
#GFP
#___________________________________________
#Single Samples
Wasps <- read.table("clipboard", header = TRUE, sep = "\t")
head(Wasps)
attach(Wasps)
names(Wasps)
y <- (exploration_rate)
summary(Explored.areas)
hist(exploration_rate)
boxplot(exploration_rate)
length(table(y)) #Jeito de ver quantos valores diferente de "y" existem no DF
plot(range(y),c(0,10))
for(i in 1:36) lines(c(y[i],y[i]),c(0,2),col= "red") #Codigo para um "rug plot"

#Normal distribution
 score <- 2:12 #Valores possiveis de rolar 2 D6
ways <- c(1,2,3,4,5,6,5,4,3,2,1) #numeros de jeitos de conseguir cada um dos resultados 
game <- rep(score,ways) # jeito de produzir uma função para todos os 36 possiveis resultados
game
sample(game,1)
outcome <- numeric(10000)
for(i in 1:10000) outcome[i] <- sample(game,1) #repetimos 10000 vezes o jogo
hist(outcome,breaks = (1.5:12.5)) #histograma dos resultados do jogo
mean.score <- numeric(10000)
for(i in 1:10000)mean.score[i] <- mean(sample(game,3))
hist(mean.score,breaks = (1.5:12.5)) #histograma da media de 3 jogos
mean(mean.score)
sd(mean.score)
xv <- seq(2,12,0.1) #eixo x da função para geração de uma curva suave encima do histograma
yv <- 10000*dnorm(xv,mean(mean.score),sd(mean.score))
hist(mean.score,breaks=(1.5:12.5),ylim=c(0,3000),
     col="yellow",main="") #histograma ajeitado da media dos 3 jogos
lines(xv,yv,col="red") #cria as linhas em formato de sino encima do histograma evidendenciando que é uma distribuição normal

#desvio padrão e media da normal
standard.deviation <- seq(-3,3,0.01)
pd <- dnorm(standard.deviation)
plot(standard.deviation,pd,type = "l",col="blue") #Codigo para fazer um grafico de distribuição normal com media = 0 e desvio padrão = 1
pnorm(-2) #forma de descobrir a area embaixo da curva da normal no valor -2 da distribuição
pnorm(-1)
1-pnorm(+3) #para saber a area de numeros positivos é necessario subtrair o valor obtido por 1

#Forma de descobrir qual valor na distribuiçao normal corresponde ao P = 0.05
qnorm(c(0.025,0.975))
#E esses valores obtidos são muito importantes pois nos mostram que 95% dos valores randomicos em uma distribuição normal vão estar entre -1.96 e +1.96

#Calculations using z of the Normal Distribution
ht <- seq(150,190,0.01)
plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab = "probability density", xlab = "height")
pnorm(-1.25) #Probabilidade de uma pessoas ser menor que 1.60 nessa distribuição
1-pnorm(1.875) #Probabilidade de alguem ser maior que 1,85 nessa distribuição
pnorm(1.25)-pnorm(-0.625) #Probabilidade de alguém ter entre 1,60 e 185 nessa distribuição

#Função polygon
par(mfrow=c(2,2))
ht <- seq(150,190,0.01)
pd <-dnorm(ht,170,8)
plot(ht,dnorm(ht,170,8),type = "l",col = "brown",
     ylab = "Probability density",xlab = "height")
plot(ht,dnorm(ht,170,8),type = "l",col = "brown",
     ylab = "Probability density",xlab = "height")
yv <- pd[ht<=160]
xv <- ht[ht<=160]
xv <- c(xv,160,150)
yv <- c(yv,yv[1],yv[1])
polygon(xv,yv,col = "orange")
 
plot(ht,dnorm(ht,170,8),type = "l",col = "brown",
     ylab = "Probability density",xlab = "height")
xv <- ht[ht>= 185]
yv <- pd[ht>=185]
xv <- c(xv,190,185)
yv <- c(yv,yv[501],yv[501])
polygon(xv,yv,col="blue")

plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab="Probability density",xlab="Height")
xv <- ht[ht>=160 & ht <= 180]
yv <- pd[ht>=160 & ht <= 180]
xv <- c(xv,180,160)
yv <- c(yv,pd[1],pd[1])
polygon(xv,yv,col="green")

#Plots for testing Normality of Single Samples
setwd("F:/Ricardo/aprendizado/Rlearnrepo")
data <- read.csv("skewdata.csv")
attach(data)
qqnorm(values) #Distribuição de quantis plotada
qqline(values,lty=2)#Distribuição normal em linha
#Esse plot é importante para mostrar que esse set de dados não segue uma distribuição normal (apresenta um formato de S) e pende para a esquerda
light <- read.csv("light.csv")
attach(light)
names(light)
hist(speed)
summary(speed) #Nesse histograma criado vimos uma distribuição não normal, e uma boa forma de detectar outliers é diminuindo os valores do terceiro com o primeiro quartil e multiplicando por 1.5 e depois somando ou diminuindo com o terceiro ou o primeiro quartil respectivamente.

#Inference in the One-Sample Case
wilcox.test(speed,mu=990) #teste de P para distribuições não normais de medida unica

#Bootstrap in Hypothesis Testing with Single Samples
a <- numeric(10000)
for (i in 1:10000) a[i] <- mean(sample(speed,replace=T))
hist(a)

#Student's T distribution
plot(c(0,30),c(0,10),type = "n",
     xlab="degrees of Freedom",ylab="students t value")
lines(1:30,qt(0.975,df=1:30),col="red")
abline(h=1.96,lty=2,col="green") #O grafico montado mostra qual o limite de graus de liberdade onde é possivel utilizar a distribuição de students T, sendo 2 o valor critico desse limite
xvs <- seq(-4,4,0.01)
plot(xvs,dnorm(xvs),type = "l",
     ylab="probability density",xlab = "deviates")
lines(xvs,dt(xvs,df=5),col="red")
qt(0.975,5) #O grafico montado mostra como a distribuição de T tem uma maior probabilidade de cair nos extremos da distribuição,ao inves de ser o valor 1.96 como na normal é 2.57 que corresponde ao P=0.05

#Higher-order Moments of a Distribution

