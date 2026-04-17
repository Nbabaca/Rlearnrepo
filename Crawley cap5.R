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
hist(outcome,breaks = (1.5:12.5))
