#14/04/2026
#Variance 4:cap/Crawley
#GFP
#______________________________________

y <- c(13,7,5,12,9,15,6,11,9,7,12)
plot(y,ylim=c(0,20)) 
range(y)#Forma de quantificar variancia em um scatter 
y-mean(y)
(y-mean(y))^2
sum((y-mean(y))^2) #Metodo de calcular a soma dos quadrados
# degrees of freedom= N-1
 variance <- function(x) sum((x-mean(x))^2)/(length(x)-1) # função para variancia
variance(y)
var(y) #função integrada ao R para variancia

#Exemplo pratico
ozone<- read.table("clipboard", header = TRUE, sep = "\t")
head(ozone)
summary(ozone)
attach(ozone)
mean(gardenA)
gardenA-mean(gardenA)
(gardenA-mean(gardenA))^2
sum((gardenA-mean(gardenA))^2)
sum((gardenA-mean(gardenA))^2)/9 #variancia de A

mean(gardenB)
gardenB-mean(gardenB)
(gardenB-mean(gardenB))^2
sum((gardenB-mean(gardenB))^2)
sum((gardenB-mean(gardenB))^2)/9 #variancia de B

mean(gardenC)
gardenC-mean(gardenC)
(gardenC-mean(gardenC))^2
sum((gardenC-mean(gardenC))^2)
sum((gardenC-mean(gardenC))^2)/9 #variancia de C

var(gardenC)/var(gardenB) #F-test para saber se as variancias são significamentes diferentes
2*(1-pf(10.66667,9,9)) #metodo de ver a significancia do F-test
var.test(gardenB,gardenC) #função integrada ao R do F-test
#LIÇÃO! QUANDO AS VARIANCIAS FOREM DIFERENTES, NÃO COMPARE AS MEDIAS!!!
    