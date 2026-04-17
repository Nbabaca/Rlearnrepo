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
s2A <- sum((gardenA-mean(gardenA))^2)

mean(gardenB)
gardenB-mean(gardenB)
(gardenB-mean(gardenB))^2
sum((gardenB-mean(gardenB))^2)
sum((gardenB-mean(gardenB))^2)/9 #variancia de B
s2B <- sum((gardenB-mean(gardenB))^2)

mean(gardenC)
gardenC-mean(gardenC)
(gardenC-mean(gardenC))^2
sum((gardenC-mean(gardenC))^2)
sum((gardenC-mean(gardenC))^2)/9 #variancia de C
s2C <- sum((gardenC-mean(gardenC))^2)

var(gardenC)/var(gardenB) #F-test para saber se as variancias são significamentes diferentes
2*(1-pf(10.66667,9,9)) #metodo de ver a significancia do F-test
var.test(gardenB,gardenC) #função integrada ao R do F-test
#LIÇÃO! QUANDO AS VARIANCIAS FOREM DIFERENTES, NÃO COMPARE AS MEDIAS!!!

#Variance and Sample Size
plot(c(0,32),c(0,15),type="n",xlab = "Sample Size",ylab = "Variance")
for (df in seq(3,31,2)) {
  for(i in 1:30){
    x<- rnorm(df,mean = 10,sd=2)
    points(df,var(x)) }}
#O plot que é montado indica a relação entre a variancia e o tamanho da amostra e essa relação mostra que quanto menor o tamanho da amostra maior sera a variancia

#Using Variance
#Two main ways: for estaabilishing, measures of unreliability/ for testing hypothesis
 sqrt(var(gardenA)/10) #erro padrão do canteiro A
 sqrt(var(gardenB)/10) #erro padrão do canteiro B
 sqrt(var(gardenC)/10) #erro padrão do canteiro C
 
 #Confidence intervals
 qt(.025,9) # codigo para calcular valores de "student's T test"
qt(.975,9) 
#Values of Student’s t are numbers of standard errors to be expected with specified probability and for a given number of degrees of freedom.
qt(.995,9)
qt(.9975,9)
qt(.975,9)*sqrt(1.33333/10) #Calculo do intervalo de confiança para o "gardenB"
#and we would present the result in written work like this: ‘The mean ozone concentration in garden B was 5.0± 0.826 (95% CI, n= 10).’

#Bootstrap
Wasps <- read.table("clipboard", header = TRUE, sep = "\t")
head(Wasps)
attach(Wasps)
names(Wasps)

