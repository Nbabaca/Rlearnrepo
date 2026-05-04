#27/04/2026
#Two Samples 6:cap/Crawley
#GFP
#______________________________________

#Comparing two Variances

  qf(0.975,9,9) # Valor critico que nos da uma regua para concluir se as duas variancias são significantes o bastante, ou seja se passar de 4.025 a hipotese nula (Que elas não são significantes) é descartada
attach(f_test_data)
names(f_test_data)
var(gardenB)
var(gardenC)
F.ratio <- var(gardenC)/var(gardenB) #compara dos variancias, dividindo a maior pela menor
F.ratio #nesse caso como passou do valor critico, foi descartada e as variancias são significativas
2*(1-pf(F.ratio,9,9)) #Calculo do valor de P da hipotese alternativa (muito significativo)
var.test(gardenB,gardenC) #Teste que compara as variancias

#Comparing Two Means
#existem dois testes para comparar as medias de dua amostras independentes

#Student's T test 
qt(0.975,18) #Valor critico para determinar se as medias são significamente diferentes no valor p=0.05
attach(t_test_data)
names(t_test_data)
ozone <- c(gardenA,gardenB)
label <- factor(c(rep("A",10),rep("B",10)))
boxplot(ozone~label,notch=T,xlab = "Garden",ylab = "Ozone") #Boxplot das duas medias do Garden A e B
s2a <- var(gardenA)
s2b <- var(gardenB)
(mean(gardenA)-mean(gardenB))/sqrt(s2a/10+s2b/10) #Forma de calcular o teste T (ignorar o negativo)
#Como o teste T deu um valor maior que 2.1 a hipotese nula foi descartada e as medias são significamente diferentes
2*pt(-3.872983,18) #Valor de P das medias serem iguais
t.test(gardenA,gardenB) #Codigo do R que equivale ao teste T

#Wilcoxon Rank sum Test
ozone #esse metodo é utilizado quando o erro padrão não segue uma normal
label <- c(rep("A",10),rep("B",10))
label
combined.ranks <- rank(ozone)
combined.ranks
tapply(combined.ranks,label,sum) #apos ranquear os valores é calculado a soma dos valores ranqueados para cada garden e comparamos o menor valor com uma tabela (GardenA) que deu 66 e para sample size 10 é um valor significativo
wilcox.test(gardenA,gardenB)  #Codigo para fazer automaticamente 
#O teste de wilcox é mais conservador que o test T

#test on paired samples
attach(streams)
names(streams) #esse set de dados é pareado pois os dados foram retirados da mesma queda de esgoto
t.test(down,up) #pelo set de dados ser pareado, o teste T vê ele como não significativo
t.test(down,up,paired=T) #logo é necessario especificar que os valores são pareados, mudando assim o valor de P
d <- up-down
t.test(d) #Ao fazer o teste t com uma amostra reduzimos os Df e obtivemos o mesmo valor de P assim diminuindo a variancia do erro

#The Sign test
 binom.test(1,9)
 
sign.test <- function(x,y)
{
  if(length(x)!=length(y))stop("The two variables must be the same length")
  d <- x-y
  binom.test(sum(d > 0),length(d))
} #Função que calcula o sign test
sign.test(gardenA,gardenB) #quando duas coisas são iguais o teste parametrico é mais forte que o não parametrico

#Binomal Test to Compare Two Proportions 
prop.test(c(4,196),c(40,3270)) ##teste para identificar significancia entre duas proporções, nesse caso para ver se ha discriminação positiva entre duas proporções, nesse caso não houve significancia

#Chi-square Contingency Tables
qchisq(0.95,1) #Codigo para determinar o valor critico do qui quadrado, o primeiro valor sendo o quantil e o segundo o Df
count <- matrix(c(38,14,11,51),nrow=2) #forma de criar uma matrix 
count
chisq.test(count)  #com a correção de Yates
chisq.test(count,correct = F) #sem a correção de yates, e mesmo assim os dois valores mostram alta significancia entre os olhos e a cor do cabelo, assim discartando a hipotese nula

