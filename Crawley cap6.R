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
#existem dois testes para comparar as medias de duas amostras independentes

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

#Fischer's Exact Test
#É utilizado quando um dos valores da tabela de contingencia é menor que 5
factorial <- function(x)max(cumprod(1:x)) #função para calcular factoriais
x <- as.matrix(c(6,4,2,8))
dim(x) <- c(2,2) #forma alternativa de montar uma matrix
x
fisher.test(x) #codigo para o teste de fisher
attach(fisher) #é possivel fazer esse teste com mais matrizes maiores que 2x2
fisher.test(tree,nests)

#Correlation and Covariance
#Covariação é a expectativa do produtor do vetor
data <- read.csv("Dados crawley/twosample.csv")
attach(data)
plot(x,y)
var(x)
var(y)
var(x,y) #covariancia de x e y
var(x,y)/sqrt(var(x)*var(y)) #forma de calcular coeficiente de correlação
cor(x,y) #O coeficiente de correlação é a covariancia dividida pela media geometrica das dua variancias

#Correlation and the Variance of Differences Between Variables
 paired <- read.csv("Dados crawley/water.table.csv")
attach(paired)
names(paired) 
cor(Summer,Winter) #cor é o codigo para avaliar a correlação entre dois vetores
cor.test(Summer, Winter) #teste para avaliar a significancia da correlação entre dois vetores
Vars = var(Summer)
varw = var(Winter)
vard = var(Summer-Winter)
(Vars + varw - vard)/(2*sqrt(Vars)*sqrt(varw)) #esse calculo comprova a correlação entre os dois vetores
Vars + varw -2*0.8820102*sqrt(Vars)*sqrt(varw) # calculo para a diferença das variancias

#Scale dependent Correlations
# Graficos podem ser enganosos, não confie em tudo que o grafico mostra
par(mfrow=c(1,1))
rm(x,y)
produtividade <- read.csv("Dados crawley/productivity.csv")
View(produtividade)
attach(produtividade)
names(produtividade)
plot(productivity,mammals,pch=16,col="blue") #o grafico mostra que a riqueza de especies aumenta conforme a produtividade
cor.test(productivity,mammals,method = "spearman")
plot(productivity,mammals)
