#4/10/2026
#Central Tendency 3:cap/ Crawley
#GFP
#___________________________________________________________

# Jeito classico de trazer tabelas 
dados <- (`dados.biodecampo.(1)...dados.biodecampo.(1).csv`)
as.data.frame(dados)
dados <- as.data.frame(dados)
head(dados)

#jeito rapido de trazer tabelas
datas<- read.table("clipboard", header = TRUE, sep = "\t")
head(datas)
summary(datas)

# como ver a "Moda" dos seus dados
hist(dados[,5])

# como calcular a media aritimetica dos seus dados
y <- dados[,5] # cria um obejto para a coluna que queremos saber a media(facilita) 
total <- sum(dados[,5]) # soma todo os numeros da coluna especifica
n <- length(dados[,5]) # ve o tamanho da coluna
ybar <- sum(dados[,5])/length(dados[,5]) # da um valor para a media aritimetica
arithmetic.mean <- function(x) {sum(x)/length(x)} # cria uma função para calcular a media
arithmetic.mean(y)
mean(y) # jeito mais facil de conseguir a media (recomendado, mas sensivel a outliers)

# como calcular a mediana dos seus dados
sorted <- sort(y) # necessario para ordenar os numeros em ordem crescente
length(y)/2 # metodo caso o vetor tenha um numero impar de numeros
sorted[ceiling(length(y)/2)] #vetor impar
length(y)
sort(sorted)[72]
sort(sorted)[73]
(sort(sorted)[72]+sort(sorted)[73])/2 #metodo para caso o vetor seja par
38%%2 #modulo de par
39%%2 #modulo de impar
med <- function(x) {odd.even <- length(x)%%2 if,odd==en==0)(sort(x)[length(x)/2] + sort(x)[1+length(x)/2])/2 else sort(x)[ceiling(length(x)/
#incompleto
  

#Media Geometrica como calcular
 100000^0.2 #metodo de calcular raiz de numeros ("^" simbolo de raiz)
 
 
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  