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

