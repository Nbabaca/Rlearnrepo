#14/04/2026
#Regression:cap/Crawley
#GFP
#___________________________________________


#___________________________________________
#Linear Regression
#___________________________________________
dados <- read.csv("Dados crawley/tannin.csv")
attach(dados)
names(dados)
plot(tannin,growth,pch=21,bg="blue")
#essa é uma regressão relativamente simples, logo é possivel tirar de olho a função da linha
#sendo ela aproximadamente y = 12.0 - 1.25x

#__________________________________________
#Linear Regression in R
#__________________________________________
lm(growth~tannin) #lm é o codigo para saber os valor real da função, sendo 11.756 -1.217x
#"we want to find the values of the slope and intercept that make the data most likely"
abline(lm(growth~tannin),col="green") #"abline cria a linda da função do plot, porém ela não é perfeita pois existem pontos fora da linha, esses pontos são conhecidos como residuos
fitted <- predict (lm(growth~tannin)) 
fitted #esta funcção "predict" mostra aonde o x se encontra com o y na função linear
lines(c(0,0),c(12,11.755556)) #a função "lines" criou uma linha entre o 12 e o 11.755556 da linha mostrando o quanto eles diferem

#_________________________________________
#Calculations Involved in Linear Regression
#_________________________________________
tannin
growth        
tannin*growth                                 
sum(tannin*growth)
