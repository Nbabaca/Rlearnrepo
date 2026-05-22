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
for (i in 1:9) lines(c(tannin[i],tannin[i]),c(growth[i],fitted[i])) #Forma de automatizar a distancia dos residuais para com a linha

#_________________________________________
#Calculations Involved in Linear Regression
#_________________________________________
tannin
growth        
tannin*growth                                 
sum(tannin*growth)

SSX = sum(tannin^2)-sum(tannin)^2/length(tannin)
SSX #somatorio dos quadrados de X

SSY = sum(growth^2)-sum(growth)^2/length(growth)
SSY #somatorio total dos quadrados

SSXY = sum(tannin*growth)-sum(tannin)*sum(growth)/length(tannin)
SSXY #somatorio dos produtos

qf(0.95,1,7) # Valor minimo de F para refutar a hipotese nula (sendo ela que  Y tem zero efeito em X logo não há correlação linear entre os dois)

1-pf(30.974,1,7) #calculo da possibilidade na hipotese nula ser verdadeira caso o Valor de F fosse 30.974, em sintese seria menor que 0.001, logo consideramos a hipotese nula falsa

model <- lm(growth~tannin)
summary(model) #resume todos os valores que obtemos até agora
summary.aov(model) #mostra a tabela anova da regressão

#_________________________________________________
#Measuring the degree of Fit r^2
#_________________________________________________
#Model checking
r^2
par(mfrow=c(2,2))
plot(model)
model
influence.measures(model)

#________________________________________________
#Transformation 
#________________________________________________
par(mfrow=c(1,1))
data <- read.csv("Dados crawley/decay.csv")
head(data)
attach(data)
names(data)
plot(time,amount,pch=21,cl="blue",bg="green")
abline(lm(amount~time),col="red")
