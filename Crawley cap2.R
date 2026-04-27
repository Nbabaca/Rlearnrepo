head(dados)
names(dados) # usado para saber quais variaveis estão no meu dataframe
attach(dados) # usado para referir as variaveis do meu dataframe sem tem que chamar o nome do dataframe
dados # usado para ver o conteudo do dataframe

# Organização em Cochetes
dados[4] #usado para ver uma coluna (eixo y) especifica do seu dataframe
dados[2,5] #usado para ver uma linha e coluna especifica (vetor z) do seu dataframe
dados[, ,] # "[,x]" quer dizer todas as linhas de uma coluna 'x'. "[x,]" quer dizer todas as colunas de uma linha 'x'
dados[,1:3] # o ":" significa 'todas as coluna de 1 a 3'
dados[25:50,] # exemplo de linhas '25 a 50' de todas as colunas
dados[visitas>10,] #codigo para selecionar linhas especificas (nesse caso linhas com visitas maiores que 10) em todas as colunas

#Sorting
dados[order(visitas),] #utilizado para ordenar em ordem crescente uma linha especifica(nesse caso visitas) em todas as colunas
dados[order(visitas),c(1,3,4)] # mesma coisa porém de colunas especificas
dados[rev(order(dados[,5])),] # rev é utilizado para ordenar em ordem decrescente

#Summarizing the content of Dataframes
summary(dados) #variaveis continuas são sumarizadas em seis 'headings' um parametrico (media aritimetica) e 5 não parametricas (maximo, minimo, mediana e os quartis)
with(dados,tapply(visitas,observador,mean)) #metodo de sumarizar variaveis continuas do seu data frame por variaveis categorigas (nesse caso 'observador')  
aggregate(dados[,5],list(observação),mean) #metodo para classificar a media das variveis continuas(mais de uma) em uma variavel categorica
aggregate(dados[,5],list(pessoa=observador,horario=observação),mean) #metodo para classificar as variaveis continuas em mais de uma variavel categorica

#First Things First: Get to know your data
plot(visitas) #plota o valor da variavel em um 'scatterplot' na ordem que os valores aparecem no datafram (util para achar outliers)
which(visitas>10) #mostra aonde no dataframe existem certos valores (nesse caso maiores que 10)
visitas[50] #ver o valor que se encontra nessa parte do dataframe
table(observador) #utilizado para verificar erros nos fatores das variaveis categoricas

#Relationships

attach(Wasps)
head(Wasps)
x <- exploration_rate
y <- Explored.areas
z <- total_distance
plot(y,x,pch=21,bg="orange") #"pch" significa "plotting character" e muda o simbolo do plot, "bg" muda a cor do simbolo
plot(factor(colonia),Explored.areas) # Quando fazemos um plot com uma variavel categorica quantitativa e uma continua o R produz um boxplot

#Looking for interactions Between Continuos Variables
windows(7,4)
par(mfrow=c(1,2))
plot(y,z)
plot(y,x)
windows(7,7)
coplot(y~x|z,pch = 16, panel = panel.smooth)
