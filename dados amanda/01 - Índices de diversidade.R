## Curso na mata branca = análise de dados
## Módulo 2, dia 1 = índices de diversidade

## Vamos importar os dados 
data=read.table(file = "clipboard", header=TRUE, stringsAsFactors = T, dec=".") ## vamos usar os mesmos dados

## para fazer os calcúlos precisamos retirar tudo o que é fator

sp=data[,-1:-2]

## vamos tirar tbm os dados abióticos

sp_data=sp[,-31:-ncol(sp)]

## vamso transformar LULC em fator
LULC=factor(data$LULC, levels=c("Agriculture","Urban", "Forest", "Grassland"))

## para ver a composição basta olhar sp_data. Vamos aprender a representar a composição mais pra frente. 

## Riqueza de espécies
## Calculando a riqueza observada de espécies para cada comunidade
library(vegan)
riqueza_sp <- specnumber(sp_data)
riqueza_sp

## vamos ver agora a riqueza por lulc? 
boxplot(riqueza_sp ~ LULC,
        col = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
        xlab = "LULC",
        ylab = "Species richness",
        ylim = c())

## se quisermos saber onde a riqueza é maior, precisamos testar com anova ou kruska. 

## testando normalidade
## Teste de normalidade
residuos <- lm(riqueza_sp ~ LULC) ## criando o modelo dos resíduos.

## Teste de Shapiro-Wilk
residuos1 <- residuals(residuos)
shapiro.test(residuos1) ## n-normal

## como já deu não normal, não preciso nem ir pro próximo pressuposto. Vamos fazer Krsuka-Wallis

kruskal.test(riqueza_sp~LULC)
## A riqueza difere entre as áreas. 
## vamos rodar o dunntest para saber onde que difere
install.packages("Rtools")
install.packages("FSA")
library(FSA)

dunn=dunnTest(riqueza_sp ~ LULC, method = "holm")

## nesse caso, a menor riqueza foi encontrada em Grassland

## vamos colocar as letras?

## vamos ver agora a riqueza por lulc? 
boxplot(riqueza_sp ~ LULC,
        col = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
        xlab = "LULC",
        ylab = "Species richness",
        ylim = c(4,16))

install.packages("rcompanion")
library(rcompanion)

# Letras
dunletras=cldList(P.adj ~ Comparison,
                  data = dunn$res,
                  threshold = 0.05)

## agriculture = ab
## urban= b
## Forest= ab
## Grassland = a

letras=c("ab","b","ab","a")

# Posição das letras (um pouco acima de cada caixa)
y.pos <- tapply(riqueza_sp, LULC, max) + 1

## esse código cria posições das letras baseados dos valores de Ph em cada categorias. A função "max" permite posicionar as letras acima de cada box plot. Somando com 0,05 as letras não ficam sobrepostas. 

text(x = 1:4,
     y = y.pos,
     labels = letras,
     font = 1,      # negrito
     cex = 1)

legend("bottomright",
       legend = "Chi=9.93; p = 0.01",
       bty = "n",
       text.font = 2)

## outra forma de calcular a riqueza
richness=rowSums(sp_data>0)

## vamos calcular a abundância 
Abundância=rowSums(sp_data)

## testando se a abundância difere entre LULCs
## Teste de normalidade
residuos <- lm(Abundância ~ LULC) ## criando o modelo dos resíduos.

## Teste de Shapiro-Wilk
residuos1 <- residuals(residuos)
shapiro.test(residuos1) ## n-normal

## Kruska-Wallis
kruskal.test(Abundância~LULC)
## abundância difere

## vamos rodar o teste de dunn
dunn=dunnTest(Abundância ~ LULC, method = "holm")

library(rcompanion)

# Letras
dunletras=cldList(P.adj ~ Comparison,
                  data = dunn$res,
                  threshold = 0.05)

## agriculture = ab
## urban= b
## Forest= ab
## Grassland = a

boxplot(Abundância ~ LULC,
        col = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
        xlab = "LULC",
        ylab = "Species abundance",
        ylim = c(10,50))

## vamos colocar as letras?

letras=c("ab","b","ab","a")

# Posição das letras (um pouco acima de cada caixa)
y.pos <- tapply(Abundância, LULC, max) + 1.5

## esse código cria posições das letras baseados dos valores de Ph em cada categorias. A função "max" permite posicionar as letras acima de cada box plot. Somando com 0,05 as letras não ficam sobrepostas. 

text(x = 1:4,
     y = y.pos,
     labels = letras,
     font = 1,      # negrito
     cex = 1)

legend("bottomright",
       legend = "Chi=10.22; p = 0.02",
       bty = "n",
       text.font = 2)

## vamos calcular as diversidades de Shannon e Simpson
## shannon dá peso as espécies raras
## Simpson dá peso as espécies dominantes
## Lmebre-se disso antes de escolher qual índice usar

## Índice de Shannon

shannon_res <- diversity(sp_data, index = "shannon")
shannon_res

simpson_res <- diversity(sp_data, index = "simpson")
simpson_res

## vamos testar?

## Teste de normalidade
residuos <- lm(simpson_res ~ LULC) ## criando o modelo dos resíduos.

## Teste de Shapiro-Wilk
residuos1 <- residuals(residuos)
shapiro.test(residuos1) ## n-normal para simpson e shannon

## Kruska-Wallis
kruska_shannon=kruskal.test(shannon_res~LULC)

## shannon difere
## simpson difere

## vamos rodar o teste de dunn
dunn=dunnTest(shannon_res ~ LULC, method = "holm")

library(rcompanion)

# Letras
dunletras=cldList(P.adj ~ Comparison,
                  data = dunn$res,
                  threshold = 0.05)

## agriculture = ab
## urban= b
## Forest= a
## Grassland = a

boxplot(shannon_res ~ LULC,
        col = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
        xlab = "LULC",
        ylab = "Shannon diversity",
        ylim = c(1,3))

  ## vamos colocar as letras?

letras=c("ab","b","a","a")

# Posição das letras (um pouco acima de cada caixa)
y.pos <- tapply(shannon_res, LULC, max) + 0.1

## esse código cria posições das letras baseados dos valores de Ph em cada categorias. A função "max" permite posicionar as letras acima de cada box plot. Somando com 0,05 as letras não ficam sobrepostas. 

text(x = 1:4,
     y = y.pos,
     labels = letras,
     font = 1,      # negrito
     cex = 1)

legend("bottomright",
       legend = "Chi=10.42; p = 0.02",
       bty = "n",
       text.font = 2)

kruska_simpson=kruskal.test(simpson_res~LULC)

## simpson difere

## vamos rodar o teste de dunn
dunn=dunnTest(simpson_res ~ LULC, method = "holm")

library(rcompanion)

# Letras
dunletras=cldList(P.adj ~ Comparison,
                  data = dunn$res,
                  threshold = 0.05)

## agriculture = ab
## urban= b
## Forest= a
## Grassland = a

boxplot(simpson_res ~ LULC,
        col = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
        xlab = "LULC",
        ylab = "Shannon diversity",
        ylim = c(0.70,1))

## vamos colocar as letras?

letras=c("ab","b","a","a")

# Posição das letras (um pouco acima de cada caixa)
y.pos <- tapply(simpson_res, LULC, max) + 0.02

## esse código cria posições das letras baseados dos valores de Ph em cada categorias. A função "max" permite posicionar as letras acima de cada box plot. Somando com 0,05 as letras não ficam sobrepostas. 

text(x = 1:4,
     y = y.pos,
     labels = letras,
     font = 1,      # negrito
     cex = 1)

legend("bottomright",
       legend = "Chi=10.68; p = 0.02",
       bty = "n",
       text.font = 2)

## vamos calcular a equabilidade de Pielou

## Índice de Pielou
Pielou <- shannon_res/log(riqueza_sp)

## vamos testar
## Teste de normalidade
residuos <- lm(Pielou ~ LULC) ## criando o modelo dos resíduos.

## Teste de Shapiro-Wilk
residuos1 <- residuals(residuos)
shapiro.test(residuos1) ## normal 

## testando homogeneidade de variâncias
bartlett.test(Pielou ~ LULC) ## variâncias homogêneas

## vamos fazer ANOVA

pielouaov=aov(Pielou ~ LULC)
summary(pielouaov)
## não difere entre as áreas
## não precisamos fazer tukey

boxplot(Pielou ~ LULC,
        col = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
        xlab = "LULC",
        ylab = "Pielou eveness",
        ylim = c(0.85,1))

legend("bottomright",
       legend = "F=1.53; p = 0.25",
       bty = "n",
       text.font = 2)
