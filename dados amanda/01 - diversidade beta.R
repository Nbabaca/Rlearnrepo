### Curso na mata branca = análise de dados
## Módulo 2, dia 1 = índices de diversidade
## Diversidade beta

## criei este script porque o outro estava muito longo

## vamos criar uma situação hipotética em que queremos a variação das assembleias em áreas com uso antrópico e usos naturais. 
## Lembre-se, a diversidade beta avalia as mudanças composicionais entre locais ou estação (diversidade beta temporal, não trabalhada aqui). 

## vamos importar os dados e depois agrupá-los
## Vamos importar os dados 
data=read.table(file = "clipboard", header=TRUE, stringsAsFactors = T, dec=".")

## Separando entre usos
antrópico=data[data$LULC %in% c("Agriculture","Urban"),]
antrópico_sp=antrópico[,-31:-ncol(antrópico)]
antrópico_sp=antrópico_sp[,-1:-2] ## para remover os fatores

natural=data[data$LULC %in% c("Forest","Grassland"),]
natural_sp=natural[,-31:-ncol(natural)]
natural_sp=natural_sp[,-1:-2] ## para remover os fatores

## baixando os pacotes
install.packages("adespatial")
library(adespatial)

## nossa matriz é de abundância por espécies. No argumento "coef" "j" indica coeficiente de jaccarda, apropriado para presença e ausência. "quant=F" indica o tipo de matriz. Se você quer que a análise leia os dados como matriz de presença e ausência, ainda que seja uma matriz de abundância, mantém quant=F. Se quer que a análise entenda a matriz com abundância, substituir para quant=True ou t. Neste caso, tem que trocar o coenficiente poris jaccard não é apropriado para dados de abundância. No Help da função você encontra todos os coefieicientes possíveis. 

## em "coef" você escolhe a abordagem de diversidade beta (Podani ou Baselga). Consultar o help da função. 

## aqui vamos manter a tabela de composição de presença e ausência com abordagem de Podani, então "coef="J"" e "quant=False". 

beta_antropico<-beta.div.comp(antrópico_sp, coef = "J", quant = FALSE, save.abc = FALSE)
beta_antropico

beta_antr=beta_antropico$part ## salvando os resultados em um objeto. 

beta_natural<-beta.div.comp(natural_sp, coef = "J", quant = FALSE, save.abc = FALSE)
beta_natural

beta_nat=beta_natural$part ## salvando os resultados em um objeto. 


## juntando tudo em um data frama
data_all_res=data.frame(beta_nat, beta_antr)

data_all_res=as.matrix(data_all_res[2:3,]) ## selecionando os dados de interesse

## criando o barplot
barplot(data_all_res,
        col=c("#91cf60","#fc8d59"), 
        xlab = "LULC", 
        ylab = "Beta diversity", 
        ylim=c(0,1))

legend("topright",
       legend = c("Replacement", "Richnees diference"),
       fill = c("#91cf60", "#fc8d59"),
       bty = "n")
box()

## vamos testar?

beta_total_nat=beta_natural$D
beta_total_ant=beta_antropico$D

## precisamos transformar isso em formato longo 
beta_total_nat <- as.vector(beta_natural$D)
beta_total_ant <- as.vector(beta_antropico$D)

dados_long <- data.frame(
  Beta = c(beta_total_nat, beta_total_ant),
  Grupo = c(rep("Natural", length(beta_total_nat)),
            rep("Antrópico", length(beta_total_ant)))
)

## vamos testar os pressuposto de normalidade? 

## Teste de normalidade
residuos <- lm(dados_long$Beta ~ dados_long$Grupo) ## criando o modelo dos resíduos.

## Teste de Shapiro-Wilk
residuos1 <- residuals(residuos)
shapiro.test(residuos1) ## n-normal
 
## Como são duas amostras (antrópico e natural) vamos usar Wilcoxon

wilcox.test(dados_long$Beta ~ dados_long$Grupo)

## houve diferença, logo, a diversidade beta é maior nos pontos com LULC natural. 

## veja que apareceu a mesma mensagem do problema do p exato. Como são dados de distância, variando de 0 a 1, provavelmente os ranks são bem parecidos entre os grupos. Então o valor de p é estimado com a correção que a análise usa. 

## vamos colocar o resultado??

legend("topleft",
       legend = "W=324; p < 0.01",
       bty = "n",
       text.font = 2)
