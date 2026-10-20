#BIO DE CAMPO GRUPO 1
#RBNLY
#TESTE DE GLM
#PL1 POLINIZADORES
#_______________________________________
pak::pak("r-lib/devtools")
install.packages("remotes")
install.packages("pak")
install.packages("visdat")
install.packages("lattice")
install.packages("RVAideMemoire")
install.packages("DHARMa")
install.packages("performance")
install.packages("MuMIn")
install.packages("piecewiseSEM")
install.packages("MASS")
install.packages("ggExtra")
install.packages("Rmisc")
install.packages("emmeans")
install.packages("sjPlot")
install.packages("bbmle")
install.packages("glmmTMB")
install.packages("ordinal")
install.packages("ecolottery")
install.packages("naniar")
install.packages("vcd")
install.packages("generalhoslem")
devtools::install_github("paternogbc/ecodados")
FORinstall.packages("devtools")
install.packages("ordinal")
#__________________________________
#Pacotes
#__________________________________
library(devtools)
library(ecodados)
library(visdat)
library(tidyverse)
library(lattice)
library(RVAideMemoire)
library(DHARMa)
library(performance)
library(MuMIn)
library(piecewiseSEM)
library(MASS)
library(ggExtra)
library(Rmisc)
library(emmeans)
library(sjPlot)
library(bbmle)
library(glmmTMB)
library(ordinal)
library(car)
library(ecolottery)
library(naniar)
library(vcd)
library(generalhoslem)
library(remotes)
library(pak)
library(viridis)
library(ordinal)
#_______________________________
#Analise de Poisson
#_______________________________
attach(fragmentos)
fragmentos<- ecodados::fragmentos
## Explorar os dados
glimpse(fragmentos)
## Grafico
ggplot(fragmentos, aes(dfrag,Riqueza_obs,size = 4))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x= "Distancia para o fragmento mais proximo", y = "riqueza observada")+
  tema_livro()
## Modelo
mod_pois <- glm(Riqueza_obs ~ dfrag, family = poisson(link = "log"),data = fragmentos)
## Diagnose basica
par(mfrow = c(2,2))
plot(mod_pois)

## Diagnose avançada
SimulationOutput <- simulateResiduals(fittedModel = mod_pois, plot = TRUE)

## Overdispersion
par(mfrow = c(1,1))
testDispersion(mod_pois)

## Testar a presença de overdispersion
check_overdispersion(mod_pois)

## Dispersion parameter
deviance(mod_pois) / df.residu (mod_pois)

## Ajuste do modelo
mod_nb <- glm.nb(Riqueza_obs ~ dfrag,data= fragmentos)
par(mfrow = c(2,2))
#diagnose
plot(mod_nb)
par(mfrow = c(1,1))   
(chat <- deviance(mod_nb)/ df.residual(mod_nb)) #Dispersion parameter   

## Diagnose avançada
SimulationOutput <- simulateResiduals(fittedModel = mod_nb, plot = TRUE)
summary(mod_nb)
## Dados de contagem: a distribuição binomial
uv_cells <- ecodados::uv_cells

goodfit(`VISITAS APIS`)
Ord_plot(LUXIMETRO)

#Grafico de luminosidade por tratamento


dados_finais$LUX_SCALE= scale(dados_finais$LUXIMETRO)#como criar uma nova coluna  
dados$tempo_de_encontro_SCALE = scale(dados$tempo_de_encontro)
#grafico tratamento por densidade boxplot
ggplot(dados_finais,aes( x = TRATAMENTO, y = DENSIDADE_po))+
         geom_boxplot(color="black",fill = "yellow")+
  ggtitle("Boxplot de Densidade de Aspilia por Tratamento na Parcela")
       


#_______________________________________________
#PL2 FORMIGAS
#_______________________________________________

#Dataset
dados<-read.csv("Dados monitoria/PL2 grupo barbies.csv")

head(dados)

attach(dados)
#____________________________________________________________________________
#nivel_agressividade
#___________________________________________________________________________
hist(nivel_agressividade)
shapiro.test(nivel_agressividade)
wilcox.test(nivel_agressividade~tratamento)
#boxplot da analise inicial dos dados, a mais simples, possivelmente um wilcox pois não segue uma distribuição normal
library(dplyr)
library(ggplot2)

# Calcular a média de agressividade por embate e tratamento
dados_media = dados%>%
  group_by(embate,tratamento)%>%
  summarise(n=n(),
            media=mean(nivel_agressividade),
            mediana=median(nivel_agressividade),
            desvio_padrao=sd(nivel_agressividade))

ggplot(dados_media, aes(x = factor(embate), y = media, fill = tratamento)) +
  geom_histogram(stat = "identity", color = "black", position = position_dodge()) +
  facet_wrap(~tratamento,scale="free")+
  labs(
    x = "Embate",
    y = "Nível de agressividade",
    title = "Nível de agressividade por embate e tratamento"
  ) +
  theme_classic()

library(ggplot2)

ggplot(dados_media, aes(x = media, fill = tratamento)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7,position = position_dodge()) +
  facet_wrap(~tratamento) +
  scale_x_continuous(breaks = seq(0, 4, 1)) +
  labs(
    x = "Nível de Agressividade",
    y = "Frequência",
    title = "Distribuição da agressividade por tratamento",
    fill = "Tratamento"
  ) +
  theme_classic()


dados%>%
  mutate(media = mean(nivel_agressividade)) %>%
ggplot(aes(x = embate, y = )) +
  geom_bar(stat = "identity")+
  facet_wrap(~tratamento, scale="free")+
  labs(
    x = "Tratamento",
    y = "Nível de agressividade",
    title = "Nível de agressividade por tratamento"
  ) +
  theme_classic()

#tempo de encontro
hist(tempo_de_encontro)
shapiro.test(tempo_de_encontro)
wilcox.test(tempo_de_encontro~tratamento)

ggplot(dados, aes(x = tratamento, y = tempo_de_encontro)) +
  geom_violin(fill = "purple") +
  geom_jitter(width = 0.15, alpha = 0.7) +
  labs(
    x = "Tratamento",
    y = "Tempo de Encontro",
    title = "tempo de encontro por tratamento"
  ) +
  theme_classic()
head(dados)

#____________________________________________________________
                #GLMM e CLMM
dadosf <- dados
#____________________________________________________________               
                      #CLMM
#____________________________________________________________
dadosf$nivel_agressividade_ord <- factor(dadosf$nivel_agressividade,levels = c("0","1","2","3","4"),ordered = T)
dadosf$tratamentof <- as.factor(dadosf$tratamento)
head(dadosf)
            
#Modelo
modelo <- clmm(nivel_agressividade_ord~tratamentof+tempo_de_encontro+Tamanho+
               (1|horario_encontro),data = dadosf, threshold = "equidistant",Hess = T)
summary(modelo)
head(cbind(dadosf, fitted(modelo)))
head(cbind(dadosf, pred = predict(modelo, new_data = dadosf)))
view(dadosf)
dadosf$pred = predict(modelo)


#GLMM
modelo2 <- glmmTMB(tempo_de_encontro~tratamentof+Tamanho+nivel_agressividade,data = dados,family = poisson)
summary(modelo2)

head(dados)
                #Grafico do glmm
#Nivel de agressividade
dados_res <- summarySE(dados,measurevar = "nivel_agressividade",
                       groupvars = c("tratamento","tempo_de_encontro"))

pd <- position_dodge(0.1)
ggplot(dados_res,aes(x = tempo_de_encontro, y = nivel_agressividade, colour = tratamento,
           group = tratamento, fill = tratamento)) +
  geom_errorbar(aes(ymin=nivel_agressividade-se, ymax=nivel_agressividade +se),
                width=.1, size = 1.1, position=pd) +
  geom_line(position=pd, size = 1.1) +
  geom_point(pch = 21, colour = "black", position=pd, size=3.5) +
  scale_colour_manual(values = c("darkorange", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  xlab("Tempo de encontro (segundos)") +
  ylab("nivel de agressividade") +
  theme_classic()

#Ttheme_classic(#Tempo medio de encontro
obj2 <- emmeans(modelo2, ~ tratamento, type="response")
summary(obj2)
obj2_df <- as.data.frame(obj2)

ggplot(obj2_df, aes(x = tratamento, y = rate))+
  geom_point(size=4)+
  geom_line(aes(group = 1),linewidth=0.7)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL))+
  theme_classic()+
  labs(x = "tratamento", y = "tempo medio de encontro")

            #Testes em relação ao CLMM


pred_interativo <- ggpredict(modelo, terms = c("tempo_de_encontro", "tratamentof"))
p <- plot(pred_interativo)
p
ggplotly(p)

# Combinar preditores #FEIO
# Gráfico principal TERRIVEL
pred <- ggpredict(modelo, terms = c("tempo_de_encontro", "tratamento", "Tamanho"))

ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2) +
  facet_wrap(~facet, labeller = label_both) +
  labs(title = "Probabilidades Previstas do Modelo Ordinal",
       x = "Tempo de Encontro", 
       y = "Probabilidade",
       color = "Tratamento",
       fill = "Tratamento") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Para visualizar cada nível da variável resposta
pred_categorias <- ggpredict(modelo, terms = "tratamentof")

ggplot(pred_categorias, aes(x = x, y = predicted, color = response.level)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  facet_wrap(~response.level) +
  labs(x = "Tratamento", 
       y = "Probabilidade",
       color = "Nível") +
  theme_minimal()

# Extrair efeitos aleatórios
ranefs <- ranef(modelo)

# Plotar para horario de encontro
ggplot(ranefs$horario_encontro, aes(x = rownames(ranefs$horario_encontro), y = `(Intercept)`)) +
  geom_bar(stat = "identity") +
  labs(x = "Horario encontro", y = "Efeito Aleatório") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotar para ID
ggplot(ranefs$ID, aes(x = rownames(ranefs$ID), y = `(Intercept)`)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Efeito Aleatório") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Para variáveis categóricas
pred_tratamento <- ggpredict(modelo, terms = "tratamentof")

ggplot(pred_tratamento, aes(x = x, y = predicted, color=response.level,fill = response.level)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(0.9), width = 0.25) +
  facet_wrap(~response.level) +
  labs(title = "Probabilidades por Categoria e Tratamento",
       x = "Tratamento", 
       y = "Probabilidade Prevista") +
  theme_minimal()


