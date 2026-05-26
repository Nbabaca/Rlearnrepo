#26/05/2026
#Analysis of variance:cap8/Crawley
#GFP
#___________________________________________

#___________________________________________
#One Way Anova
#___________________________________________
Dado <- read.csv("Dados crawley/oneway.csv")
attach(Dado)
names(Dado)
head(Dado)
plot(1:20,ozone,ylim=c(0,8),ylab="y",xlab="order",pch=21,bg="red")
abline(h=mean(ozone),col="blue")
for(i in 1:20) lines(c(i,i),c(mean(ozone),ozone[i]),col="green")

plot(1:20,ozone,ylim=c(0,8),ylab="y",xlab="order",
     pch=21,bg=as.numeric(garden))
abline(h=mean(ozone[garden=="A"]))
abline(h=mean(ozone[garden=="B"]),col="red") #a anova faz uma comparação das variancias da media, esse teste aponta significancia quando essas medias são variantes o suficiente para não ocorrer aleatoriamente
index <- 1:length(ozone)
for (i in 1:length(index)){
  if (garden[i] == "A")
    lines(c(index[i],index[i]),c(mean(ozone[garden=="A"]),ozone
                                 [i]))
  else
    lines(c(index[i],index[i]),c(mean(ozone[garden=="B"]),ozone
                                 [i]), col="red")}


SSY <-  sum((ozone-mean(ozone))^2)
SSY
sum((ozone[garden=="A"]-mean(ozone[garden=="A"]))^2)
sum((ozone[garden=="B"]-mean(ozone[garden=="B"]))^2)
summary(aov(ozone~garden)) 
par(mfrow=c(1,1))
plot(aov(ozone~garden))

#____________________________________________
#Shortcut Formula
#____________________________________________
cbind(ozone[garden=="A"],ozone[garden=="B"])
tapply(ozone,garden,sum)

#___________________________________________
#Effect Size
#___________________________________________
summary.lm(aov(ozone~garden))

#___________________________________________
#Plots of Interpreting One-way Anova
#___________________________________________
comp <- read.csv("Dados crawley/competition.csv")
attach(comp)
names(comp)
head(comp)
plot(clipping,biomass,ylim = c(0,900),xlab="Competition treatment", ylab = "Biomass",col="lightgrey")
heights <- tapply(biomass,clipping,mean)
barplot(heights,col="green",ylim=c(0,700),
        ylab="mean biomass",xlab="competition treatment")
