library(dplyr)
library(ggpubr)
conc <- c(0.0002, 0.0004, 0.0008, 0.0016, 0.002)
abs <- c(0.088, 0.188, 0.369, 0.682, 0.852)
onda <- c(400, 425, 450, 475, 500, 525, 550, 575, 600)
abs2 <- c(0.097, 0.083, 0.081, 0.097, 0.141, 0.189, 0.168, 0.101, 0.054)


df <- data.frame(conc, abs)


ggplot(df, aes(x=conc,y=abs)) +
  geom_point() +
  geom_smooth(method = "lm", se=F)+
  stat_cor(method = "pearson",
           show.legend = FALSE,
           cor.coef.name = "r",
           label.sep = "\n",
           label.x = 0.00020,
           label.y = 0.62,
           size = 7) +
  theme_bw()
  
  
 

m1 <- lm(conc~abs,df)
summary(m1)

, abs2, onda