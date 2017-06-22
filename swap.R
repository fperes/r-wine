
# calc li and ls
q1 <- quantile(x, probs=c(.25), na.rm = T)
q3 <- quantile(x, probs=c(.75), na.rm = T)
li <- q1 - 1.5 * IQR(x) # iqr = q3 - q1
ls <- q3 + 1.5 * IQR(x) # iqr = q3 - q1

lils <- c(li, ls)
lils

table(all.wine$outlier)

# mark outliers
start = TRUE
df = all.wine
field = field.name

if (start) df$outlier = FALSE

df$outlier[df[field] < lils[1]] <- TRUE
df$outlier[df[field] > lils[2]] <- TRUE
df$outlier <- as.factor(df$outlier)

all.wine$outlier <- df$outlier

#*******************************************************************************
# #### wine outliers pie plot ####
#*******************************************************************************

wine.pie.outliers <- function(t, main){
  
  
}

wine.attr2.analysis <- function(x, xlab, y, ylab, y2, y2lab, color){
  
  par.customized <- par(mfrow=c(1,2)) 
  
  main0 = xlab
  main1 = paste(ylab, "X", xlab)
  main2 = paste(y2lab, "X", xlab)
  
  boxplot(x = x, xlab = xlab, 
          col = color,
          main = main0,
          cex.axis = 0.75, cex.lab  = 0.75, cex.main = 0.85, 
          horizontal = T, 
          frame = F)
  
  abline(v = mean(x),
         lwd = 2,
         col = "red")
  
  abline(v = median(x),
         lwd = 2,
         col = "dark red")
  
  
  h <- hist(x = x, xlab = xlab, 
            main = main0, adj = 0,
            ylab = "Frequencia",
            col  = color,
            include.lowest = TRUE,
            cex.axis = 0.75, cex.lab  = 0.75, cex.main = 0.85,
            labels = TRUE,
            xlim = c(min(x),max(x) * 1.1))
  
  # curva normal
  xfit <- seq(min(x), max(x), length = 40) 
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x) 
  lines(xfit, yfit, col = "blue", lwd = 2)
  
  abline(v = mean(x),
         lwd = 2,
         col = "red")
  
  abline(v = median(x),
         lwd = 2,
         col = "dark red")
  
  boxplot(x ~ y, 
          xlab = ylab, 
          ylab = xlab,
          main = main1,
          varwidth = TRUE,
          staplewex = 2,
          col = wine.color.all,
          cex.axis = 0.75, cex.lab  = 0.75, cex.main = 0.85,
          labels = TRUE, frame = FALSE,
          ylim = c(min(x),max(x) * 1.1))
  
  text(y = boxplot.stats(x)$stats, 
       labels = boxplot.stats(x)$stats, 
       x = 8)
  
  boxplot(x ~ y2, 
          xlab = y2lab, 
          ylab = xlab,
          main = main2,
          varwidth = TRUE,
          staplewex = 2,
          col = wine.color.all,
          cex.axis = 0.75, cex.lab  = 0.75, cex.main = 0.85,
          levels(c("Ruim", "Regular", "Bom")),
          labels = TRUE, frame = FALSE,
          ylim = c(min(x),max(x) * 1.1))
  
  text(y = boxplot.stats(x)$stats, 
       labels = boxplot.stats(x)$stats, 
       x = 8)
  
}










# wines$quality
# Quality: output variable (based on sensory data, score between 0 and 10)
#------------------------------------------------------------------------------

tb <- quantile(all.wine$quality)
tb <- round(t, digits = 2)
tb.h <- c("0%", "25%", "50%", "75%", "100%")

grid.table(tb, tb.h)
grid.table(quantile(white.wine$quality))


?par
par.customized <- par(mfrow=c(1,2))


boxplot(all.wine$quality, 
        xlab = "xx", 
        ylab = "Acidez",
        main = "[Box plot] Vinhos: qualidade x acidez",
        varwidth = TRUE,
        staplewex = 2,
        labels = TRUE)

h <- hist(all.wine$quality, 
          main = "Qualidade do Vinho", 
          xlab = "Qualidade", 
          ylab = "Frequência",
          col  = "peru",
          include.lowest = TRUE,
          labels = TRUE) #prob = TRUE)

colors

# curva normal
xfit <- seq(min(all.wine$quality), max(all.wine$quality), length = 40) 
yfit <- dnorm(xfit, mean = mean(all.wine$quality), sd = sd(all.wine$quality))
yfit <- yfit * diff(h$mids[1:2]) * length(all.wine$quality) 
lines(xfit, yfit, col="blue", lwd=2)

abline(v = mean(all.wine$quality),
       lwd = 1,
       col = "chocolate4")

abline(v = median(all.wine$quality),
       lwd = 3,
       col = "blue")

abline(v = max(all.wine$quality),
       lwd = 1,
       col = "red")

abline(v = min(all.wine$quality),
       lwd = 1,
       col = "red")

abline(v = quantile(all.wine$quality),
       lwd = 1,
       col = "chocolate3")

hist(red.wine$quality, 
     main = "Qualidade do Vinho - Tinto",  
     xlab = "Qualidade", 
     ylab = "Frequência")

hist(white.wine$quality, 
     main = "Qualidade do Vinho - Branco", 
     xlab = "Qualidade", 
     ylab = "Frequência")



hist(x=0, plot = FALSE)
legend( x = "topright", c("Mean", "Median", "Max", "Min"), 
        col = c("chocolate3", "royalblue", "red", "chocolate3", "chocolate3"),
        lwd = c(2, 2, 2, 2, 2))

summary(all.wine$quality)

# wines$taste
# Taste: categorical variable based on wine$quality
#------------------------------------------------------------------------------

all.wine.tastes.summary = table(all.wine$taste)
all.wine.tastes.percent = 100 * all.wine.tastes.summary / sum(all.wine.tastes.summary)
all.wine.tastes.percent = round(all.wine.tastes.percent, digits = 2)
all.wine.tastes.summary

red.wine.tastes.summary = table(red.wine$taste)
red.wine.tastes.percent = 100 * red.wine.tastes.summary / sum(red.wine.tastes.summary)
red.wine.tastes.percent = round(red.wine.tastes.percent, digits = 2)
red.wine.tastes.summary

white.wine.tastes.summary = table(white.wine$taste)
white.wine.tastes.percent = 100 * white.wine.tastes.summary / sum(white.wine.tastes.summary)
white.wine.tastes.percent = round(white.wine.tastes.percent, digits = 2)
white.wine.tastes.summary

all.labs   = c(paste("Bom - ", all.wine.tastes.percent[1], "%"), 
               paste("Regular - ", all.wine.tastes.percent[2], "%"), 
               paste("Ruim - ", all.wine.tastes.percent[3], "%"))

red.labs   = c(paste("Bom - ", red.wine.tastes.percent[1], "%"), 
               paste("Regular - ", red.wine.tastes.percent[2], "%"), 
               paste("Ruim - ", red.wine.tastes.percent[3], "%"))

white.labs   = c(paste("Bom - ", white.wine.tastes.percent[1], "%"), 
                 paste("Regular - ", white.wine.tastes.percent[2], "%"), 
                 paste("Ruim - ", white.wine.tastes.percent[3], "%"))

par.threeColumn <- par(mfrow=c(1,3))

pie(x = all.wine.tastes.percent, 
    labels = all.labs, 
    main = "Qualidade do Vinho (gosto)")

pie(x = red.wine.tastes.percent, 
    labels = red.labs, 
    main ="Qualidade do Vinho - Tinto (gosto)")

pie(x = white.wine.tastes.percent, 
    labels = white.labs, 
    main = "Qualidade do Vinho - Branco (gosto)")


# Avaliar wine$taste versus 


plot.wines.fields <- function(field.independent, field.dependent) {
  plot(x = all.wine[field.dependent], xlab = "Qualidade",  xlim = c(2,10),
       y = all.wine[field.independent], ylab = "Acidez", ylim = c(0,2),
       main  = "Vinhos", col.main = "red", cex.main = 1, adj = 0,
       sub   = "Acidez versus Qualidade", col.sub = "blue", cex.sub = 0.75, 
       pch   = 19,
       col   = 1,
       las   = 1,
       cex.axis = 0.75, cex.lab  = 0.75, 
       bty   = "n")
  
}

plot.wines.fields("citric.acid", "taste")

plot(x = all.wine$taste, xlab = "Qualidade",  xlim = c(2,10),
     y = all.wine$citric.acid, ylab = "Acidez", ylim = c(0,2),
     main  = "Vinhos", col.main = "red", cex.main = 1, adj = 0,
     sub   = "Acidez versus Qualidade", col.sub = "blue", cex.sub = 0.75, 
     pch   = 19,
     col   = 1,
     las   = 1,
     cex.axis = 0.75, cex.lab  = 0.75, 
     bty   = "n")

plot(x = all.wine$quality, xlab = "Qualidade",  xlim = c(2,10),
     y = all.wine$citric.acid, ylab = "Acidez", ylim = c(0,2),
     main  = "Vinhos", col.main = "red", cex.main = 1, adj = 0,
     sub   = "Acidez versus Qualidade", col.sub = "blue", cex.sub = 0.75, 
     pch   = 19,
     col   = 1,
     las   = 1,
     cex.axis = 0.75, cex.lab  = 0.75, 
     bty   = "n")



boxplot(wines$citric.acid,
        outline = TRUE, staplewex = TRUE, add  = TRUE, at = 1
)



boxplot(all.wine$citric.acid ~ all.wine$taste, 
        xlab = "Qualidade", 
        ylab = "Acidez",
        main = "[Box plot] Vinhos: qualidade x acidez",
        varwidth = TRUE,
        staplewex = 2,
        labels = TRUE)

text(y = boxplot.stats(wines$citric.acid)$stats, 
     labels = boxplot.stats(wines$citric.acid)$stats, 
     x = 8)


quality.freq = table(wines$quality)
pie(quality.freq)

hist(wines$quality)

par.threeColumn <- par(mfrow=c(1,3), family ="Century Gothic")

plot(x = wines$quality, xlab = "Qualidade", xlim = 0:10,
     y = wines$citric.acid, ylab = "Acidez", ylim = 0:1.6,
     main = "Acidez versus Qualidade", 
     pch  = 19,
     col  = wines$col.color)

plot(x = red.wine$quality, xlab = "Qualidade", 
     y = red.wine$citric.acid, ylab = "Acidez", 
     main = "Vinho tinto: Acidez versus Qualidade", 
     pch  = 19,
     col  = red.wine$col.color)

plot(x = white.wine$quality, xlab = "Qualidade", 
     y = white.wine$citric.acid, ylab = "Acidez", 
     main = "Vinho branco: Acidez versus Qualidade", 
     pch  = 19,
     col  = white.wine$col.color)

par(par.threeColumn)

abline(lm(wines$citric.acid ~ wines$quality))

library(MASS)


# Fixed acidity: most acids involved with wine or fixed or nonvolatile 
# (do not evaporate readily)
# Acidez fixa: a maioria dos ácidos envolvidos com vinho ou fixo ou
# não volátil (não se evapore prontamente)
#------------------------------------------------------------------------------

hist(wines$fixed.acidity, col = "blue", breaks = 5)
summary(wines$fixed.acidity)
typeof(wines$fixed.acidity)

#Volatile acidity: the amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste
#Citric acid: found in small quantities, citric acid can add ‘freshness’ and flavor to wines
#Residual sugar: the amount of sugar remaining after fermentation stops, it’s rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet
#Chlorides: the amount of salt in the wine
#Free sulfur dioxide: the free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine
#Total sulfur dioxide: amount of free and bound forms of S02; in low concentrations, SO2 is mostly undetectable in wine, but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine
#Density: the density of water is close to that of water depending on the percent alcohol and sugar content
#pH: describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
#Sulphates: a wine additive which can contribute to sulfur dioxide gas (S02) levels, wich acts as an antimicrobial and antioxidant
#Alcohol: the percent alcohol content of the 


install.packages("locfit")
library("locfit")

plotbyfactor(x = all.wine$fixed.acidity, y = all.wine$pH, 
             f = all.wine$taste, data = all.wine,
             col = all.wine$taste.color )




a = c("bom", "bom", "regular", "regular", "regular", "ruim", "ruim", "ruim", "ruim", "ruim")
x = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4)
y = c(1:10)
b = c("green", "green", "yellow", "yellow", "yellow", "red", "red", "red", "red", "red")


d <- data.frame(a)
d$b <- b
d$x <- x
d$y <- y

d

plot(x = d$x, y = d$y, 
     xlab = "x", ylab = "y",
     main = "Gosto",
     col = d$b,
     pch = 20 , frame.plot = F)



install.packages(gridExtra)
library(gridExtra)

library(grid)
library(ggplot2)
library(lattice)


scp = ggplot(all.wine, aes(x = fixed.acidity, y = pH)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19)
scp

scp1 = scp + geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle("Pelo gosto do vinho") + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))
scp1


scp2 = scp + geom_point(shape = 19, aes(color = factor(all.wine$color))) +
  ggtitle("Pela cor do vinho") + 
  scale_color_manual("", 
                     values = c("red", "gold"),
                     breaks = c("red", "white"))
scp2




par.customized <- par(mfrow=c(3,1)) 

x = all.wine$fixed.acidity

wine.boxplot(x, wine.fields.fixed.acidity, color =  wine.color.all, 
             xlim = c(min(x), max(x)), li.ls = wine.lils(x))

all.wine$outlier
all.wine$outlier = wine.mark.outlier(start = TRUE, df = all.wine, field = "fixed.acidity")
all.wine$outlier

field = "fixed.acidity"
df = all.wine

df$outlier = FALSE
df$outlier[df[field] < lils[1]] <- TRUE
df$outlier[df[field] > lils[2]] <- TRUE
df$outlier <- as.factor(df$outlier)
df

df$outlier
all.wine$outlier

all.wine$outlier <- df$outlier
all.wine$outlier

x = subset(all.wine$fixed.acidity,  all.wine$outlier == FALSE)
wine.boxplot(x, wine.fields.fixed.acidity, color =  wine.color.all, xlim, 
             li.ls = c(min(x), max(x)))

wine.pie.outliers(table(all.wine$outlier), main = "")


max(x)
min(x)


#resumo 2
#all.wine[,c("fixed.acidity","pH", "taste", "taste.color", "color", "col.color")]
#
#par.customized <- par(mfrow=c(1,2))
#
#plot(x = all.wine$fixed.acidity, y = all.wine$pH, 
#     col = all.wine$taste.color,  
#     xlab = wine.fields.fixed.acidity, ylab = wine.fields.pH,
#     main = "Gosto",
#     pch = 20 , frame.plot = F)
#
#legend(x = "topright", 
#       legend = unique(all.wine$taste), 
#       col    = unique(all.wine$taste.color),
#       pch = 16
#       #  c(wine.good.color, wine.regular.color, wine.bad.color), pch = 1
#)