#################################################################################################
#
# Trabalho Final de R - InfNet
# Identificação da Qualidade de vinhos
# Juntar arquivos e salvar a base de dados unificada.
# Fernando A J Peres
#
#################################################################################################

# Set work directory
getwd()
setwd("/Users/fernandoperes/Google Drive/data/wine-quality/")

# Open data sets
#------------------------------------------------------------------------------
white.wine = read.csv("winequality-white.csv", sep = ";", header = TRUE)
red.wine   = read.csv("winequality-red.csv", sep = ";", header = TRUE)

names(white.wine)
names(red.wine)

# Add wine color column, before merge tables white_wine and red_wine
#------------------------------------------------------------------------------
red.wine$color       = "red"
red.wine$col.color   = "red" 
white.wine$color     = "white"
white.wine$col.color = "orange" 


# merge white_wine and red_wine tables
#------------------------------------------------------------------------------
wines <- rbind(white.wine, red.wine)
save(wines, file="wines.Rda")

load(file="wines.Rda")
names(wines)

# wines$quality
# Quality: output variable (based on sensory data, score between 0 and 10)
summary(wines$quality)
hist(wines$quality, col = "blue", breaks = 5)
typeof(wines$quality)

boxplot(wines$citric.acid ~ wines$quality, varwidth = TRUE,
        xlab = "Qualidade", 
        ylab = "Acidez")


plot(x = wines$quality, xlab = "Qualidade", 
     y = wines$citric.acid, ylab = "Acidez", 
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

abline(lm(wines$citric.acid ~ wines$quality))

library(MASS)


boxplot(wines ~ )
# fixed.acidity
# Fixed acidity: most acids involved with wine or fixed or 
# nonvolatile (do not evaporate readily)

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
#Alcohol: the percent alcohol content of the wine





