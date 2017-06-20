#Top############################################################################
#
# INFNET MIT Big Data - Bloco A - Trabalho de R
# 
# Título do Trabalho : Qualidade do vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Análise descritiva 1
#
#Top############################################################################

# Análise entre variável dependente e independentes
# A T R I B U T O S:
# 01. fixed.acidity        = Acidez fixa
# 02. volatile.acidity     = Acidez volátil
# 03. citric.acid          = Ácido cítrico
# 04. residual.sugar       = Açucar residual
# 05. chlorides            = Cloretos
# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
# 07. total.sulfur.dioxide = Total de dióxido de enxofre
# 08. density              = Densidade
# 09. pH                   = pH             
# 10. sulphates            = Sulfatos
# 11. alcohol              = Teor alcólico
# 12. quality              = Qualidade
# 13. color
# 14. col.color
# 15. taste
# 16. taste.color

library(plyr) #the split-apply-combine paradigm for R
install.packages(dplyr)
library(dplyr)
?plyr

# wine.path       = "/Users/fernandoperes/Google Drive/r-dev/2017-06-Trabalho-R/"
wine.path = "C:/r-wine/"
setwd(wine.path) 

# use wines-util.r resources
source(file =  paste(wine.path, "wines-utils.R", sep = ""))

getwd()

# load prepared data files
load(file="all-wine.Rda")
load(file="red-wine.Rda")
load(file="white-wine.Rda")

# Acidez fixa
# ******************************************************************************
# 1. FIXED ACIDITY -------------------------------------------------------------
# ******************************************************************************
wine.attr2.analysis (x = all.wine$fixed.acidity, xlab = wine.fields.fixed.acidity, 
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste,
                     y2lab = wine.fields.taste,
                     color = wine.color.all)

x = all.wine$fixed.acidity
q1 <- quantile(x, probs=c(.25), na.rm = T)
q3 <- quantile(x, probs=c(.75), na.rm = T)
li <- q1 - 1.5 * IQR(x) # iqr = q3 - q1
ls <- q3 + 1.5 * IQR(x) # iqr = q3 - q1

li
ls

all.wine$outlier = FALSE

all.wine$outlier[all.wine$fixed.acidity < li] <- TRUE
all.wine$outlier[all.wine$fixed.acidity > ls] <- TRUE
all.wine$outlier <- as.factor(all.wine$outlier)

table(all.wine$outlier)


nrow(all.wine)
nrow(all.wine.2)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

xxx <- function(x){
  x = x +1
  x
}

x = 2
x

x = xxx(x)

x


# Acidez volátil
# 2. VOLATILE ACIDITY ----------------------------------------------------------
# The amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste
wine.attr2.analysis (x = all.wine$volatile.acidity, xlab = wine.fields.volatile.acidity,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Ácido cítrico
# 3. CITRIC ACID ---------------------------------------------------------------
# Found in small quantities, citric acid can add ‘freshness’ and flavor to wines
wine.attr2.analysis (x = all.wine$citric.acid, xlab = wine.fields.citric.acid, 
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Açucar residual
# 4. RESIDUAL SUGAR ---------------------------------------------------------------
# The amount of sugar remaining after fermentation stops, it’s rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet
wine.attr2.analysis (x = all.wine$residual.sugar, xlab = wine.fields.residual.sugar,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Cloretos
# 5. CHLORIDES --------------------------------------------------------------------
# The amount of salt in the wine
wine.attr2.analysis (x = all.wine$chlorides, xlab = wine.fields.chlorides,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Livre de dióxido de enxofre
# 6. FREE SULFUR DIOXIDE ----------------------------------------------------------
# The free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine
wine.attr2.analysis (x = all.wine$free.sulfur.dioxide, xlab = wine.fields.free.sulfur.dioxide,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Total de dióxido de enxofre
# 7. TOTAL SULFUR DIOXIDE ---------------------------------------------------------
# Amount of free and bound forms of S02; in low concentrations, SO2 is mostly undetectable in wine, but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine
wine.attr2.analysis (x = all.wine$total.sulfur.dioxide, 
                     y = all.wine$quality,
                     xlab = wine.fields.total.sulfur.dioxide,
                     ylab = wine.fields.quality,
                     y2 = all.wine$taste,
                     y2lab = wine.fields.taste,
                     color = wine.color.all)

# Densidade
# 8. DENSITY -----------------------------------------------------------------------
# Density of water is close to that of water depending on the percent alcohol and sugar content
wine.attr2.analysis (x = all.wine$density, xlab = wine.fields.density,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# pH
# 9. PH ---------------------------------------------------------------------------
# Describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
wine.attr2.analysis (x = all.wine$pH, xlab = wine.fields.pH,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Sulfato
# 10. SULPHATES --------------------------------------------------------------------
# A wine additive which can contribute to sulfur dioxide gas (S02) levels, wich acts as an antimicrobial and antioxidant
wine.attr2.analysis (x = all.wine$sulphates, xlab = wine.fields.sulphates,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)

# Teor Álcólico
# 11. ALCOHOL ----------------------------------------------------------------------
# The percent of alcohol 
wine.attr2.analysis (x = all.wine$alcohol, xlab = wine.fields.alcohol,
                     y = all.wine$quality, ylab = wine.fields.quality,
                     y2 = all.wine$taste, y2lab = wine.fields.taste,
                     color = wine.color.all)









