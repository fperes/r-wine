#
# INFNET MIT Big Data - Bloco A - Trabalho de R
# 
# Título do Trabalho : Qualidade do vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Análise descritiva 1: análise bi-variada
#
#*******************************************************************************

# ******************************************************************************
# #### SETUP ####
# ******************************************************************************
## work directory path ##
wine.path = "/Users/fernandoperes/dev/r/r-wine/" # to be reused as needed
setwd(wine.path) 
## Sources
source(file =  paste(wine.path, "wines-utils.R", sep = ""))
## Libraries
library(dplyr)
library(ggplot2)

# ******************************************************************************
# #### Load prepared data files ####
# ******************************************************************************
load(file="all-wine.Rda") # load(file="red-wine.Rda") # load(file="white-wine.Rda")

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

# ******************************************************************************
# #### 1. Acidez fixa vs * ####
# ******************************************************************************
title = paste(wine.fields.fixed.acidity, "X", wine.fields.volatile.acidity)
ggplot(all.wine, aes(x = fixed.acidity, y = volatile.acidity)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.volatile.acidity, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 02. volatile.acidity     = Acidez volátil
title = paste(wine.fields.fixed.acidity, "X", wine.fields.citric.acid)
ggplot(all.wine, aes(x = fixed.acidity, y = citric.acid)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.fixed.acidity, "X", wine.fields.residual.sugar)
ggplot(all.wine, aes(x = fixed.acidity, y = residual.sugar)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.fixed.acidity, "X", wine.fields.residual.sugar)
ggplot(all.wine, aes(x = fixed.acidity, y = residual.sugar)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 05. chlorides            = Cloretos
title = paste(wine.fields.fixed.acidity, "X", wine.fields.chlorides)
ggplot(all.wine, aes(x = fixed.acidity, y = chlorides)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.fixed.acidity, "X", wine.fields.free.sulfur.dioxide)
ggplot(all.wine, aes(x = fixed.acidity, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.fixed.acidity, "X", wine.fields.total.sulfur.dioxide)
ggplot(all.wine, aes(x = fixed.acidity, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 08. density              = Densidade
title = paste(wine.fields.fixed.acidity, "X", wine.fields.density)
ggplot(all.wine, aes(x = fixed.acidity, y = density)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH
title = paste(wine.fields.fixed.acidity, "X", wine.fields.pH)
ggplot(all.wine, aes(x = fixed.acidity, y = pH)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.fixed.acidity, "X", wine.fields.sulphates)
ggplot(all.wine, aes(x = fixed.acidity, y = sulphates)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.fixed.acidity, "X", wine.fields.alcohol)
ggplot(all.wine, aes(x = fixed.acidity, y = alcohol)) + 
  labs(x = wine.fields.fixed.acidity, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# ******************************************************************************
# 02. volatile.acidity     = Acidez volátil
# ******************************************************************************

# 03. citric.acid          = Ácido cítrico
title = paste(wine.fields.volatile.acidity, "X", wine.fields.citric.acid)
ggplot(all.wine, aes(x = volatile.acidity, y = citric.acid)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.citric.acid, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.volatile.acidity, "X", wine.fields.residual.sugar)
ggplot(all.wine, aes(x = volatile.acidity, y = residual.sugar)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.residual.sugar, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 05. chlorides            = Cloretos
title = paste(wine.fields.volatile.acidity, "X", wine.fields.chlorides)
ggplot(all.wine, aes(x = volatile.acidity, y = chlorides)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.chlorides, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.volatile.acidity, "X", wine.fields.free.sulfur.dioxide)
ggplot(all.wine, aes(x = volatile.acidity, y = free.sulfur.dioxide)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.free.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.volatile.acidity, "X", wine.fields.total.sulfur.dioxide)
ggplot(all.wine, aes(x = volatile.acidity, y = total.sulfur.dioxide)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.total.sulfur.dioxide, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 08. density              = Densidade
title = paste(wine.fields.volatile.acidity, "X", wine.fields.density)
ggplot(all.wine, aes(x = volatile.acidity, y = density)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.density, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH
title = paste(wine.fields.volatile.acidity, "X", wine.fields.pH)
ggplot(all.wine, aes(x = volatile.acidity, y = pH)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.pH, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.volatile.acidity, "X", wine.fields.sulphates)
ggplot(all.wine, aes(x = volatile.acidity, y = sulphates)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.sulphates, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.volatile.acidity, "X", wine.fields.alcohol)
ggplot(all.wine, aes(x = volatile.acidity, y = alcohol)) + 
  labs(x = wine.fields.volatile.acidity, y = wine.fields.alcohol, fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# ******************************************************************************
# 03. citric.acid          = Ácido cítrico
# ******************************************************************************

# 04. residual.sugar       = Açucar residual
title = paste(wine.fields.citric.acid , "X", wine.fields.residual.sugar )
ggplot(all.wine, aes(x = citric.acid , y = residual.sugar )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.residual.sugar , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 05. chlorides            = Cloretos
title = paste(wine.fields.citric.acid , "X", wine.fields.chlorides )
ggplot(all.wine, aes(x = citric.acid , y = chlorides )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.chlorides , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.citric.acid , "X", wine.fields.free.sulfur.dioxide )
ggplot(all.wine, aes(x = citric.acid , y = free.sulfur.dioxide )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.free.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.citric.acid , "X", wine.fields.total.sulfur.dioxide )
ggplot(all.wine, aes(x = citric.acid , y = total.sulfur.dioxide )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.total.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 08. density              = Densidade
title = paste(wine.fields.citric.acid , "X", wine.fields.density )
ggplot(all.wine, aes(x = citric.acid , y = density )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.density , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH             
title = paste(wine.fields.citric.acid , "X", wine.fields.pH )
ggplot(all.wine, aes(x = citric.acid , y = pH )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.pH , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.citric.acid , "X", wine.fields.sulphates )
ggplot(all.wine, aes(x = citric.acid , y = sulphates )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.citric.acid , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = citric.acid , y = alcohol )) + 
  labs(x = wine.fields.citric.acid , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))


# ******************************************************************************
# 04. residual.sugar       = Açucar residual
# ******************************************************************************
# 05. chlorides            = Cloretos
title = paste(wine.fields.residual.sugar , "X", wine.fields.chlorides )
ggplot(all.wine, aes(x = residual.sugar , y = chlorides )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.chlorides , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.residual.sugar , "X", wine.fields.free.sulfur.dioxide )
ggplot(all.wine, aes(x = residual.sugar , y = free.sulfur.dioxide )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.free.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.residual.sugar , "X", wine.fields.total.sulfur.dioxide )
ggplot(all.wine, aes(x = residual.sugar , y = total.sulfur.dioxide )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.total.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 08. density              = Densidade
title = paste(wine.fields.residual.sugar , "X", wine.fields.density)
ggplot(all.wine, aes(x = residual.sugar , y = density )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.density , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH             
title = paste(wine.fields.residual.sugar , "X", wine.fields.pH)
ggplot(all.wine, aes(x = residual.sugar , y = pH )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.pH , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.residual.sugar , "X", wine.fields.sulphates )
ggplot(all.wine, aes(x = residual.sugar , y = sulphates )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.residual.sugar , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = residual.sugar , y = alcohol )) + 
  labs(x = wine.fields.residual.sugar , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))


# ******************************************************************************
# 05. chlorides            = Cloretos
# ******************************************************************************
# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
title = paste(wine.fields.chlorides , "X", wine.fields.free.sulfur.dioxide )
ggplot(all.wine, aes(x = chlorides  , y = free.sulfur.dioxide )) + 
  labs(x = wine.fields.chlorides  , y = wine.fields.free.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.chlorides , "X", wine.fields.total.sulfur.dioxide )
ggplot(all.wine, aes(x = chlorides  , y = total.sulfur.dioxide )) + 
  labs(x = wine.fields.chlorides  , y = wine.fields.total.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 08. density              = Densidade
title = paste(wine.fields.chlorides , "X", wine.fields.density )
ggplot(all.wine, aes(x = chlorides  , y = density)) + 
  labs(x = wine.fields.chlorides  , y = wine.fields.density , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH             
title = paste(wine.fields.chlorides , "X", wine.fields.pH )
ggplot(all.wine, aes(x = chlorides  , y = pH )) + 
  labs(x = wine.fields.chlorides  , y = wine.fields.pH , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.chlorides , "X", wine.fields.sulphates)
ggplot(all.wine, aes(x = chlorides  , y = sulphates)) + 
  labs(x = wine.fields.chlorides  , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.chlorides , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = chlorides  , y = alcohol)) + 
  labs(x = wine.fields.chlorides  , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# ******************************************************************************
# 06. free.sulfur.dioxide  = Livre de dióxido de enxofre
# ******************************************************************************
# 07. total.sulfur.dioxide = Total de dióxido de enxofre
title = paste(wine.fields.free.sulfur.dioxide , "X", wine.fields.total.sulfur.dioxide )
ggplot(all.wine, aes(x = free.sulfur.dioxide , y = total.sulfur.dioxide )) + 
  labs(x = wine.fields.free.sulfur.dioxide , y = wine.fields.total.sulfur.dioxide , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 08. density              = Densidade
title = paste(wine.fields.free.sulfur.dioxide , "X", wine.fields.density )
ggplot(all.wine, aes(x = free.sulfur.dioxide , y = density )) + 
  labs(x = wine.fields.free.sulfur.dioxide , y = wine.fields.density , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH             
title = paste(wine.fields.free.sulfur.dioxide , "X", wine.fields.pH )
ggplot(all.wine, aes(x = free.sulfur.dioxide , y = pH )) + 
  labs(x = wine.fields.free.sulfur.dioxide , y = wine.fields.pH , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.free.sulfur.dioxide , "X", wine.fields.sulphates )
ggplot(all.wine, aes(x = free.sulfur.dioxide , y = sulphates )) + 
  labs(x = wine.fields.free.sulfur.dioxide , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.free.sulfur.dioxide , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = free.sulfur.dioxide , y = alcohol )) + 
  labs(x = wine.fields.free.sulfur.dioxide , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))


# ******************************************************************************
# 07. total.sulfur.dioxide = Total de dióxido de enxofre
# ******************************************************************************
# 08. density              = Densidade
title = paste(wine.fields.total.sulfur.dioxide , "X", wine.fields.density )
ggplot(all.wine, aes(x = total.sulfur.dioxide , y = density )) + 
  labs(x = wine.fields.total.sulfur.dioxide , y = wine.fields.density , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 09. pH                   = pH
title = paste(wine.fields.total.sulfur.dioxide , "X", wine.fields.pH )
ggplot(all.wine, aes(x = total.sulfur.dioxide , y = pH )) + 
  labs(x = wine.fields.total.sulfur.dioxide , y = wine.fields.pH , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.total.sulfur.dioxide , "X", wine.fields.sulphates )
ggplot(all.wine, aes(x = total.sulfur.dioxide , y = sulphates )) + 
  labs(x = wine.fields.total.sulfur.dioxide , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.total.sulfur.dioxide , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = total.sulfur.dioxide , y = alcohol )) + 
  labs(x = wine.fields.total.sulfur.dioxide , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))


# ******************************************************************************
# 08. density              = Densidade
# ******************************************************************************
# 09. pH                   = pH
title = paste(wine.fields.density , "X", wine.fields.pH )
ggplot(all.wine, aes(x = density  , y = pH )) + 
  labs(x = wine.fields.density  , y = wine.fields.pH , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 10. sulphates            = Sulfatos
title = paste(wine.fields.density , "X", wine.fields.sulphates)
ggplot(all.wine, aes(x = density  , y = sulphates)) + 
  labs(x = wine.fields.density  , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.density , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = density  , y = alcohol)) + 
  labs(x = wine.fields.density  , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# ******************************************************************************
# 09. pH                   = pH   
# ******************************************************************************
# 10. sulphates            = Sulfatos
title = paste(wine.fields.pH , "X", wine.fields.sulphates )
ggplot(all.wine, aes(x = pH  , y = sulphates )) + 
  labs(x = wine.fields.pH  , y = wine.fields.sulphates , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# 11. alcohol              = Teor alcólico
title = paste(wine.fields.pH , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = pH  , y = alcohol )) + 
  labs(x = wine.fields.pH  , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

# ******************************************************************************
# 10. sulphates            = Sulfatos
# ******************************************************************************
# 11. alcohol              = Teor alcólico
title = paste(wine.fields.sulphates , "X", wine.fields.alcohol )
ggplot(all.wine, aes(x = sulphates  , y = alcohol )) + 
  labs(x = wine.fields.sulphates  , y = wine.fields.alcohol , fill = "Gosto") +
  geom_point(shape = 19, aes(color = factor(all.wine$taste))) +
  ggtitle(title) + 
  scale_color_manual("", 
                     values = c("green", "gold", "red"),
                     breaks = c("bom", "regular", "ruim"))

