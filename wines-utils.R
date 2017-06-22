#
# MIT EM BIG DATA INFNET - BLOCO A - TRABALHO FINAL DE R - PROFESSOR CÁSSIUS
# 
# Título do Trabalho : Identificação da Qualidade de vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Utilidades como funções e contantes 
#
#*******************************************************************************


#### C O N S T A N T S ####
# colors() # list all color codes of System R
#*******************************************************************************
#### Language ####
wine.language = "PT-BR"
# wine.language = "EN-US"

#### Colors ####
wine.alpha = 0.5 # level of transparency 

wine.color.white   = adjustcolor("gold", alpha.f = wine.alpha) 
wine.color.red     = adjustcolor("red",  alpha.f = wine.alpha)  
wine.color.all     = adjustcolor("gray", alpha.f = wine.alpha) 

wine.good.color    = adjustcolor("green", alpha.f = wine.alpha)
wine.regular.color = adjustcolor("gold",  alpha.f = wine.alpha)
wine.bad.color     = adjustcolor("red",   alpha.f = wine.alpha)      

#### Taste ####
if (wine.language == "PT-BR"){
  wine.good.label    = "Bom"
  wine.regular.label = "Regular"
  wine.bad.label     = "Ruim"
} else { # If it is not configured the defualt is ENGLISH
  wine.good.label    = "Good"
  wine.regular.label = "Regular"
  wine.bad.label     = "Bad"
  }

#### Wine type (color) ####

if (wine.language == "PT-BR"){
  wine.red.label   = "tinto"
  wine.white.label = "branco"
} else { # If it is not configured the defualt is ENGLISH
  wine.red.label   = "tinto"
  wine.white.label = "branco"
}

#### Fields label constants ####
if (wine.language == "PT-BR"){
  wine.fields.fixed.acidity        = "Acidez fixa"
  wine.fields.volatile.acidity     = "Acidez volatil"
  wine.fields.citric.acid          = "Acido citrico"
  wine.fields.residual.sugar       = "Acucar residual"
  wine.fields.chlorides            = "Cloretos"
  wine.fields.free.sulfur.dioxide  = "Livre de dioxido de enxofre"
  wine.fields.total.sulfur.dioxide = "Total de dioxido de enxofre"
  wine.fields.density              = "Densidade"
  wine.fields.pH                   = "pH"                  
  wine.fields.sulphates            = "Sulfatos"
  wine.fields.alcohol              = "Teor alcolico" 
  wine.fields.quality              = "Qualidade"             
  wine.fields.color                = "Cor"
  wine.fields.col.color            = "Cor para cor do vinho"
  wine.fields.taste                = "Gosto (Conceito)" 
} else { # If it is not configured the defualt is ENGLISH
  wine.fields.fixed.acidity        = "fixed.acidity"       
  wine.fields.volatile.acidity     = "volatile.acidity"    
  wine.fields.citric.acid          = "citric.acid"         
  wine.fields.residual.sugar       = "residual.sugar"      
  wine.fields.chlorides            = "chlorides"           
  wine.fields.free.sulfur.dioxide  = "free.sulfur.dioxide" 
  wine.fields.total.sulfur.dioxide = "total.sulfur.dioxide"
  wine.fields.density              = "density"             
  wine.fields.pH                   = "pH"                  
  wine.fields.sulphates            = "sulphates"           
  wine.fields.alcohol              = "alcohol"             
  wine.fields.quality              = "quality"             
  wine.fields.color                = "color"               
  wine.fields.col.color            = "col.color"           
  wine.fields.taste                = "taste"               
}
#### Fields matrix/vector constants ####
label = c(1:15)
if (wine.language == "PT-BR"){
  label[1] <- "Acidez fixa"
  label[2] <- "Acidez volatil"
  label[3] <- "Acido citrico"
  label[4] <- "Acucar residual"
  label[5] <- "Cloretos"
  label[6] <- "Livre de dioxido de enxofre"
  label[7] <- "Total de dioxido de enxofre"
  label[8] <- "Densidade"
  label[9] <- "pH"                  
  label[10] <- "Sulfatos"
  label[11] <- "Teor alcolico" 
  label[12] <- "Qualidade"             
  label[13] <- "Cor"
  label[14] <- "Cor para cor do vinho"
  label[15] <- "Gosto (Conceito)" 
} else { # If it is not configured the defualt is ENGLISH
  label[1] <- "fixed.acidity"       
  label[2] <- "volatile.acidity"    
  label[3] <- "citric.acid"         
  label[4] <- "residual.sugar"      
  label[5] <- "chlorides"           
  label[6] <- "free.sulfur.dioxide" 
  label[7] <- "total.sulfur.dioxide"
  label[8] <- "density"             
  label[9] <- "pH"                  
  label[10] <-"sulphates"           
  label[11] <-"alcohol"             
  label[12] <-"quality"             
  label[13] <-"color"               
  label[14] <-"col.color"           
  label[15] <-"taste" 
}

name = c(1:15)
name[1] <- "fixed.acidity"       
name[2] <- "volatile.acidity"    
name[3] <- "citric.acid"         
name[4] <- "residual.sugar"      
name[5] <- "chlorides"           
name[6] <- "free.sulfur.dioxide" 
name[7] <- "total.sulfur.dioxide"
name[8] <- "density"             
name[9] <- "pH"                  
name[10] <-"sulphates"           
name[11] <-"alcohol"             
name[12] <-"quality"             
name[13] <-"color"               
name[14] <-"col.color"           
name[15] <-"taste"               

wine.fields <- as.data.frame(label)
wine.fields$name <- name

#### F U N C T I O N S ####

#*******************************************************************************
# #### Plot the field distrution  ####
# Boxplot and a histohram to present the distribution of the current field
#*******************************************************************************
wine.distribution.plot <- function(title, x, field.label, color, xlim, sno.lno, line.color) {
  wine.boxplot(title = title,x = x, xlab = field.label, color =  color, 
               xlim = xlim, sno.lno = sno.lno, line.color = line.color)
  
  wine.histogram(title = title, x = x, xlab = field.label, color = color, 
                 xlim = xlim, sno.lno = sno.lno, line.color = line.color)
}

#*******************************************************************************
# #### Wine boxplot  ####
# Wine Boxplot specially prepared to present the distribution of the current 
# field
#*******************************************************************************
wine.boxplot <- function(title, x, xlab, color, xlim, sno.lno, line.color){
  
  ## Box plot
  boxplot(x = x, xlab = xlab, 
          main = title,
          col = color,
          ylim = xlim, # main = main0,
          cex.axis = 0.75, cex.lab  = 0.75, cex.main = 0.85, 
          horizontal = T, 
          frame = F)
  
  ## Lines
  # smallest non-outlier
  abline(v = sno.lno[1],
         lwd = 2,
         col = line.color)
  
  # median or q2
  abline(v = median(x),
         lwd = 2,
         col = line.color)
  
  # largest non-outlier
  abline(v = sno.lno[2],
         lwd = 2,
         col = line.color)
}

#*******************************************************************************
# #### wine histogram ####
# Wine histogram specially prepared to present the distribution of the current 
# field.
#
# Note: sno.lno = smallest non-outlier . largest non-outlier
#*******************************************************************************
wine.histogram <- function(title, x, xlab, color, xlim, sno.lno, line.color){
  # get highest count of hist breaks (to avoid cut labels when plotted) 
  yhist <-  hist(x, plot = FALSE)
  highestCount <- max(yhist$count) * 1.1
  
  # histogram  
  h <- hist(x = x, xlab = xlab, 
            main = title,
            ylab = "Frequencia",
            col  = color, xlim = xlim, ylim = c(0, highestCount * 1.1),
            cex.main =  0.85, adj = 0, include.lowest = TRUE, cex.axis = 0.75, 
            cex.lab  = 0.75, labels = TRUE)
  
  xfit <- seq(min(x), max(x), length = 40) 
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x) 
  lines(xfit, yfit, col = "blue", lwd = 2)
  
  ## Lines
  
  # smallest non-outlier
  abline(v = sno.lno[1],
         lwd = 2,
         col = line.color)
  
  # median or q2
  abline(v = median(x),
         lwd = 2,
         col = line.color)
  
  # largest non-outlier
  abline(v = sno.lno[2],
         lwd = 2,
         col = line.color)
}

#*******************************************************************************
# #### wine sno.lno ####
# Wine lils() calc the limits non outliers 
# - sno = smallest non-outlier
# - lno = largest non-outlier
#*******************************************************************************
wine.sno.lno<- function(x) {
  q1 <- quantile(x, probs=c(.25), na.rm = T)
  q3 <- quantile(x, probs=c(.75), na.rm = T)
  
  # iqr = q3 - q1
  sno <- q1 - 1.5 * IQR(x) # sno = smallest non-outlier
  if (sno < min(x)) sno = min(x)
  
  lno <- q3 + 1.5 * IQR(x) # lno = largest non-outlie
  if (lno > max(x)) lno = max(x)
  
  sno.lno <- c(sno, lno)
  sno.lno
}

#*******************************************************************************
# #### wine mark outliers ####
# according field limits (sno - lno) the line are tagged as outliers 
# - sno = smallest non-outlier
# - lno = largest non-outlier
#*******************************************************************************
wine.mark.outlier <- function(start, df, field, sno.lno){
  
  # if it is the first time or if it should be reseted, the start is true
  # else start is false, it will continue tagging the outliers
  if (start) df$outlier = FALSE
  
  # mark true for outliers lines
  df$outlier[df[field] < sno.lno[1]] <- TRUE
  df$outlier[df[field] > sno.lno[2]] <- TRUE
  df$outlier <- as.factor(df$outlier)
  
  df$outlier
}

#*******************************************************************************
# #### wine outliers plot ####
#*******************************************************************************
wine.outliers.summary.plot <- function(title, t,  colors){
  # calc the percentuals to be used on labels
  p = 100 * t / sum(t)
  p = round(p, digits = 2)  
  
  # customized label
  labs = c(paste("dados ", p[1], "%", sep = ""), 
           paste("outliers ", p[2], "%", sep = ""))
  
  # pie chart
  pie(x        = p, 
      main     = title,
      labels   = labs,
      col      = colors, 
      cex.axis = 0.75, 
      cex.lab  = 0.75,   
      cex.main = 0.85)
  
  # bar chart
  bp <- barplot( t,
                 main = title,
                 col = colors,
                 cex.axis = 0.75, 
                 cex.lab  = 0.75, 
                 cex.main = 0.85,
                 horiz = TRUE,
                 beside = TRUE)
}