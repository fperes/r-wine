#################################################################################################
#
# MIT EM BIG DATA INFNET - BLOCO A - TRABALHO FINAL DE R - PROFESSOR CÁSSIUS
# 
# Título do Trabalho : Identificação da Qualidade de vinhos
# Fonte de dados     : UCI - base wines
# Autor              : Fernando A J Peres
# Data               : 2017-06-09
# Arquivo            : Utilidades como funções e contantes 
#
#################################################################################################


# Constants
#------------------------------------------------------------------------------
# colors() # list all color codes of System R

wine.alpha = 0.5 # level 

wine.color.white   = adjustcolor("gold", alpha.f = wine.alpha) 
wine.color.red     = adjustcolor("red",  alpha.f = wine.alpha)  
wine.color.all     = adjustcolor("gray", alpha.f = wine.alpha) 

wine.good.color    = adjustcolor("green", alpha.f = wine.alpha)
wine.regular.color = adjustcolor("gold",  alpha.f = wine.alpha)
wine.bad.color     = adjustcolor("red",   alpha.f = wine.alpha)      

wine.good.label    = "Bom"
wine.regular.label = "Regular"
wine.bad.label     = "Ruim"

wine.red.label   = "tinto"
wine.white.label = "branco"

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

label = c(1:15)
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

# Function ----
wine.boxplot <- function(x, xlab, color, xlim, li.ls){
  
  main0 = xlab
  
  boxplot(x = x, xlab = xlab, 
          col = color,
          ylim = xlim, # main = main0,
          cex.axis = 0.75, cex.lab  = 0.75, cex.main = 0.85, 
          horizontal = T, 
          frame = F)
  
  abline(v = li.ls[1],
         lwd = 2,
         col = "red")
  
  text(mean(x),(max(x)), "text", srt=0.2, pos=3)
  
  abline(v = li.ls[2],
         lwd = 2,
         col = "red")
}

wine.lils <- function(x) {
  q1 <- quantile(x, probs=c(.25), na.rm = T)
  q3 <- quantile(x, probs=c(.75), na.rm = T)
  li <- q1 - 1.5 * IQR(x) # iqr = q3 - q1
  ls <- q3 + 1.5 * IQR(x) # iqr = q3 - q1
  
  lils <- c(li, ls)
  lils
}

wine.pie.outliers <- function(t, main){
  p = 100 * t / sum(t)
  p = round(p, digits = 2)  
  
  labs = c(paste("dados ", p[1], "%", sep = ""), 
           paste("outliers ", p[2], "%", sep = ""))
  
  pie(x        = p, 
      labels   = labs, 
      cex.axis = 0.75, cex.lab  = 0.75,   cex.main = 1,
      main     = main,
      col      = c("green", "black"))
  
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

# 
# outliers ---------------------------------------------------------------------
wine.mark.outlier <- function(start = TRUE, df = all.wine, field = "fixed.acidity"){
  
  if (start) df$outlier = FALSE
  
  df$outlier[df[field] < lils[1]] <- TRUE
  df$outlier[df[field] > lils[2]] <- TRUE
  df$outlier <- as.factor(df$outlier)
  
  df$outlier

}
