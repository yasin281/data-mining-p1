# ------------------------------
# Cargar y preprocesar los datos
# ------------------------------
setwd("C:/Users/usuario/Documents/uni_upc/ultimoaño/MD/scripts_r")

dd <- read.csv("filtered_data.csv", sep = ",", stringsAsFactors = TRUE)

# Definir las variables de interés
columnas_interes <- c("Month", "Year", "CustAge", "CustGender", 
                      "Country", "ProdCat", "OrdQty", 
                      "UnitCost", "UnitPrice", "Size", "Color", 
                      "Warranty", "ShipWeight", "DelTime", 
                      "Discount", "EcoFriendly", "ShipCost", "Insurance")
vars_categoricas <- c("Year", "Month", "CustGender", "Country", "ProdCat",
                      "Size", "Color", "Warranty", "EcoFriendly", "Insurance")

# Convertir variables a sus respectivos tipos
dd[vars_categoricas] <- lapply(dd[vars_categoricas], as.factor)
vars_numericas <- setdiff(columnas_interes, vars_categoricas)
dd[vars_numericas] <- lapply(dd[vars_numericas], as.numeric)
dcon <- data.frame(dd[vars_numericas])  # Solo las variables numéricas

# -------------------------------------------------------
# Dendrograma 1: Euclidean Distance with Ward’s Method
# -------------------------------------------------------
d_euclidean <- dist(dcon)  # Calcula la matriz de distancias Euclidianas
h_euclidean <- hclust(d_euclidean, method = "ward.D2")  # Clustering jerárquico con Ward
plot(h_euclidean, main = "1. Euclidean ward")

# ---------------------------------------------------------
# Dendrograma 2: Gower Dissimilarity (Squared) with Ward’s Method
# ---------------------------------------------------------
library(cluster)
# 'actives' define las columnas a usar (se incluyen tanto numéricas como categóricas)
actives <- c(1:16)
dissimMatrix <- daisy(dd[, actives], metric = "gower", stand = TRUE)  # Calcula la disimilitud con Gower
distMatrix <- dissimMatrix^2  # Eleva al cuadrado para adecuarlo a Ward’s method
h_gower <- hclust(distMatrix, method = "ward.D2")  # Clustering jerárquico con Ward
plot(h_gower, main = "2. Gower ward")

# ---------------------------------------------------------
# Creación de objetos de clusters para profiling:
# Se crea 'c1' a partir del dendrograma Euclidean y 'c2' a partir del Gower
# Ajusta k según lo que resulte más adecuado en tu análisis.
# Por ejemplo, cortamos el dendrograma Euclidean en 2 clusters y el Gower en 4 clusters.
#c1 <- cutree(h_euclidean, k = 2)
c1 <- cutree(h_gower, k = 4)

##PROFILING##

# Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  nk <- as.vector(table(P))
  n <- sum(nk)
  xk <- tapply(Xnum,P,mean)
  txk <- (xk - mean(Xnum)) / (sd(Xnum) * sqrt((n - nk)/(n * nk)))
  pxk <- pt(txk, n - 1, lower.tail = FALSE)
  for(c in 1:length(levels(as.factor(P)))){
    if(pxk[c] > 0.5){ 
      pxk[c] <- 1 - pxk[c]
    }
  }
  return(pxk)
}

ValorTestXquali <- function(P,Xquali){
  taula <- table(P, Xquali)
  n <- sum(taula)
  pk <- apply(taula, 1, sum) / n
  pj <- apply(taula, 2, sum) / n
  pf <- taula / (n * pk)
  pjm <- matrix(data = pj, nrow = dim(pf)[1], ncol = dim(pf)[2], byrow = TRUE)
  dpf <- pf - pjm
  dvt <- sqrt(((1 - pk)/(n * pk)) %*% t(pj * (1 - pj)))
  zkj <- dpf
  zkj[dpf != 0] <- dpf[dpf != 0] / dvt[dpf != 0]
  pzkj <- pnorm(zkj, lower.tail = FALSE)
  for(c in 1:length(levels(as.factor(P)))){
    for(s in 1:length(levels(Xquali))){
      if(pzkj[c, s] > 0.5){ pzkj[c, s] <- 1 - pzkj[c, s] }
    }
  }
  return(list(rowpf = pf, vtest = zkj, pval = pzkj))
}

# P contains class variable (which cluster an individual belongs to)
P <- c1
nameP <- "classe"

nc <- length(levels(factor(P)))
nc
K <- 18
pvalk <- matrix(data = 0, nrow = nc, ncol = K, dimnames = list(levels(P), names(dd)))
nameP <- "Class"
n <- dim(dd)[1]

if(!dir.exists("profilingImages")) dir.create("profilingImages")
setwd("C:/Users/usuario/Documents/uni_upc/ultimoaño/MD/scripts_r/profilingImages")

for(k in 1:K){
  if(is.numeric(dd[, k])){
    print(paste("Anàlisi per classes de la Variable:", names(dd)[k]))
    
    # Guardar Boxplot
    png(paste("Boxplot", names(dd)[k], "Vs", nameP, ".png", sep = ""))
    boxplot(dd[, k] ~ P, 
            main = paste("Boxplot of", names(dd)[k], "vs", nameP),
            horizontal = TRUE, xlab = names(dd)[k], ylab = nameP)
    dev.off()
    
    # Guardar Barplot de Medias
    png(paste("Means", names(dd)[k], "By", nameP, ".png", sep = ""))
    barplot(tapply(dd[[k]], P, mean), 
            main = paste("Means of", names(dd)[k], "by", nameP))
    dev.off()
    
    abline(h = mean(dd[[k]]))
    legend(0, mean(dd[[k]]), "global mean", bty = "n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {
      print(summary(dd[P == s, k]))
    }
    o <- oneway.test(dd[, k] ~ P)
    print(paste("p-valueANOVA:", o$p.value))
    kw <- kruskal.test(dd[, k] ~ P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[, k] <- ValorTestXnum(dd[, k], P)
    print("p-values ValorsTest: ")
    print(pvalk[, k])
    
  } else {
    if(class(dd[, k]) == "Date"){
      print(summary(dd[, k]))
      print(sd(dd[, k]))
      png(paste("Hist_Date_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      hist(dd[, k], breaks = "weeks")
      dev.off()
    } else {
      # Variables cualitatives
      print(paste("Variable", names(dd)[k]))
      tbl <- table(P, dd[, k])
      rowperc <- prop.table(tbl, 1)
      colperc <- prop.table(tbl, 2)
      
      dd[, k] <- as.factor(dd[, k])
      
      marg <- table(as.factor(P)) / n
      print(append("Categories=", levels(as.factor(dd[, k]))))
      
      # Plot 1: Líneas de colperc
      png(paste("Prop_lines_colperc_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      plot(marg, type = "l", ylim = c(0, 1), main = paste("Prop. of pos & neg by", names(dd)[k]))
      paleta <- rainbow(length(levels(dd[, k])))
      for(c in 1:length(levels(dd[, k]))){
        lines(colperc[, c], col = paleta[c])
      }
      dev.off()
      
      # Plot 2: Líneas de colperc con leyenda
      png(paste("Prop_lines_colperc_leg_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      plot(marg, type = "l", ylim = c(0, 1), main = paste("Prop. of pos & neg by", names(dd)[k]))
      paleta <- rainbow(length(levels(dd[, k])))
      for(c in 1:length(levels(dd[, k]))){
        lines(colperc[, c], col = paleta[c])
      }
      legend("topright", levels(dd[, k]), col = paleta, lty = 2, cex = 0.6)
      dev.off()
      
      # Plot 3: Líneas de rowperc
      png(paste("Prop_lines_rowperc_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      plot(marg, type = "n", ylim = c(0, 1), main = paste("Prop. of pos & neg by", names(dd)[k]))
      paleta <- rainbow(length(levels(dd[, k])))
      for(c in 1:length(levels(dd[, k]))){
        lines(rowperc[, c], col = paleta[c])
      }
      dev.off()
      
      # Plot 4: Líneas de rowperc con leyenda
      png(paste("Prop_lines_rowperc_leg_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      plot(marg, type = "n", ylim = c(0, 1), main = paste("Prop. of pos & neg by", names(dd)[k]))
      paleta <- rainbow(length(levels(dd[, k])))
      for(c in 1:length(levels(dd[, k]))){
        lines(rowperc[, c], col = paleta[c])
      }
      legend("topright", levels(dd[, k]), col = paleta, lty = 2, cex = 0.6)
      dev.off()
      
      # Plot 5: Barplot apilat
      png(paste("Barplot_stacked_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      paleta <- rainbow(length(levels(dd[, k])))
      barplot(table(dd[, k], as.factor(P)), beside = FALSE, col = paleta)
      legend("topright", levels(as.factor(dd[, k])), pch = 1, cex = 0.5, col = paleta)
      dev.off()
      
      # Plot 6: Barplot adosat
      png(paste("Barplot_sidebyside_", names(dd)[k], ".png", sep = ""), width = 800, height = 600)
      paleta <- rainbow(length(levels(dd[, k])))
      barplot(table(dd[, k], as.factor(P)), beside = TRUE, col = paleta)
      legend("topright", levels(as.factor(dd[, k])), pch = 1, cex = 0.5, col = paleta)
      dev.off()
      
      print("Test Chi quadrat: ")
      print(chisq.test(dd[, k], as.factor(P)))
      
      print("valorsTest:")
      print(ValorTestXquali(P, dd[, k]))
    }
  }
}#endfor

for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:", levels(as.factor(P))[c]))
    print(sort(pvalk[c, ]), digits = 3)
  }
}

# Afegeix el snake plot (nou)
snake_plot <- function(data, clusters, file_name) {
  means <- aggregate(data, by = list(cluster = clusters), FUN = mean)
  rownames(means) <- means[, 1]
  means <- means[ , -1]
  s_means <- scale(means)
  png(file_name, width = 900, height = 600)
  matplot(t(s_means), type = "l", lty = 1, lwd = 2,
          xlab = "Variables", ylab = "Standardized Mean",
          main = "Snake Plot of Standardized Means by Cluster")
  legend("topright", legend = rownames(s_means), col = 1:nrow(s_means),
         lty = 1, lwd = 2)
  dev.off()
}

snake_plot(dcon, P, "SnakePlot.png")
