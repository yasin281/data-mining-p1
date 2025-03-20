###############################################################################
# SCRIPT DE CLUSTERING (GOWER + WARD) + PROFILING (4 CLUSTERS)
###############################################################################

# ------------------------------
# 1. CONFIGURACIÓN INICIAL
# ------------------------------
setwd("C:/Users/usuario/Documents/uni_upc/ultimoaño/MD/scripts_r")

# Cargar librerías necesarias
library(cluster)   # Para daisy(), silhouette, etc.
library(fpc)       # Para calinhara(), si la usas
library(dplyr)     # Para manipulación de datos (opcional)

# Cargar datos
dd <- read.csv("filtered_data.csv", sep = ",", stringsAsFactors = TRUE)

# Definir las variables de interés
columnas_interes <- c("Month", "Year", "CustAge", "CustGender", 
                      "Country", "ProdCat", "OrdQty", 
                      "UnitCost", "UnitPrice", "Size", "Color", 
                      "Warranty", "ShipWeight", "DelTime", 
                      "Discount", "EcoFriendly", "ShipCost", "Insurance")
vars_categoricas <- c("Year", "Month", "CustGender", "Country", "ProdCat",
                      "Size", "Color", "Warranty", "EcoFriendly", "Insurance")

# Convertir variables
dd[vars_categoricas] <- lapply(dd[vars_categoricas], as.factor)
vars_numericas <- setdiff(columnas_interes, vars_categoricas)
dd[vars_numericas] <- lapply(dd[vars_numericas], as.numeric)

# Data frame solo con numéricas (para algunos análisis)
dcon <- dd[vars_numericas]

# ------------------------------
# 2. CLUSTERING: GOWER + WARD
# ------------------------------
dissimMatrix <- daisy(dd, metric = "gower", stand = TRUE)
distMatrix <- dissimMatrix^2  
h_gower <- hclust(distMatrix, method = "ward.D2")

# Graficar el dendrograma de Gower
plot(h_gower, main = "Gower ward dendrogram")

# Elegir número de clusters = 4
k_final <- 4
P <- cutree(h_gower, k_final)  # P es la variable de clase (clusters)

# Crear carpeta para guardar gráficos
if(!dir.exists("profilingImages")) dir.create("profilingImages")
outPath <- file.path(getwd(), "profilingImages")

# ------------------------------
# 3. FUNCIONES AUXILIARES (opcionales)
# ------------------------------

# Ejemplo de Snake Plot (opcional)
snake_plot <- function(data, clusters, file_name) {
  means <- aggregate(data, by = list(cluster = clusters), FUN = mean)
  rownames(means) <- means[,1]
  means <- means[,-1]
  s_means <- scale(means)  # Estandarizar
  png(file_name, width = 900, height = 600)
  matplot(t(s_means), type = "l", lty = 1, lwd = 2,
          xlab = "Variables", ylab = "Standardized Mean",
          main = "Snake Plot of Standardized Means by Cluster")
  legend("topright", legend = rownames(s_means), col = 1:nrow(s_means),
         lty = 1, lwd = 2)
  dev.off()
}

# ------------------------------
# 4. PROFILING
# ------------------------------

# 4.1 "¿Son todas las variables importantes?"
# - Numéricas: ANOVA + Kruskal-Wallis
# - Categóricas: Chi-cuadrado
for (varName in colnames(dd)) {
  
  png_file <- file.path(outPath, paste0("Profiling_", varName, ".png"))
  
  if(is.numeric(dd[[varName]])) {
    # ANOVA y Kruskal-Wallis
    anova_res <- oneway.test(dd[[varName]] ~ P)  # ANOVA
    kw_res <- kruskal.test(dd[[varName]] ~ P)    # Kruskal-Wallis
    
    cat("\n-------------------------------------------\n")
    cat("Variable numérica:", varName, "\n")
    cat("ANOVA p-value:", anova_res$p.value, "\n")
    cat("Kruskal-Wallis p-value:", kw_res$p.value, "\n")
    
    # Gráficos: Boxplot + Histograma global
    png(png_file, width = 1000, height = 700)
    par(mfrow = c(1,2))
    boxplot(dd[[varName]] ~ P, 
            main = paste("Boxplot of", varName, "by cluster"),
            xlab = "Cluster", ylab = varName)
    hist(dd[[varName]], main = paste("Histogram of", varName),
         xlab = varName, col = "lightblue", breaks = 20)
    dev.off()
    
  } else {
    # Variable categórica
    cat("\n-------------------------------------------\n")
    cat("Variable categórica:", varName, "\n")
    tbl <- table(P, dd[[varName]])
    chi_res <- chisq.test(tbl)
    cat("Chi-squared p-value:", chi_res$p.value, "\n")
    
    # Barplot side-by-side
    png(png_file, width = 1000, height = 700)
    barplot(tbl, beside = TRUE, legend = TRUE,
            main = paste("Barplot of", varName, "by cluster"),
            xlab = "Cluster", ylab = "Count", col = rainbow(nrow(tbl)))
    dev.off()
  }
}

# 4.2 "¿Qué diferencias hay entre clases?" (4 clusters)
# - Numéricas: pairwise.t.test
# - Categóricas: pairwise.prop.test
vars_categoricas <- vars_categoricas[vars_categoricas %in% colnames(dd)]
vars_numericas <- vars_numericas[vars_numericas %in% colnames(dd)]

## 4.2.1 Pairwise T-tests (numéricas)
cat("\n#### Pairwise T-tests para variables numéricas ####\n")
for(numVar in vars_numericas) {
  cat("\nVariable numérica:", numVar, "\n")
  # pairwise.t.test recibe un vector de datos y un factor de grupos
  ptt <- pairwise.t.test(dd[[numVar]], factor(P), p.adjust.method = "bonferroni")
  print(ptt)
}

## 4.2.2 Pairwise prop.test (categóricas)
cat("\n#### Pairwise prop.test para variables categóricas ####\n")
for(catVar in vars_categoricas) {
  cat("\nVariable categórica:", catVar, "\n")
  
  # Para cada categoría
  catLevels <- levels(dd[[catVar]])
  for(lvl in catLevels) {
    cat("Categoría:", lvl, "\n")
    x_counts <- tapply(dd[[catVar]] == lvl, P, sum)
    n_counts <- table(P)
    pwt <- pairwise.prop.test(x = x_counts, n = n_counts, p.adjust.method = "bonferroni")
    print(pwt)
  }
}

# 4.3 Snake Plot (opcional)
snake_plot(dcon, P, file.path(outPath, "SnakePlot.png"))

cat("\nFinalizado el profiling con 4 clusters (Gower). Revisa la carpeta:", outPath, "\n")
