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
dissimMatrix <- daisy(dd, metric = "gower", stand = TRUE)  # Calcula la disimilitud con Gower
distMatrix <- dissimMatrix^2  # Eleva al cuadrado para adecuarlo a Ward’s method
h_gower <- hclust(distMatrix, method = "ward.D2")  # Clustering jerárquico con Ward
plot(h_gower, main = "2. Gower ward")

# ---------------------------------------------------------
# Función para calcular WSS (Within-cluster Sum of Squares)
# ---------------------------------------------------------
computeWSS <- function(data, clusters) {
  wss <- 0
  for (cl in unique(clusters)) {
    cluster_data <- data[clusters == cl, ]
    centroid <- colMeans(cluster_data)
    wss <- wss + sum(rowSums((cluster_data - centroid)^2))
  }
  return(wss)
}

# ---------------------------------------------------------
# Sección 1: Elbow Method (Método del Codo) usando clustering jerárquico (Gower)
# ---------------------------------------------------------
wss_hc <- numeric(9)  # Para k = 2 a 10
for (k in 2:10) {
  clusters_hc <- cutree(h_gower, k)
  wss_hc[k - 1] <- computeWSS(dcon, clusters_hc)
}
plot(2:10, wss_hc, type = "b", 
     xlab = "Number of clusters", 
     ylab = "WSS (Within-cluster Sum of Squares)", 
     main = "Elbow Method (Hierarchical, Gower)")

# ---------------------------------------------------------
# Sección 2: Silhouette Method (Método de la Silueta) usando clustering jerárquico (Gower)
# ---------------------------------------------------------
sil_width_hc <- numeric(9)
for (k in 2:10) {
  clusters_hc <- cutree(h_gower, k)
  # Usamos dissimMatrix (la distancia de Gower sin elevar al cuadrado)
  sil <- silhouette(clusters_hc, dissimMatrix)
  sil_width_hc[k - 1] <- mean(sil[, 3])
}
plot(2:10, sil_width_hc, type = "b", 
     xlab = "Number of clusters", 
     ylab = "Average Silhouette Width", 
     main = "Silhouette Method (Hierarchical, Gower)")

# ---------------------------------------------------------
# Sección 3: Calinski-Harabasz Index usando clustering jerárquico (Gower)
# ---------------------------------------------------------
install.packages("fpc")  # Instalar si no está instalado
library(fpc)
ch_index_hc <- numeric(9)
for (k in 2:10) {
  clusters_hc <- cutree(h_gower, k)
  ch_index_hc[k - 1] <- calinhara(dcon, clusters_hc, k)
}
# ---------------------------------------------------------
# Sección 4: Análisis del tamaño de los clusters
# ---------------------------------------------------------
cut_euclidean <- cutree(h_euclidean, k=4)
table(cut_euclidean)

cut_gower <- cutree(h_gower, k=4)
table(cut_gower)
plot(2:10, ch_index_hc, type = "b", 
     xlab = "Number of clusters", 
     ylab = "Calinski-Harabasz Index", 
     main = "Calinski-Harabasz Index (Hierarchical, Gower)")
