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
# 'actives' define las columnas a usar (aquí se incluyen tanto numéricas como categóricas)
actives <- c(2:16)
dissimMatrix <- daisy(dd[, actives], metric = "gower", stand = TRUE)  # Calcula la disimilitud con Gower
distMatrix <- dissimMatrix^2  # Eleva al cuadrado para adecuarlo a Ward’s method
h_gower <- hclust(distMatrix, method = "ward.D2")  # Clustering jerárquico con Ward
plot(h_gower, main = "2. Gower ward")

# ---------------------------------------------------------
# Sección 1: Elbow Method (Método del Codo) con k-means
# ---------------------------------------------------------
# Se utiliza el dataset numérico (dcon)
wss <- numeric(10)  # Vector para almacenar la suma total de cuadrados intra-cluster
for (k in 1:10) {
  km <- kmeans(dcon, centers = k)
  wss[k] <- km$tot.withinss
}
plot(1:10, wss, type = "b", 
     xlab = "Number of clusters", 
     ylab = "Within-cluster sum of squares", 
     main = "Elbow Method")

# ---------------------------------------------------------
# Sección 2: Silhouette Method (Método de la Silueta) con k-means
# ---------------------------------------------------------
library(cluster)
sil_width <- numeric(9)
for (k in 2:10) {
  km <- kmeans(dcon, centers = k)
  ss <- silhouette(km$cluster, dist(dcon))
  sil_width[k-1] <- mean(ss[, 3])
}
plot(2:10, sil_width, type = "b", 
     xlab = "Number of clusters", 
     ylab = "Average silhouette width", 
     main = "Silhouette Method")

# ---------------------------------------------------------
# Sección 3: Calinski-Harabasz Index con k-means
# ---------------------------------------------------------
install.packages("fpc")
library(fpc)  # Asegúrate de tener instalado el paquete fpc
ch_index <- numeric(9)
for (k in 2:10) {
  km <- kmeans(dcon, centers = k)
  ch_index[k-1] <- calinhara(dcon, km$cluster, k)
}
plot(2:10, ch_index, type = "b", 
     xlab = "Number of clusters", 
     ylab = "Calinski-Harabasz Index", 
     main = "Calinski-Harabasz Index")
