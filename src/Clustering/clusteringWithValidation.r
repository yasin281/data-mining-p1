# ------------------------------
# Cargar y preprocesar los datos
# ------------------------------

dd <- read.csv("data/filtered_data.csv", sep = ",", stringsAsFactors = TRUE)
columnas_interes <- c("Month", "Year", "CustAge", "CustGender", 
                      "Country", "ProdCat", "OrdQty", 
                      "UnitCost", "UnitPrice", "Size", "Color", 
                      "Warranty", "ShipWeight", "DelTime", 
                      "Discount", "EcoFriendly", "ShipCost", "Insurance")

vars_categoricas <- c("Year", "Month", "CustGender", "Country", "ProdCat",
                      "Size", "Color", "Warranty", "EcoFriendly", "Insurance")

dd[vars_categoricas] <- lapply(dd[vars_categoricas], as.factor)
vars_numericas <- setdiff(columnas_interes, vars_categoricas)
dd[vars_numericas] <- lapply(dd[vars_numericas], as.numeric)

dcon <- data.frame(dd[vars_numericas])  

# ------------------------------
# Clustering Hierarchical
# ------------------------------

# MÉTODO 1: Distancia Euclídea + Ward (Solo Numéricas)
d_euclidean <- dist(dcon)  
h_euclidean <- hclust(d_euclidean, method = "ward.D2")  
plot(h_euclidean, main = "1. Euclidean Ward")
rect.hclust(h_euclidean, k = 2, border = "red") 
rect.hclust(h_euclidean, k = 3, border = "blue") 

# MÉTODO 2: Distancia Gower + Ward (Todas las Variables)
library(cluster)
dissimMatrix <- daisy(dd, metric = "gower", stand = TRUE)  
distMatrix <- dissimMatrix^2  
h_gower <- hclust(distMatrix, method = "ward.D2")  
plot(h_gower, main = "2. Gower Ward")
rect.hclust(h_gower, k = 2, border = "red") 
rect.hclust(h_gower, k = 3, border = "blue") 

# ------------------------------
# Validation of Clustering
# ------------------------------

library(cluster)

sil_width_euclidean <- numeric(9)
sil_width_gower <- numeric(9)

for (k in 2:10) {
  c_euclidean <- cutree(h_euclidean, k)  
  c_gower <- cutree(h_gower, k)  

  sil_euclidean <- silhouette(c_euclidean, d_euclidean)
  sil_gower <- silhouette(c_gower, distMatrix)

  sil_width_euclidean[k-1] <- mean(sil_euclidean[, 3])
  sil_width_gower[k-1] <- mean(sil_gower[, 3])
}


plot(2:10, sil_width_euclidean, type = "b", col = "blue", pch = 19,
     xlab = "Number of clusters (k)", 
     ylab = "Average silhouette width", 
     main = "Silhouette Method - Hierarchical Clustering",
     ylim = range(c(sil_width_euclidean, sil_width_gower)))
lines(2:10, sil_width_gower, type = "b", col = "red", pch = 19)

legend("topright", legend = c("Euclidean", "Gower"), col = c("blue", "red"), pch = 19)

# ------------------------------
# Validation of Clustering with Calinski-Harabasz
# ------------------------------
library(fpc)

ch_index_euclidean <- numeric(9)
ch_index_gower <- numeric(9)

for (k in 2:10) {

  c_euclidean <- cutree(h_euclidean, k)  
  c_gower <- cutree(h_gower, k)  

   ch_index_euclidean[k-1] <- calinhara(dcon, c_euclidean, k)
   ch_index_gower[k-1] <- calinhara(dd, c_gower, k)
}

plot(2:10, ch_index_euclidean, type = "b", col = "blue", pch = 19,
     xlab = "Number of clusters (k)", 
     ylab = "Calinski-Harabasz Index", 
     main = "Calinski-Harabasz Index - Hierarchical Clustering",
     ylim = range(c(ch_index_euclidean, ch_index_gower)))
lines(2:10, ch_index_gower, type = "b", col = "red", pch = 19)
legend("topright", legend = c("Euclidean", "Gower"), col = c("blue", "red"), pch = 19)

# ----------------------------