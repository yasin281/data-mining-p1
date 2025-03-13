# Cargar los datos
dd <- read.csv("filtered_data.csv", sep = ",", stringsAsFactors = TRUE)

# Definir las variables de interés
columnas_interes <- c("Month", "Year", "CustAge", "CustGender", 
                      "Country", "ProdCat", "OrdQty", 
                      "UnitCost", "UnitPrice", "Size", "Color", 
                      "Warranty", "ShipWeight", "DelTime", 
                      "Discount", "EcoFriendly", "ShipCost", "Insurance")
vars_categoricas <- c("Year", "Month", "CustGender", "Country", "ProdCat",
                      "Size", "Color", "Warranty", "EcoFriendly", "Insurance")

# Convertir variables categóricas y numéricas
dd[vars_categoricas] <- lapply(dd[vars_categoricas], as.factor)
vars_numericas <- setdiff(columnas_interes, vars_categoricas)
dd[vars_numericas] <- lapply(dd[vars_numericas], as.numeric)
dcon <- data.frame(dd[vars_numericas])

# Dendrograma 1: Euclidean Ward
d_euclidean <- dist(dcon)  # Distancia Euclidiana
h_euclidean <- hclust(d_euclidean, method = "ward.D2")
plot(h_euclidean, main = "1. Euclidean ward")

# Dendrograma 2: Gower Ward
library(cluster)
# Seleccionamos columnas (ajusta 'actives' según corresponda)
actives <- c(2:16)
dissimMatrix <- daisy(dd[, actives], metric = "gower", stand = TRUE)
distMatrix <- dissimMatrix^2  # Elevar al cuadrado para Ward
h_gower <- hclust(distMatrix, method = "ward.D2")
plot(h_gower, main = "2. Gower ward")
