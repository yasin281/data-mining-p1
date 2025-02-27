# Configurar directorio de trabajo
getwd()
#setwd("C:/Users/jaaaa/Desktop/MD")
#setwd("C:/Users/usuario/Documents/uni_upc/ultimoaño/MD/scripts_univariante")

# Leer datos
dd <- read.csv("muestras.csv", header = TRUE, sep = ",")

# Crear carpeta para guardar gráficos
dir.create("graficas", showWarnings = FALSE)

custom_breaks <- function(x) {
  min_val <- min(x)  # mínimo
  max_val <- max(x)  # máximo
  seq(min_val, max_val, by = 3)  # cortes cada 3 unidades
}

# Instalar y cargar librerías necesarias
install.packages("colorspace")
install.packages("DescTools", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
library(colorspace)
library(stats)
library(DescTools)
library(ggplot2)

colors <- qualitative_hcl(12, palette = "dark 3")

#-------------------------------------------------------------------------------
# Análisis para CustomerAge

CustomerAge <- dd[,"Customer_Age"]

# Histograma de CustomerAge
png("graficas/hist_CustomerAge.png")
hist(CustomerAge, custom_breaks, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Customer_Age", xlab = "Customer Age")
dev.off()

# Histograma de log(CustomerAge)
png("graficas/hist_log_CustomerAge.png")
hist(log(CustomerAge), custom_breaks, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de log(Customer_Age)", xlab = "log(Customer Age)")
dev.off()

summary(CustomerAge)

# Test de normalidad
shapiro.test(CustomerAge)
shapiro.test(log(CustomerAge))

# QQ-plot de CustomerAge
png("graficas/qqplot_CustomerAge.png")
qqnorm(CustomerAge, main = "QQ-Plot de Customer_Age")
qqline(CustomerAge, col = "orange2")
dev.off()

# QQ-plot de log(CustomerAge)
png("graficas/qqplot_log_CustomerAge.png")
qqnorm(log(CustomerAge), main = "QQ-Plot de log(Customer_Age)")
qqline(log(CustomerAge), col = "orange2")
dev.off()

# Boxplot de CustomerAge
png("graficas/boxplot_CustomerAge.png")
boxplot(CustomerAge, col = "purple4", main = "Boxplot de Customer_Age", ylab = "Customer Age")
dev.off()

Freq(CustomerAge)

#-------------------------------------------------------------------------------
# Análisis para CustomerGender

CustomerGender <- dd[,"Customer_Gender"]

# Diagrama de barras para CustomerGender
png("graficas/barplot_CustomerGender.png")
barplot(table(CustomerGender), col = c("purple4", "green4"),
        main = "Diagrama de barras de Customer_Gender", ylab = "Frecuencia")
dev.off()

Freq(CustomerGender)
chisq.test(table(CustomerGender), p = c(0.5, 0.5))

#-------------------------------------------------------------------------------
# Análisis para Month

Month <- dd[,"Month"]

summary(factor(Month, levels = month.name))
png("graficas/barplot_Month.png", width= 600, height=480)
barplot(table(factor(Month, levels = month.name)), col = colors,
        main = "Diagrama de barras de Meses", ylab = "Frecuencia"
        ,las=2)
dev.off()
Freq(factor(Month, levels = month.name))

#-------------------------------------------------------------------------------
# Análisis para Year

Year <- dd[,"Year"]

summary(as.factor(Year))
png("graficas/barplot_Year.png")
barplot(table(as.factor(Year)), col = "gray50",
        main = "Diagrama de barras de Year", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Year))

#-------------------------------------------------------------------------------
# Análisis para Country

Country <- dd[,"Country"]

summary(as.factor(Country))
png("graficas/pie_Country.png")
pie(table(as.factor(Country)), col = colors,
    main = "Gráfico de pastel de Country")
dev.off()

png("graficas/barplot_Country.png", width=800)
barplot(table(as.factor(Country)), col = colors,
        main = "Diagrama de barras de Country", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Country))

#-------------------------------------------------------------------------------
# Análisis para ProductCategory

ProductCategory <- dd[,"Product_Category"]

summary(as.factor(ProductCategory))
png("graficas/pie_ProductCategory.png")
pie(table(as.factor(ProductCategory)), col = colors,
    main = "Gráfico de pastel de Product_Category")
dev.off()

png("graficas/barplot_ProductCategory.png")
barplot(table(as.factor(ProductCategory)), col = colors,
        main = "Diagrama de barras de Product_Category", ylab = "Frecuencia")
dev.off()
Freq(as.factor(ProductCategory))

#-------------------------------------------------------------------------------
# Análisis para OrderQty

OrderQty <- dd[,"Order_Quantity"]

png("graficas/hist_OrderQty.png")
hist(OrderQty, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Order_Quantity", xlab = "Order Quantity")
dev.off()

summary(OrderQty)

png("graficas/boxplot_OrderQty.png")
boxplot(OrderQty, col = "purple4", main = "Boxplot de Order_Quantity", ylab = "Order Quantity")
dev.off()

#-------------------------------------------------------------------------------
# Análisis para UCost

UCost <- dd[,"Unit_Cost"]

png("graficas/hist_UCost.png")
hist(UCost, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Unit_Cost", xlab = "Unit Cost")
dev.off()

png("graficas/hist_log_UCost.png")
hist(log(UCost), freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de log(Unit_Cost)", xlab = "log(Unit Cost)")
dev.off()

summary(UCost)

png("graficas/boxplot_log_UCost.png")
boxplot(log(UCost), col = "purple4", main = "Boxplot de log(Unit_Cost)", ylab = "log(Unit Cost)")
dev.off()

#-------------------------------------------------------------------------------
# Análisis para UPrice

UPrice <- dd[,"Unit_Price"]

png("graficas/hist_UPrice.png")
hist(UPrice, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Unit_Price", xlab = "Unit Price")
dev.off()

png("graficas/hist_log_UPrice.png")
hist(log(UPrice), freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de log(Unit_Price)", xlab = "log(Unit Price)")
dev.off()

summary(UPrice)

png("graficas/boxplot_log_UPrice.png")
boxplot(log(UPrice), col = "purple4", main = "Boxplot de log(Unit_Price)", ylab = "log(Unit Price)")
dev.off()

#-------------------------------------------------------------------------------
# Análisis para Size

Size <- dd[,"Size"]

summary(as.factor(Size))
png("graficas/pie_Size.png")
pie(table(as.factor(Size)), col = colors, main = "Gráfico de pastel de Size")
dev.off()

png("graficas/barplot_Size.png")
barplot(table(factor(Size, levels = c("Small", "Medium", "Large", "Extra Large"))), col = colors,
        main = "Diagrama de barras de Size", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Size))
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Análisis para Insurance

Insurance <- dd[,"Insurance"]

summary(as.factor(Insurance))
png("graficas/pie_Insurance.png")
pie(table(as.factor(Insurance)), col = colors, main = "Gráfico de pastel de Insurance")
dev.off()

png("graficas/barplot_Insurance.png")
barplot(table(factor(Insurance, levels = c("None", "Basic", "Premium"))), col = colors,
        main = "Diagrama de barras de Insurance", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Insurance))

#-------------------------------------------------------------------------------
# Análisis para Color

Color <- dd[,"Color"]

summary(as.factor(Color))
png("graficas/pie_Color.png")
pie(table(as.factor(Color)), col = colors, main = "Gráfico de pastel de Color")
dev.off()

png("graficas/barplot_Color.png")
barplot(table(factor(Color, levels = c("Black", "Red", "Blue", "Green","White"))), col = colors,
        main = "Diagrama de barras de Color", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Color))

#-------------------------------------------------------------------------------
# Análisis para Warranty

Warranty <- dd[,"Warranty"]

summary(as.factor(Warranty))
png("graficas/pie_Warranty.png")
pie(table(as.factor(Warranty)), col = colors, main = "Gráfico de pastel de Warranty")
dev.off()

png("graficas/barplot_Warranty.png")
barplot(table(factor(Warranty, levels = c("1 Year", "2 Years", "3 Years", "Lifetime"))), col = colors,
        main = "Diagrama de barras de Warranty", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Warranty))

#-------------------------------------------------------------------------------
# Análisis para Eco_friendly

Eco_friendly <- dd[,"Eco_Friendly"]

summary(as.factor(Eco_friendly))
png("graficas/pie_Eco_friendly.png")
pie(table(as.factor(Eco_friendly)), col = colors, main = "Gráfico de pastel de Eco_friendly")
dev.off()

png("graficas/barplot_Eco_friendly.png")
barplot(table(factor(Eco_friendly, levels = c("True", "False"))), col = colors,
        main = "Diagrama de barras de Eco_friendly", ylab = "Frecuencia")
dev.off()
Freq(as.factor(Eco_friendly))
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Análisis para Shipping_Cost

Shipping_cost <- dd[,"Shipping_Cost"]

png("graficas/hist_Shipping_cost.png")
hist(Shipping_cost, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Shipping_cost", xlab = "Shipping_cost")
dev.off()

summary(Shipping_cost)

png("graficas/boxplot_OrderQty.png")
boxplot(Shipping_cost, col = "purple4", main = "Boxplot de Shipping_cost", ylab = "Shipping_cost")
dev.off()
#-------------------------------------------------------------------------------
# Análisis para Discount

Discount <- dd[,"Discount"]

png("graficas/hist_Discount.png")
hist(Discount, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Discount", xlab = "Discount")
dev.off()

summary(Discount)

png("graficas/boxplot_Discount.png")
boxplot(Discount, col = "purple4", main = "Boxplot de Discount", ylab = "Discount")
dev.off()
#-------------------------------------------------------------------------------
# Análisis para Shipping_Weight

Shipping_weight <- dd[,"Shipping_Weight"]

png("graficas/hist_Shipping_weight.png")
hist(Shipping_weight, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Shipping_weight", xlab = "Shipping_weight")
dev.off()

summary(Shipping_weight)

png("graficas/boxplot_Shipping_weight.png")
boxplot(OrderQty, col = "purple4", main = "Boxplot de Shipping_weight", ylab = "Shipping_weight")
dev.off()
#-------------------------------------------------------------------------------
# Análisis para Devilery_time

Devilery_time <- dd[,"Delivery_Time"]

png("graficas/hist_Devilery_time.png")
hist(Devilery_time, freq = TRUE, col = "purple4", border = "lightblue1",
     main = "Histograma de Devilery_time", xlab = "Devilery_time")
dev.off()

summary(Devilery_time)

png("graficas/boxplot_Devilery_time.png")
boxplot(Devilery_time, col = "purple4", main = "Boxplot de Devilery_time", ylab = "Devilery_time")
dev.off()

