#Load libraries
library(corrplot)


#Read the data file
data <- read.csv("./data/muestras.csv", header = TRUE, sep = ",")
#After preprocessing
data_after_prepro <- read.csv("./data/filtered_data.csv", header = TRUE, sep = ",")

names(data)


#Select the numerical variables
num<-c("Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Shipping_Weight", "Delivery_Time", "Discount", "Shipping_Cost")
#After preprocessing
num_after_prepro<-c("CustAge", "OrdQty", "UnitCost", "UnitPrice", "ShipWeight", "DelTime", "Discount", "ShipCost")

NumericVariables<-data[,num]
NumericVariables_after_prepro<-data_after_prepro[,num_after_prepro]


n<-dim(NumericVariables)[1]
K<-dim(NumericVariables)[2]

n_after_prepro<-dim(NumericVariables_after_prepro)[1]
K_after_prepro<-dim(NumericVariables_after_prepro)[2]

summary(NumericVariables)
summary(NumericVariables_after_prepro)

#Create graphics directory
if (!dir.exists("graficos/")){
  dir.create("graficos/")
}
#After preprocessing
if (!dir.exists("graficos_after_preprocessing/")){
  dir.create("graficos_after_preprocessing/")
}

#Compute the correlation matrix
correlation<-cor(NumericVariables)
correlation_after_prepro<-cor(NumericVariables_after_prepro)


#Visualize the correlation matrix
png("graficos/corrMat.png")
corrplot(correlation, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 1, col = colorRampPalette(c("red", "grey", "blue"))(200))
dev.off()
#After preprocessing
png("graficos_after_preprocessing/corrMat_after_preprocessing.png")
corrplot(correlation_after_prepro, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 1, col = colorRampPalette(c("red", "grey", "blue"))(200))
dev.off()
 
#Create all scatter plots

for (i in 1:(K-1)) {
  for (j in (i + 1):K) {
    name <- paste0(num[i], "_vs_", num[j])
    png(paste0("graficos/scatterplot_", name, ".png"))
    plot(NumericVariables[[i]], NumericVariables[[j]], main = paste(num[i], "vs", num[j]), xlab = num[i], ylab = num[j], col = "blue", pch = 16)
    modelo <- lm(NumericVariables[[j]] ~ NumericVariables[[i]])
    abline(modelo, col = "red", lwd = 2)
    dev.off()
  }
}

#After preprocessing
for (i in 1:(K-1)) {
  for (j in (i + 1):K) {
    name <- paste0(num_after_prepro[i], "_vs_", num_after_prepro[j])
    png(paste0("graficos_after_preprocessing/scatterplot_", name, "_after_preprocessing", ".png"))
    plot(NumericVariables_after_prepro[[i]], NumericVariables_after_prepro[[j]], main = paste(num_after_prepro[i], "vs", num_after_prepro[j]), xlab = num_after_prepro[i], ylab = num_after_prepro[j], col = "blue", pch = 16)
    modelo <- lm(NumericVariables_after_prepro[[j]] ~ NumericVariables_after_prepro[[i]])
    abline(modelo, col = "red", lwd = 2)
    dev.off()
  }
}

