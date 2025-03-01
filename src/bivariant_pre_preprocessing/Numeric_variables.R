#Load libraries
library(corrplot)


#Read the data file
data <- read.csv("./data/reduced_data.csv", header = TRUE, sep = ",")

#Select the numerical variables
num<-c("Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Revenue", "Shipping_Weight", "Delivery_Time", "Discount", "Shipping_Cost")
NumericVariables<-data[,num]

n<-dim(NumericVariables)[1]
K<-dim(NumericVariables)[2]

summary(NumericVariables)

#Compute the correlation matrix
correlation<-cor(NumericVariables)

#Visualize the correlation matrix
corrplot(correlation, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 1, col = colorRampPalette(c("red", "grey", "blue"))(200))
 
#Create all scatter plots
if (!dir.exists("graficos/")){
  dir.create("graficos/")
}

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

