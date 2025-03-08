# To successfully run this script, set the working directory to data-mining-p1
setwd(file.path(getwd(), "src/bivariant_pre_preprocessing"))

datos <- read.csv("../../data/reduced_data.csv", header = TRUE, stringsAsFactors = FALSE)
columnas_interes <- c("Month", "Year", "Customer_Age", "Customer_Gender", 
                      "Country", "Product_Category", "Order_Quantity", 
                      "Unit_Cost", "Unit_Price", "Size", "Color", 
                      "Warranty", "Shipping_Weight", "Delivery_Time", 
                      "Discount", "Eco_Friendly", "Shipping_Cost", "Insurance")
datos <- datos[, columnas_interes]
vars_categoricas <- c("Year", "Month", "Customer_Gender", "Country", "Product_Category",
                      "Size", "Color", "Warranty", "Eco_Friendly", "Insurance")
datos[vars_categoricas] <- lapply(datos[vars_categoricas], as.factor)
vars_numericas <- setdiff(columnas_interes, vars_categoricas)
datos[vars_numericas] <- lapply(datos[vars_numericas], as.numeric)

# Set Month to print in the correct order
datos$Month <- factor(datos$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

output_dir <- "../../images/before_preprocessing/bivariate/numericalVScategorical/"
if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}

pdf(paste(output_dir, "boxplots.pdf", sep = ""), width = 8, height = 8)
for (cat in vars_categoricas) {
  for (num in vars_numericas) {
    boxplot(as.formula(paste(num, "~", cat)), data = datos, col = "lightgreen",
            main = paste(num, "vs", cat))
  }
}
dev.off()

library(ggplot2)
pdf(paste(output_dir, "histograms.pdf", sep = ""), width = 8, height = 8)
for (cat in vars_categoricas) {
  for (num in vars_numericas) {
    print(ggplot(datos, aes_string(x = num, fill = cat)) +
            geom_histogram(position = "dodge", bins = 30) +
            labs(x = num, y = "Count") +
            theme_minimal() +
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8),
                  plot.title = element_blank()))
  }
}
dev.off()
