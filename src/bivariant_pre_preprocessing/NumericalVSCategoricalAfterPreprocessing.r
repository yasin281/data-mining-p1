# To successfully run this script, set the working directory to data-mining-p1
setwd(file.path(getwd(), "src/bivariant_pre_preprocessing"))

datos <- read.csv("../../data/filtered_data.csv", header = TRUE, stringsAsFactors = FALSE)
columnas_interes <- c("Month", "Year", "CustAge", "CustGender", 
                      "Country", "ProdCat", "OrdQty", 
                      "UnitCost", "UnitPrice", "Size", "Color", 
                      "Warranty", "ShipWeight", "DelTime", 
                      "Discount", "EcoFriendly", "ShipCost", "Insurance")


datos <- datos[, columnas_interes]
vars_categoricas <- c("Year", "Month", "CustGender", "Country", "ProdCat",
                      "Size", "Color", "Warranty", "EcoFriendly", "Insurance")
datos[vars_categoricas] <- lapply(datos[vars_categoricas], as.factor)
vars_numericas <- setdiff(columnas_interes, vars_categoricas)
datos[vars_numericas] <- lapply(datos[vars_numericas], as.numeric)

# Set Month to print in the correct order, the month are in short format
datos$Month <- factor(datos$Month, levels = month.abb, ordered = TRUE)


output_dir <- "../../images/after_preprocessing/bivariate/numericalVScategorical/"
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
