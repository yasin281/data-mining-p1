# Load libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Set the working directory, you have to be located in the root of the project, data-mining-p1
setwd(file.path(getwd(), "/src/bivariant_pre_preprocessing"))

# Read the data file
data <- read.csv("../../data/filtered_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

categoricalVariablesNames <- c("Month", "CustGender", "Country", "ProdCat", "Size", "Color", "Warranty", "EcoFriendly", "Insurance")
# Select the categorical variables from the data and convert them to factors
categoricalVariables <- lapply(data[,categoricalVariablesNames], as.factor)

print(length(categoricalVariables))
print(categoricalVariables)

output_dir <- "../../images/after_preprocessing/bivariate/categoricalVScategorical/contingency_tables/"

if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}

contingency_tables <- list()

# Loop through each pair of categorical variables
for (i in 1:length(categoricalVariablesNames)) {
  for (j in 1:length(categoricalVariablesNames)) {
    if (i != j) {
      var1 <- categoricalVariables[[i]]
      var2 <- categoricalVariables[[j]]

      contingencyTable <- table(var1, var2)
      contingencyTable <- cbind(contingencyTable, Total = rowSums(contingencyTable))
      contingencyTable <- rbind(contingencyTable, Total = colSums(contingencyTable))

      # Print the contingency table
      print(paste("Contingency table for", categoricalVariablesNames[i], "and", categoricalVariablesNames[j]))
      print(contingencyTable)

      # Save the images in the graphics folder
      png(file = paste(output_dir, categoricalVariablesNames[i], "_vs_", categoricalVariablesNames[j], ".png", sep = ""), width = 800, height = 600)

      df <- as.data.frame.matrix(contingencyTable)

      grid.table(df)

      grid.text(paste("Contingency table for", categoricalVariablesNames[i], "and", categoricalVariablesNames[j]), x = unit(0.5, "npc"), y = unit(0.95, "npc"), just = "center", gp = gpar(fontsize = 14, fontface = "bold"))

      dev.off()
    }
  }
}

print("Contingency tables have been created")

output_dir <- "../../images/after_preprocessing/bivariate/categoricalVScategorical/multipleBarplots/"

if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}


for (i in 1:length(categoricalVariablesNames)) {
  for (j in 1:length(categoricalVariablesNames)) {
    if (i != j) {
      var1_name <- categoricalVariablesNames[i]
      var2_name <- categoricalVariablesNames[j]
      
      print(paste("Creating barplot for", var1_name, "and", var2_name))
      
      p <- ggplot(data, aes_string(x = var1_name, fill = var2_name)) +
        geom_bar(position = "dodge") +
        labs(title = paste("Multiple Barplot of", var1_name, "vs", var2_name),
             x = var1_name,
             y = "Count") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white", colour = "white"),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 10),
              plot.title = element_text(size = 14, face = "bold"),
              axis.title = element_text(size = 12),
              panel.border = element_blank())  
      
      ggsave(filename = paste(output_dir,"MultipleBarplot", var1_name, "_vs_", var2_name, ".png", sep = ""), plot = p, width = 10, height = 6)
    }
  }
}
print("Multiple barplots have been created")

print("End of the script")

