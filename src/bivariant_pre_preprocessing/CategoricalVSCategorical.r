# In this script we will implement the analysis of the relationship between categorical variables. 
# We will use the contingency tables to analyze the relationship between two categorical variables.

# Load libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Set the working directory, you have to be located in the root of the project, data-mining-p1
setwd(file.path(getwd(), "/src/bivariant_pre_preprocessing"))

# Read the data file
data <- read.csv("../../data/reduced_data.csv", header = TRUE, sep = ",")

categoricalVariablesNames <- c("Month","Customer_Gender","Country","Product_Category","Size","Color","Warranty","Eco_Friendly","Insurance")

# Select the categorical variables from the data and convert them to factors
categoricalVariables <- lapply(data[,categoricalVariablesNames], as.factor)

print(length(categoricalVariables))
print(categoricalVariables)

output_dir <- "graphics/categoricalVScategorical/contingency_tables/"
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

print("End of script: CategoricalVSCategorical.r")