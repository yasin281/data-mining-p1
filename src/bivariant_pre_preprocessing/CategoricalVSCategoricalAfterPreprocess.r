# In this script we will implement the analysis of the relationship between categorical variables. 
# We will use the contingency tables and multiple barplots to visualize the relationship between the variables.

# Load libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Set the working directory, you have to be located in the root of the project, data-mining-p1
setwd(file.path(getwd(), "/src/bivariant_pre_preprocessing"))

# Read the data file
data <- read.csv("../../data/filtered_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

data$Year <- as.factor(data$Year)
# Set Month to print in the correct order, the months are in short format
data$Month <- factor(datos$Month, levels = month.abb, ordered = TRUE)

categoricalVariablesNames <- c("Year", "Month", "CustGender", "Country", "ProdCat", "Size", "Color", "Warranty", "EcoFriendly", "Insurance")
# Select the categorical variables from the data and convert them to factors
categoricalVariables <- lapply(data[,categoricalVariablesNames], as.factor)

print(length(categoricalVariables))
print(categoricalVariables)

output_dir <- "../../images/after_preprocessing/bivariate/categoricalVScategorical/"

if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}

pdf(file = paste(output_dir, "contingency_tables.pdf", sep = ""), width = 12, height = 7)

for (i in 1:length(categoricalVariablesNames)) {
  for (j in 1:length(categoricalVariablesNames)) {
    if (i != j) {
      var1 <- categoricalVariables[[i]]
      var2 <- categoricalVariables[[j]]
      
      contingencyTable <- table(var1, var2)
      contingencyTable <- cbind(contingencyTable, Total = rowSums(contingencyTable))
      contingencyTable <- rbind(contingencyTable, Total = colSums(contingencyTable))
      
      print(paste("Contingency table for", categoricalVariablesNames[i], "and", categoricalVariablesNames[j]))
      print(contingencyTable)
      
      df <- as.data.frame.matrix(contingencyTable)

      grid.newpage()

      grid.table(df)
      grid.text(paste("Contingency table for", categoricalVariablesNames[i], "and", categoricalVariablesNames[j]),
                x = unit(0.5, "npc"), y = unit(0.95, "npc"), just = "center", 
                gp = gpar(fontsize = 14, fontface = "bold"))
    }
  }
}
dev.off()

print("Contingency tables have been saved to PDF.")

pdf(file = paste(output_dir, "multiple_barplots.pdf", sep = ""), width = 10, height = 8)

for (i in 1:length(categoricalVariablesNames)) {
  for (j in 1:length(categoricalVariablesNames)) {
    if (i != j) {
      var1_name <- categoricalVariablesNames[i]
      var2_name <- categoricalVariablesNames[j]

      print(ggplot(data, aes_string(x = var1_name, fill = var2_name)) +
            geom_bar(position = "dodge") +
            labs(x = var1_name, y = "Count") +
            theme_minimal() +
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8),
                  plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +  # Center and bold the title
            ggtitle(paste(var1_name, "vs", var2_name))
      )
    }
  }
}

dev.off()

print("Multiple barplots have been saved to PDF.")
print("End of the script")