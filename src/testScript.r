
# Check if file exists and load it
if (!file.exists("../data/muestra.csv")) {
  stop("File not found")
}

data <- read.csv("../data/muestra.csv", header = TRUE, sep = ",")
# Print the first 6 rows
head(data) 