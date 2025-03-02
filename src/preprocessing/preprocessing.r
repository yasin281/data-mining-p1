# This script is used to preprocess the data

# Steps:
# 1. Data selection
# 2. Missings treatment
# 3. Outlier treatment
# 4. Variable transformation

#-------------------------------------------------------------------------------
# remember to set the working directory to data-mini-project-1

setwd(file.path(getwd(), "src/preprocessing/"))

dd <- read.csv("../../data/reduced_data.csv", header = TRUE, sep = ",")

variablesToKeep <- c("Month", "Year", "Customer_Age", "Customer_Gender", "Country",
                     "Product_Category", "Order_Quantity", "Unit_Cost", "Unit_Price", 
                     "Size", "Color", "Warranty", "Shipping_Weight", "Delivery_Time", 
                     "Discount", "Eco_Friendly", "Shipping_Cost", "Insurance")

# 1. Data selection
dd <- dd[, variablesToKeep]
ddOrig <- dd

str(dd)

# 2. Missings treatment, print the variables with missings, empty strings are considered as missings
colSums(is.na(dd))
colSums(dd == "")
dd$Insurance[dd$Insurance == ""] <- "NA"

table(dd$Insurance)

# Save image with the barplot
output_dir <- "../../images/preprocessing/missing_values/"
if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}

png(file = paste(output_dir, "Insurance_barplot_filled_missings.png", sep = ""), width = 800, height = 600)
barplot(table(dd$Insurance))
dev.off()


# 3. Outliers treatment
# Categorical variables, remove instances with Portugal
output_dir <- "../../images/preprocessing/outliers/"
if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}

png(file = paste(output_dir, "Country_barplot.png", sep = ""), width = 800, height = 600)
barplot(table(dd$Country))
dev.off()

dd <- dd[dd$Country != "Portugal",]

png(file = paste(output_dir, "Country_barplot_outliers_removed.png", sep = ""), width = 800, height = 600)
barplot(table(dd$Country))
dev.off()

# Numerical variables,  no outliers detected

# 4. Variable transformation

# 4.1 Shorten the name of the variables
newNames <- c("Month", "Year", "CustAge", "CustGender", "Country", 
                    "ProdCat", "OrdQty", "UnitCost", "UnitPrice", 
                    "Size", "Color", "Warranty", "ShipWeight", "DelTime", 
                    "Discount", "EcoFriendly", "ShipCost", "Insurance")
colnames(dd) <- newNames

print(colnames(dd))

# 4.2 Shorten the modality names
newShortMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_mapping <- setNames(newShortMonths, month.name)
dd$Month <- month_mapping[dd$Month]
# Check 
print(sort(unique(dd$Month)) == sort(newShortMonths))


newShortCountries <- c("US", "UK", "CAN", "AUS", "DE", "FR")
country_mapping <- setNames(newShortCountries, c("United States", "United Kingdom", "Canada", "Australia", "Germany", "France"))
dd$Country <- country_mapping[dd$Country]
# Check 
print(sort(unique(dd$Country)) == sort(newShortCountries))

newShortSizes <- c("S", "M", "L", "XL")
size_mapping <- setNames(newShortSizes, c("Small", "Medium", "Large", "Extra Large"))
dd$Size <- size_mapping[dd$Size]
# Check if the mapping is correct
print(sort(unique(dd$Size)) == sort(newShortSizes))

newShortWarranties <- c("1Y", "2Y", "3Y", "Lifetime")
warranty_mapping <- setNames(newShortWarranties, c("1 Year", "2 Years", "3 Years", "Lifetime"))
dd$Warranty <- warranty_mapping[dd$Warranty]
# Check if the mapping is correct
print(sort(unique(dd$Warranty)) == sort(newShortWarranties))

newShortProdCats <- c("Acc","Clothes","Bike")
prodcat_mapping <- setNames(newShortProdCats, c("Accessories", "Clothing", "Bikes"))
dd$ProdCat <- prodcat_mapping[dd$ProdCat]
# Check if the mapping is correct
print(sort(unique(dd$ProdCat)) == sort(newShortProdCats))



# 4.3 Normalization of numerical variables
# Normalize the numerical variables, save the images with the histograms
output_dir <- "../../images/preprocessing/normalized/"
if (!dir.exists(output_dir)) {
  print(paste("Creating directory", output_dir))
  dir.create(output_dir, recursive = TRUE)
}

numericalVariables <- c("CustAge", "OrdQty", "UnitCost", "UnitPrice", 
                        "ShipWeight", "DelTime", "Discount", "ShipCost")

dd[, numericalVariables] <- scale(dd[, numericalVariables])

for (var in numericalVariables) {
  png(file = paste(output_dir, var, "_histogram.png", sep = ""), width = 800, height = 600)
  hist(dd[[var]], main = paste("Histogram of", var))
  dev.off()
}

# Save the preprocessed data
write.csv(dd, "../../data/filtered_data.csv", row.names = FALSE)

print("Preprocessing finished")

