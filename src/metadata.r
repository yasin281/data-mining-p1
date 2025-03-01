# Load the data
dd <- read.csv("data/reduced_data.csv", header = T, sep = ",")

# Current structure
str(dd)

variablesToKeep <- c("Month", "Year", "Customer_Age", "Customer_Gender", "Country",
                     "Product_Category", "Order_Quantity", "Unit_Cost", "Unit_Price", 
                     "Size", "Color", "Warranty", "Shipping_Weight", "Delivery_Time", 
                     "Discount", "Eco_Friendly", "Shipping_Cost", "Insurance")

# Select the variables to keep
dd <- dd[, variablesToKeep]

for (i in 1:ncol(dd)) {
  print(paste("Variable:", names(dd)[i]))

  if (is.numeric(dd[, i])) {
    print(range(dd[, i]))
  } else {
    print(sort(unique(dd[, i])))
  }
}

# Shorten names
newNames <- c("Month", "Year", "CustAge", "CustGender", "Country", 
                    "ProdCat", "OrdQty", "UnitCost", "UnitPrice", 
                    "Size", "Color", "Warranty", "ShipWeight", "DelTime", 
                    "Discount", "EcoFriendly", "ShipCost", "Insurance")
colnames(dd) <- newNames

# New structure
str(dd)

# Save the new data frame
write.csv(dd, "data/filtered_data.csv", row.names = FALSE)

print("File saved")

rm(dd)