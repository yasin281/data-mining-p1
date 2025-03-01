# Load the data
dd <- read.csv("data/reduced_data.csv", header = T, sep = ",")
dd_original <- dd

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

# Shorten the modality name Month to Jan, Feb, Mar, ...
newShortMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# Map full month names to short month names
month_mapping <- setNames(newShortMonths, month.name)
# mappping
dd$Month <- month_mapping[dd$Month]
# Check
for (i in 1:length(newShortMonths)) {
  print(sum(dd_original$Month == names(month_mapping)[i]) == sum(dd$Month == newShortMonths[i]))
}
print(sort(unique(dd$Month)) == sort(newShortMonths))

# Shorten the Country names
newShortCountries <- c("US", "UK", "Canada", "AUS", "DE", "FR", "PT")
# Map full country names to short country names
country_mapping <- setNames(newShortCountries, c("United States", "United Kingdom", "Canada", "Australia", "Germany", "France", "Portugal"))
# mappping
dd$Country <- country_mapping[dd$Country]
# Check
for (i in 1:length(newShortCountries)) {
  print(sum(dd_original$Country == names(country_mapping)[i]) == sum(dd$Country == newShortCountries[i]))
}
print(sort(unique(dd$Country)) == sort(newShortCountries))


newShortSizes <- c("S", "M", "L", "XL")
# Map full size names to short size names
size_mapping <- setNames(newShortSizes, c("Small", "Medium", "Large", "Extra Large"))
# mappping
dd$Size <- size_mapping[dd$Size]
# Check
for (i in 1:length(newShortSizes)) {
  print(sum(dd_original$Size == names(size_mapping)[i]) == sum(dd$Size == newShortSizes[i]))
}
print(sort(unique(dd$Size)) == sort(newShortSizes))


# Shorten the Warranty names
newShortWarranties <- c("1Y", "2Y", "3Y", "Lifetime")
# Map full warranty names to short warranty names
warranty_mapping <- setNames(newShortWarranties, c("1 Year", "2 Years", "3 Years", "Lifetime"))
# mappping
dd$Warranty <- warranty_mapping[dd$Warranty]
# Check
for (i in 1:length(newShortWarranties)) {
  print(sum(dd_original$Warranty == names(warranty_mapping)[i]) == sum(dd$Warranty == newShortWarranties[i]))
}
print(sort(unique(dd$Warranty)) == sort(newShortWarranties))

# Save the new data frame
write.csv(dd, "data/filtered_data.csv", row.names = FALSE)

print("File saved")

rm(dd)