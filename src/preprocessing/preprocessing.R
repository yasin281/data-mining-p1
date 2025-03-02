#missings treatment

dd <- read.csv("filtered_data.csv", header = TRUE, sep = ",")

table(dd$Insurance, useNA="ifany")

#visually we haven't seen missings apart from Insurance which is a structural missing
colSums(is.na(dd))

dd$Insurance[dd$Insurance == ""] <- "None"

table(dd$Insurance)

barplot(table(dd$Insurance))

write.table(dd, file = "data_without_missings.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

#-------------------------------------------------------------------------------
#numerical variables normalization

dd <- read.csv("data_without_missings.csv", header = TRUE, sep = ",")

dd$CustAgeNormalized <- scale(dd$CustAge)
hist(dd$CustAgeNormalized)
summary(dd$CustAgeNormalized)
dd$CustAge <- NULL

dd$OrdQtyNormalized <- scale(dd$OrdQty)
hist(dd$OrdQtyNormalized)
summary(dd$OrdQtyNormalized)
dd$OrdQty <- NULL

#also perform logarithmic transformation to remove the gap
dd$UnitCostNormalized <- scale(log(dd$UnitCost))
hist(dd$UnitCostNormalized)
summary(dd$UnitCostNormalized)
dd$UnitCost <- NULL

#also perform logarithmic transformation to remove the gap
dd$UnitPriceNormalized <- scale(log(dd$UnitPrice))
hist(dd$UnitPriceNormalized)
summary(dd$UnitPriceNormalized)
dd$UnitPrice <- NULL

dd$DiscountNormalized <- scale(dd$Discount)
hist(dd$DiscountNormalized)
summary(dd$DiscountNormalized)
dd$Discount <- NULL

dd$ShipWeightNormalized <- scale(dd$ShipWeight)
hist(dd$ShipWeightNormalized)
summary(dd$ShipWeightNormalized)
dd$ShipWeight <- NULL

dd$ShipCostNormalized <- scale(dd$ShipCost)
hist(dd$ShipCost)
summary(dd$ShipCostNormalized)
dd$ShipCost <- NULL

dd$DelTimeNormalized <- scale(dd$DelTime)
hist(dd$DelTimeNormalized)
summary(dd$DelTimeNormalized)
dd$DelTime <- NULL

write.table(dd, file = "data_without_missings_and_numericals_normalized.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

#-------------------------------------------------------------------------------
#outlier treatment

dd <- read.csv("data_without_missings_and_numericals_normalized.csv", header = TRUE, sep = ",")

#visually we haven't seen any outliers apart from Country
barplot(table(dd$Country))

dd <- dd[dd$Country != "PT",]

barplot(table(dd$Country))

write.table(dd, file = "data_without_missings_and_numericals_normalized_and_outliers_removed.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

#-------------------------------------------------------------------------------
# adding new variables

dd <- read.csv("data_without_missings_and_numericals_normalized_and_outliers_removed.csv", header = TRUE, sep = ",")

#to do

write.table(dd, file = "data_without_missings_and_numericals_normalized_and_outliers_removed_with_new_variables.csv"
            , sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

#-------------------------------------------------------------------------------

# categorical variables transformation OHE

dd <- read.csv("data_without_missings_and_numericals_normalized_and_outliers_removed_with_new_variables.csv"
               ,header = TRUE, sep = ",")

dd$MonthOHE <- model.matrix(~ Month - 1, data = dd)
print(dd$MonthOHE)
dd$Month <- NULL

Year <- as.factor(dd$Year)
dd$YearOHE <- model.matrix(~ Year - 1, data = dd)
print(dd$YearOHE)
dd$Year <- NULL

dd$CustGenderOHE <- model.matrix(~ CustGender - 1, data = dd)
print(dd$CustGenderOHE)
dd$CustGender <- NULL

dd$CountryOHE <- model.matrix(~ Country - 1, data = dd)
print(dd$CountryOHE)
dd$Country <- NULL

dd$ProdCatOHE <- model.matrix(~ ProdCat - 1, data = dd)
print(dd$ProdCatOHE)
dd$ProdCat <- NULL

dd$SizeOHE <- model.matrix(~ Size - 1, data = dd)
print(dd$SizeOHE)
dd$Size <- NULL

dd$ColorOHE <- model.matrix(~ Color - 1, data = dd)
print(dd$ColorOHE)
dd$Color <- NULL

dd$WarrantyOHE <- model.matrix(~ Warranty - 1, data = dd)
print(dd$WarrantyOHE)
dd$Warranty <- NULL

dd$EcoFriendlyOHE <- model.matrix(~ EcoFriendly - 1, data = dd)
print(dd$EcoFriendlyOHE)
dd$EcoFriendly <- NULL

dd$InsuranceOHE <- model.matrix(~ Insurance - 1, data = dd)
print(dd$InsuranceOHE)
dd$Insurance <- NULL

write.table(dd, file = "data_without_missings_and_numericals_normalized_and_outliers_removed_with_new_variables_categorical_transformed.csv"
            , sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

#-------------------------------------------------------------------------------

dd <- read.csv("data_without_missings_and_numericals_normalized_and_outliers_removed_with_new_variables_categorical_transformed.csv"
               ,header = TRUE, sep = ",")



write.table(dd, file = "processed_data.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)