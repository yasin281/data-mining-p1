# Read data file
dd <- read.csv("data/filtered_data.csv", header = TRUE, sep = ",")

# Create graphics directory
if (!dir.exists("graficos/")){
  dir.create("graficos/")
}

# Identify numeric and categorical variables
numeriques <- which(sapply(dd, is.numeric) & names(dd) != "Year")
categoriques <- which(sapply(dd, is.character) | names(dd) == "Year")

# Select numerical data
dcon <- dd[, numeriques]

# Principal Component Analysis
pc1 <- prcomp(dcon, scale = TRUE)

# Inertia explained by each component
inerProj <- pc1$sdev^2 
totalIner <- sum(inerProj)
pinerEix <- 100 * inerProj / totalIner

# Scree plot
barplot(pinerEix)

# Cumulative inertia
percInerAccum <- 100 * cumsum(pc1$sdev[1:dim(dcon)[2]]^2) / dim(dcon)[2]

png("graficos/cumulative_inertia_plot.png", width = 800, height = 600)
barplot(percInerAccum, 
        names.arg = paste0("PC", 1:length(percInerAccum)), 
        main = "Cumulative Inertia by Principal Components", 
        xlab = "Dimensions", 
        ylab = "Cumulative Inertia (%)")
abline(h = 80, col = "red", lwd = 2, lty = 2) 
dev.off()

# Select number of dimensions (80% of inertia)
nd <- 5
Psi <- pc1$x[, 1:nd] # Projections
iden <- row.names(dcon)
etiq <- names(dcon)
ze <- rep(0, length(etiq))

# Loop through axes combinations
for(i in 1:4) {
  eje1 <- i
  for (j in (i+1):5) {
    eje2 <- j
    dir_name <- paste0("graficos/PC", eje1, "_VS_", eje2, "/")
    if (!dir.exists(dir_name)){
      dir.create(dir_name)
    }
    
    # Plot individuals
    file_name <- paste0(dir_name, "Individuos_PC", eje1, "_PC", eje2, ".png")
    png(file_name, width = 800, height = 600)
    plot(Psi[,eje1], Psi[,eje2], type="n",
         main = paste("Individuals in PC", eje1, "vs PC", eje2), 
         xlab = paste0("Principal Component ", eje1), 
         ylab = paste0("Principal Component ", eje2))
    text(Psi[,eje1], Psi[,eje2], labels = iden, cex = 0.5)
    axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
    axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
    axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
    axis(side = 4, pos = 0, labels = FALSE, col = "cyan")
    dev.off()
    
    # Projections of variables
    Phi <- cor(dcon, Psi)
    X <- Phi[, eje1]
    Y <- Phi[, eje2]
    
    file_name <- paste0(dir_name, "Proyecciones_PC", eje1, "_PC", eje2, ".png")
    png(file_name, width = 800, height = 600)
    plot(Psi[,eje1], Psi[,eje2], type = "n",
         xlim = c(min(X, 0), max(X, 0)), ylim = c(-1, 1),
         main = paste("Variable Projections on PC", eje1, "vs PC", eje2), 
         xlab = paste0("Principal Component ", eje1), 
         ylab = paste0("Principal Component ", eje2))
    axis(side = 1, pos = 0, labels = FALSE)
    axis(side = 2, pos = 0, labels = FALSE)
    arrows(ze, ze, X, Y, length = 0.07, col = "blue")
    text(X, Y, labels = etiq, col = "darkblue", cex = 0.7)
    dev.off()
    
    # Projection of qualitative variables
    for (var in names(categoriques)) {
      file_name <- paste0(dir_name, "Individuos_PC", eje1, "_PC", eje2, "_", var, ".png")
      png(file_name, width = 800, height = 600)
      
      varcat <- factor(dd[[var]])
      plot(Psi[, eje1], Psi[, eje2], col = varcat, pch = 16, 
           main = paste("Projection of", var, "on individuals' map"),
           xlab = paste0("Principal Component ", eje1), 
           ylab = paste0("Principal Component ", eje2))
      axis(side = 1, pos = 0, labels = FALSE, col = "darkgray")
      axis(side = 2, pos = 0, labels = FALSE, col = "darkgray")
      legend("bottomleft", levels(varcat), pch = 16, col = 1:length(levels(varcat)), cex = 0.6)
      dev.off()
    }
  }
}

# Recalculate Phi outside the loop just in case
Phi <- cor(dcon, Psi)

# Select principal components to project
eje_zoom1 <- 1  # PC1
eje_zoom2 <- 2  # PC2

# Coordinates of all variables on PC1 and PC2
X_zoom <- Phi[, eje_zoom1]
Y_zoom <- Phi[, eje_zoom2]

# Variables to highlight
subVars <- c("UnitPrice", "Discount", "OrdQty")
X_sub <- Phi[subVars, eje_zoom1]
Y_sub <- Phi[subVars, eje_zoom2]

# Create output folder for final plot
if (!dir.exists("graficos/final/")) {
  dir.create("graficos/final/", recursive = TRUE)
}

# Save the plot
png("graficos/final/Highlighted_Variables_PC1_PC2.png", width = 800, height = 600)
plot(Psi[, eje_zoom1], Psi[, eje_zoom2], type = "n",
     xlim = c(min(X_zoom, 0), max(X_zoom, 0)), ylim = c(-1, 1),
     main = "Projection of Variables (PC1 vs PC2)",
     xlab = "PC1", ylab = "PC2")

# Draw axis
axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
axis(side = 4, pos = 0, labels = FALSE, col = "cyan")

# Draw all variable vectors in grey
arrows(0, 0, X_zoom, Y_zoom, length = 0.07, col = "grey")
text(X_zoom, Y_zoom, labels = etiq, col = "grey", cex = 0.7)

# Highlight selected variables in green
arrows(0, 0, X_sub, Y_sub, length = 0.07, col = "green")
text(X_sub, Y_sub, labels = subVars, col = "green", cex = 0.9, font = 2)

dev.off()

# Select principal components to project
eje_1 <- 1
eje_2 <- 2

# Coordinates of all variables on PC1 and PC2
X <- Phi[,eje_1]
Y <- Phi[,eje_2]

#Select data from ProdCat to show centroids PC1 vs PC2
varcat <- factor(dd[,6])

# Calculate centroids for each group
fdic1 <- tapply(Psi[,eje_1], varcat, mean) 
fdic2 <- tapply(Psi[,eje_2], varcat, mean) 

png("graficos/final/Projection_With_Centroids_PC1_PC2.png", width = 800, height = 600)
plot(Psi[, eje_1], Psi[, eje_2], type = "n",
     main = paste("Variable Projection and Centroids: PC", eje_1, "vs PC", eje_2),
     xlab = paste("PC", eje_1), ylab = paste("PC", eje_2))
axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
axis(side = 4, pos = 0, labels = FALSE, col = "cyan")

# Draw arrows representing the variable loadings
arrows(ze, ze, X, Y, length = 0.07, col = "blue")
text(X, Y, labels = etiq, col = "darkblue", cex = 0.7)

# Plot the centroids for each group and label them
points(fdic1, fdic2, pch = 16, col = "red", cex = 1.2)
text(fdic1, fdic2, labels = names(fdic1), col = "black", cex = 1.5, font = 2, pos = 3)

dev.off()

