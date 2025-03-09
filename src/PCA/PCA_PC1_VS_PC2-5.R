# Read data file
dd <- read.csv("data/filtered_data.csv", header = TRUE, sep = ",")

objects()
attributes(dd)

#Create graphics directory
if (!dir.exists("graficos/")){
  dir.create("graficos/")
}

#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

# CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES
names(dd)



# Check factor variables
sapply(dd,class)

#set a list of numerical variables excluding "Year" (with no missing values)
numeriques <- which(sapply(dd, is.numeric) & names(dd) != "Year")
numeriques
categoriques <- which(sapply(dd, is.character) | names(dd) == "Year")
categoriques

dcon<-dd[,numeriques]
sapply(dcon,class)

# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 <- prcomp(dcon)
class(pc1)
attributes(pc1)

print(pc1)

str(pc1)


# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)


# Cummulated Inertia in subspaces, from first principal component to the 8th dimension subspace
percInerAccum <- 100 * cumsum(pc1$sdev[1:dim(dcon)[2]]^2) / dim(dcon)[2]

png("graficos/cumulative_inertia_plot.png", width = 800, height = 600)

barplot(percInerAccum, 
        names.arg = paste0("PC", 1:length(percInerAccum)), 
        main = "Cumulative Inertia by Principal Components", 
        xlab = "Dimensions", 
        ylab = "Cumulative Inertia (%)")

abline(h = 80, col = "red", lwd = 2, lty = 2) 

dev.off()

percInerAccum



# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 5

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

#select your axis

#HACER UN FOR PARA RECORRER TODOS LOS PARES

eje1<-1
for (i in 2:5) {
  eje2<-i
  dir_name <- paste0("graficos/PC", eje1, "_VS_", eje2, "/")
  if (!dir.exists(dir_name)){
    dir.create(dir_name)
  }
  
  file_name <- paste0(dir_name, "Individuos_PC", eje1, "_PC", eje2, ".png")
  png(file_name, width = 800, height = 600)
  plot(Psi[,eje1],Psi[,eje2], type="n",
       main = paste("Individuals in PC", eje1, "vs PC", eje2), 
       xlab = paste0("Principal Component ", eje1), 
       ylab = paste0("Principal Component ", eje2))
  text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  dev.off()

  #Projection of variables
  
  Phi = cor(dcon,Psi)

  #select your axis

  X<-Phi[,eje1]
  Y<-Phi[,eje2]

  #zooms
  file_name <- paste0(dir_name, "Proyecciones_PC", eje1, "_PC", eje2, ".png")
  png(file_name, width = 800, height = 600)
  plot(Psi[,eje1],Psi[,eje2],type="n",
       xlim=c(min(X,0),max(X,0)), ylim=c(-1,1),
       main = paste("Variable Projections on PC", eje1, "vs PC", eje2), 
       xlab = paste0("Principal Component ", eje1), 
       ylab = paste0("Principal Component ", eje2))
  axis(side=1, pos= 0, labels = F)
  axis(side=3, pos= 0, labels = F)
  axis(side=2, pos= 0, labels = F)
  axis(side=4, pos= 0, labels = F)
  arrows(ze, ze, X, Y, length = 0.07,col="blue")
  text(X,Y,labels=etiq,col="darkblue", cex=0.7)
  dev.off()


  
  # PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
  
  for (var in names(categoriques)) {
    file_name <- paste0(dir_name, "Individuos_PC", eje1, "_PC", eje2, "_", var, ".png")
    png(file_name, width = 800, height = 600)
    
    varcat <- factor(dd[[var]])
    
    plot(Psi[, eje1], Psi[, eje2], col = varcat, pch = 16, 
         main = paste("Projection of", var, "on individuals' map"),
         xlab = paste0("Principal Component ", eje1), ylab = paste0("Principal Component ", eje2))
    
    axis(side=1, pos= 0, labels = F, col="darkgray")
    axis(side=3, pos= 0, labels = F, col="darkgray")
    axis(side=2, pos= 0, labels = F, col="darkgray")
    axis(side=4, pos= 0, labels = F, col="darkgray")
    levels(varcat)
    legend("bottomleft", levels(varcat), pch = 16, col = 1:length(levels(varcat)), cex = 0.6)
    
    #varcat<-factor(dd[,var])
    #fdic1 = tapply(Psi[,eje1],varcat,mean)
    #fdic2 = tapply(Psi[,eje2],varcat,mean)
    #text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=1, font = 2)
    
    dev.off()
  }
  
}

# 
# for (i in 2:5) {
#   eje2<-i
#   dir_name <- paste0("graficos/PC", eje1, "_VS_", eje2, "/")
#   if (!dir.exists(dir_name)){
#     dir.create(dir_name)
#   }
#   
#   #Now we project both cdgs of levels of a selected qualitative variable without
#   #representing the individual anymore
#   file_name <- paste0(dir_name, "ProdCat_PC", eje1, "_PC", eje2, ".png")
#   png(file_name, width = 800, height = 600)
#   
#   plot(Psi[,eje1],Psi[,eje2],type="n")
#   axis(side=1, pos= 0, labels = F, col="cyan")
#   axis(side=3, pos= 0, labels = F, col="cyan")
#   axis(side=2, pos= 0, labels = F, col="cyan")
#   axis(side=4, pos= 0, labels = F, col="cyan")
# 
#   #select your qualitative variable
#   k<-"ProdCat"
# 
#   varcat<-factor(dd[,k])
#   fdic1 = tapply(Psi[,eje1],varcat,mean)
#   fdic2 = tapply(Psi[,eje2],varcat,mean) 
# 
#   text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)
#   dev.off()
# 
#   #all qualitative together
#   plot(Psi[,eje1],Psi[,eje2],type="n")
#   axis(side=1, pos= 0, labels = F, col="cyan")
#   axis(side=3, pos= 0, labels = F, col="cyan")
#   axis(side=2, pos= 0, labels = F, col="cyan")
#   axis(side=4, pos= 0, labels = F, col="cyan")
# 
#   #nominal qualitative variables
#   
#   names(categoriques)
# 
#   dcat<-c("CustGender", "Country", "ProdCat", "Color", "EcoFriendly", "Insurance")
#   #divide categoricals in several graphs if joint representation saturates
# 
#   #build a palette with as much colors as qualitative variables 
# 
#   #colors<-c("blue","red","green","orange","darkgreen")
#   #alternative
#   colors<-rainbow(length(dcat))
# 
#   c<-1
#   for(k in dcat){
#     seguentColor<-colors[c]
#     fdic1 = tapply(Psi[,eje1],dd[,k],mean)
#     fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
#     
#     text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
#     c<-c+1
#   }
#   legend("bottomleft",names(dd[,dcat]),pch=1,col=colors, cex=0.6)
#   
#   #determine zoom level
#   #use the scale factor or not depending on the position of centroids
#   # ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
#   #fm = round(max(abs(Psi[,1]))) 
#   fm=20
#   
#   #scale the projected variables
#   #X<-fm*U[,eje1]
#   #Y<-fm*U[,eje2]
#   
#   #represent numerical variables in background
#   plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-3,1))
#   #plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
#   axis(side=1, pos= 0, labels = F, col="cyan")
#   axis(side=3, pos= 0, labels = F, col="cyan")
#   axis(side=2, pos= 0, labels = F, col="cyan")
#   axis(side=4, pos= 0, labels = F, col="cyan")
#   
#   #add projections of numerical variables in background
#   arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
#   text(X,Y,labels=etiq,col="gray", cex=0.7)
#   
#   #add centroids
#   c<-1
#   for(k in dcat){
#     seguentColor<-colors[c]
#     
#     fdic1 = tapply(Psi[,eje1],dd[,k],mean)
#     fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
#     
#     #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
#     text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
#     c<-c+1
#   }
#   legend("bottomleft",names(dd[,dcat]),pch=1,col=colors, cex=0.6)
#   
#   
#   #add ordinal qualitative variables. Ensure ordering is the correct
#   
#   dordi<-c("Month", "Year", "Size", "Warranty")
#   
#   levels(factor(dd[,dordi[1]]))
#   levels(factor(dd[,dordi[2]]))
#   levels(factor(dd[,dordi[3]]))
#   levels(factor(dd[,dordi[4]]))
#   #reorder modalities: when required
#   dd[,dordi[1]] <- factor(dd[,dordi[1]], ordered=TRUE,  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#   levels(dd[,dordi[1]])
#   dd[,dordi[2]] <- factor(dd[,dordi[2]], ordered=TRUE)
#   levels(dd[,dordi[2]])
#   dd[,dordi[3]] <- factor(dd[,dordi[3]], ordered=TRUE,  levels= c("S", "M", "L", "XL"))
#   levels(dd[,dordi[3]])
#   dd[,dordi[4]] <- factor(dd[,dordi[1]], ordered=TRUE, levels=c("1Y", "2Y", "3Y", "Lifetime"))
#   levels(dd[,dordi[4]])
#   
#   c<-1
#   col<-1
#   for(k in dordi){
#     seguentColor<-colors[col]
#     fdic1 = tapply(Psi[,eje1],dd[,k],mean)
#     fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
#     
#     #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
#     #connect modalities of qualitative variables
#     lines(fdic1,fdic2,pch=16,col=seguentColor)
#     text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
#     c<-c+1
#     col<-col+1
#   }
#   legend("topleft",names(dd[,dordi]),pch=1,col=colors[1:length(dordi)], cex=0.6)
# 
# }
# 
