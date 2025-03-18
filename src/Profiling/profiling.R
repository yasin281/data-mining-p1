#read data 
dd <- read.csv("filtered_data.csv", sep = ",", stringsAsFactors = TRUE)

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}

ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}

#P contains class variable (which cluster an individual belongs to)
P<-c1
nameP<-"classe"

nc<-length(levels(factor(P)))
nc
K<-18
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dd)))
nameP<-"Class"
n<-dim(dd)[1]

if(!dir.exists("profilingImages")) dir.create("profilingImages")
setwd("../profilingImages")

for(k in 1:K){
  if (is.numeric(dd[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dd)[k]))
    
    png(paste("profilingImages/", "Boxplot", names(dd)[k], "Vs", nameP, ".png",sep=""))
    boxplot(dd[,k]~P, main=paste("Boxplot of", names(dd)[k], "vs", nameP ), horizontal=TRUE, xlab=names(dd)[k], ylab=nameP)
    dev.off()
    
    png(paste("profilingImages/", "Means", names(dd)[k], "By", nameP, ".png",sep=""))
    barplot(tapply(dd[[k]], P, mean),main=paste("Means of", names(dd)[k], "by", nameP ))
    dev.off();
    
    abline(h=mean(dd[[k]]))
    legend(0,mean(dd[[k]]),"global mean",bty="n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dd[P==s,k]))}
    o<-oneway.test(dd[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dd[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dd[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    if(class(dd[,k])=="Date"){
      print(summary(dd[,k]))
      print(sd(dd[,k]))
      #decide breaks: weeks, months, quarters...
      hist(dd[,k],breaks="weeks")
    }else{
      #qualitatives
      print(paste("Variable", names(dd)[k]))
      table<-table(P,dd[,k])
      #   print("Cross-table")
      #   print(table)
      rowperc<-prop.table(table,1)
      
      colperc<-prop.table(table,2)
      #  print("Distribucions condicionades a files")
      # print(rowperc)
      
      #ojo porque si la variable es true o false la identifica amb el tipus Logical i
      #aquest no te levels, por tanto, coertion preventiva
      
      dd[,k]<-as.factor(dd[,k])
      
      
      marg <- table(as.factor(P))/n
      print(append("Categories=",levels(as.factor(dd[,k]))))
      
      #from next plots, select one of them according to your practical case
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]))
      paleta<-rainbow(length(levels(dd[,k])))
      for(c in 1:length(levels(dd[,k]))){lines(colperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]))
      paleta<-rainbow(length(levels(dd[,k])))
      for(c in 1:length(levels(dd[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(dd[,k]), col=paleta, lty=2, cex=0.6)
      
      #condicionades a classes
      print(append("Categories=",levels(dd[,k])))
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]))
      paleta<-rainbow(length(levels(dd[,k])))
      for(c in 1:length(levels(dd[,k]))){lines(rowperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]))
      paleta<-rainbow(length(levels(dd[,k])))
      for(c in 1:length(levels(dd[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(dd[,k]), col=paleta, lty=2, cex=0.6)
      
      #amb variable en eix d'abcisses
      marg <-table(dd[,k])/n
      print(append("Categories=",levels(dd[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]), las=3)
      #x<-plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]), xaxt="n")
      #text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      #condicionades a columna 
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]), las=3)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dd)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      table<-table(dd[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)
      
      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(dd[,k])))
      barplot(table(dd[,k], as.factor(P)), beside=FALSE,col=paleta )
      
      barplot(table(dd[,k], as.factor(P)), beside=FALSE,col=paleta )
      legend("topright",levels(as.factor(dd[,k])),pch=1,cex=0.5, col=paleta)
      
      #diagrames de barres adosades
      barplot(table(dd[,k], as.factor(P)), beside=TRUE,col=paleta )
      
      barplot(table(dd[,k], as.factor(P)), beside=TRUE,col=paleta)
      legend("topright",levels(as.factor(dd[,k])),pch=1,cex=0.5, col=paleta)
      
      print("Test Chi quadrat: ")
      print(chisq.test(dd[,k], as.factor(P)))
      
      print("valorsTest:")
      print( ValorTestXquali(P,dd[,k]))
      #calcular els pvalues de les quali
    }
  }
}#endfor

#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}

#afegir la informacio de les modalitats de les qualitatives a la llista de pvalues i fer ordenacio global

