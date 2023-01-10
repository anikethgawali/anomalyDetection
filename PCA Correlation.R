
library(readxl)
library(ggplot2)
library(MSQC)
Project_dataset_1 <- read_excel("C:/Users/gawal/Downloads/project_dataset.xlsx", col_names = FALSE)
dataset1 <- as.matrix.data.frame(Project_dataset_1)
dataset2 <- as.matrix.data.frame(dataset1)

? ## check the range of variables in dataset

#for(j in 1:209){
#  print(c(range(dataset2[,j]),j))
#}

#for(j in 1:209){
#   print(c(max(dataset2[,j])-min(dataset2[,j]),j))
#}

##---- Initialize variables ----

UCL<- NULL   #Upper Control Limit
LCL<- NULL   #Lower Control Limit
y<- NULL
out_up<-NULL  #Outlier - Above UCL
out_low<-NULL #Outlier - below LCL

n<- nrow(dataset2)
p<- ncol(dataset2)
S <- cov(dataset2)
print(n)


##---- Decide number of Principle components ----


pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("cumulitive proportions of variance:")
  print(cumsum(x.pvar[1:35]))
  
  par(mfrow=c(2,2))
  plot(x.pvar[1:20],xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar[1:20]),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x[1:20])
  screeplot(x[1:20],type="l")
  par(mfrow=c(1,1))
}


## Screeplot

#pdf(file= "PC Analysis Cor - screeplot.pdf" )

dataset2.PCA <- prcomp(dataset2, scale. = TRUE)
variance <- (dataset2.PCA$sdev)^2
var_explained = variance / sum(variance)
pcaCharts(dataset2.PCA)

#dev.off() 

## MDL Analysis

#pdf(file= "PC Analysis Cor - MDL.pdf" )

MDL<-c(rep(0,(p-1)))
l<- c(1:(p-1))
for (k in 1:(p-1))
{
  
  al<- mean(variance[(k+1):p]) 
  gl<- exp(mean(log(variance[(k+1):p])))
  MDL[k]<- n*(p-k)*log(al/gl)+k*(2*p-k)*log(n)/2
  
}
plot(l,MDL, xlab = "l", ylab = "MDL", col="red",)
optimum_MDL<-which.min(MDL)
optimum_MDL

#dev.off()

## ---- PCA Analysis using Covariance ----

#pdf(file= "PCA Cor.pdf" )

par(mfrow=c(2,2))
pc <- 4   #no of PC used

for (j in 1:30){

  n<- nrow(dataset2)        # no of rows
  #S = cor(dataset2)
  x1<- matrix(1:n,nrow = 1,ncol = n)
  dataset2.PCA <- prcomp(dataset2, scale = TRUE)
  variance <- dataset2.PCA$sdev^2
  scores <- dataset2.PCA$x
  
  for (i in 1:pc){
    UCL[i]<- 3*sqrt(variance[i])
    LCL[i]<- -3*sqrt(variance[i])
    out_up[i]<-list(which(t(scores[,i])>UCL[i]))
    out_low[i]<-list(which(t(scores[,i])<LCL[i]))
    plot(x1,scores[,i], col="blue", main = paste("Iteration J = ",j), xlab = "X", ylab =paste("PC",i))
    lines(x1, rep(UCL[[i]],n), col="red",lty=2)
    lines(x1,rep(LCL[[i]],n),col="red",lty=2)
  }
  
  outliers <- unlist(c(out_up,out_low))
  list(outliers)
  if (is.null(outliers) | length(outliers)==0)
    break
  dataset2<-dataset2[-outliers,]
  print(outliers)
}
dim(dataset2)
dataset2.PCA2 <- prcomp(dataset2, scale = TRUE)
var_explained2 = dataset2.PCA2$sdev^2 / sum(dataset2.PCA2$sdev^2)
sum(var_explained2[1:pc])
sum(var_explained[1:pc])
UCL
LCL
#dev.off()

#---- mCUSUM Analysis ---- 

#pdf(file= "mCUSUM cor.pdf" )

par(mfrow=c(2,2))


Project3 <- as.matrix.data.frame(dataset1)
project3.PCA <- prcomp(Project3, scale = TRUE)


  for (i in 1:10){
    
    project3.PCA <- prcomp(Project3, scale = TRUE)
    df.pca <- project3.PCA$x[,1:pc]
    df.pca <- as.data.frame(df.pca)
    mcusum <- mult.chart(x = df.pca, type = "mcusum2", alpha=0.000027, k = 1, h = 20, method = "sw")
    outliers.cusum <- which(mcusum$t2 > mcusum$ucl)
    if (is.null(outliers.cusum) | length(outliers.cusum)==0)
      break
    Project3 <- Project3[-outliers.cusum,]
    print(outliers.cusum)
  }
var_explained.cusum = project3.PCA$sdev^2 / sum(project3.PCA$sdev^2)
#print("iteration = ", j)
print(dim(Project3))
print(sum(var_explained.cusum[1:pc]))
#}
#dev.off()


