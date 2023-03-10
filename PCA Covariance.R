library(readxl)
library(ggplot2)
library(MSQC)
Project_dataset_1 <- read_excel("C:/Users/gawal/Downloads/project_dataset.xlsx", col_names = FALSE)
Project1 <- as.matrix.data.frame(Project_dataset_1)
Project2 <- as.matrix.data.frame(Project1)

#---- Initialize variables ----

UCL<- NULL
LCL<- NULL
y<- NULL
out_up<-NULL
out_low<-NULL

S = cov(Project2)
n<- nrow(Project2)
p<- ncol(Project2)
mu<-colMeans((Project2))

#---- Decide number of Principle components ----


pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("cumulitive proportions of variance:")
  print(cumsum(x.pvar[1:35]))
  
  par(mfrow=c(2,2))
  plot(x.pvar[1:20],xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar[1:20]),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}



### Screeplot

#pdf(file= "PC Analysis - screeplot.pdf" )

project2.PCA <- princomp(Project2, cor = FALSE)
var_explained = project2.PCA$sdev^2 / sum(project2.PCA$sdev^2)
pcaCharts(project2.PCA)

#dev.off() 

### MDL Analysis

#pdf(file= "PC Analysis - MDL.pdf" )

eigen_values <- eigen(S)$values
MDL<-c(rep(0,208))
l<- c(1:208)
for (k in 1:208)
{
  
  al<- mean(eigen_values[(k+1):209]) 
  gl<- exp(mean(log(eigen_values[(k+1):209])))
  MDL[k]<- n*(209-k)*log(al/gl)+k*(2*209-k)*log(n)/2
  
}
plot(l,MDL, xlab = "l", ylab = "MDL", col="red",)
optimum_MDL<-which.min(MDL)
optimum_MDL

#dev.off()

#---- PCA Analysis using Covariance ----

#pdf(file= "PCA Cov.pdf" )
par(mfrow=c(2,2))

p <- 4   #no of PC used

for (j in 1:20){

  n<- nrow(Project2)        # no of rows
  S = cov(Project2)
  x1<- matrix(1:n,nrow = 1,ncol = n)
  y<- princomp(Project2, cor = FALSE)$scores
  
  for (i in 1:p){
    UCL[i]<- 3*sqrt(eigen(S)$values[i])
    LCL[i]<- -3*sqrt(eigen(S)$values[i])
    out_up[i]<-list(which(t(y[,i])>UCL[i]))
    out_low[i]<-list(which(t(y[,i])<LCL[i]))
    plot(x1,y[,i], col="blue", main = paste("Iteration J = ",j), xlab = "X", ylab =paste("PC",i))
    lines(x1, rep(UCL[[i]],n), col="red",lty=2)
    lines(x1,rep(LCL[[i]],n),col="red",lty=2)
  }
  
  outliers <- unlist(c(out_up,out_low))
  list(outliers)
  if (is.null(outliers) | length(outliers)==0)
    break
  Project2<-Project2[-outliers,]
  print(outliers)
}
dim(Project2)
project2.PCA2 <- princomp(Project2, cor = FALSE)
var_explained2 = project2.PCA2$sdev^2 / sum(project2.PCA2$sdev^2)
sum(var_explained2[1:p])
sum(var_explained[1:p])

#dev.off()

#---- mCUSUM Analysis ---- 

#pdf(file= "mCUSUM.pdf" )
par(mfrow=c(2,2))

k <- list(0.5,0.7,1.0,1.2,1.5, 1.7, 2)  # various values of k tested.


  Project3 <- as.matrix.data.frame(Project1)
  Project3.centered <- scale(Project3, scale = F, center = T)
  project3.PCA <- princomp(Project3.centered, cor = FALSE)


  for (i in 1:10){
    
    project3.PCA <- princomp(Project3.centered, cor = FALSE)
    df.pca <- project3.PCA$scores[,1:p]
    df.pca <- as.data.frame(df.pca)
    mcusum <- mult.chart(x = df.pca, type = "mcusum2", alpha=0.0027, k = 1, h = 20, method = "sw")
    outliers.cusum <- which(mcusum$t2 > mcusum$ucl)
    if (is.null(outliers.cusum) | length(outliers.cusum)==0)
      break
    Project3.centered <- Project3.centered[-outliers.cusum,]
    #print(outliers.cusum)
  }
var_explained.cusum = project3.PCA$sdev^2 / sum(project3.PCA$sdev^2)
#print("iteration = ", j)
print(dim(Project3.centered))
print(sum(var_explained.cusum[1:p]))

#dev.off()


