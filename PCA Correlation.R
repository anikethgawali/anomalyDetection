
#Project_dataset_1 <- read("Project_dataset (1).xlsx", col_names = FALSE)
#install.packages("MSQC")
library(readxl)
library(ggplot2)
library(MSQC)
Project_dataset_1 <- read_excel("C:/Users/gawal/Downloads/project_dataset.xlsx", col_names = FALSE)
Project1 <- as.matrix.data.frame(Project_dataset_1)
Project2 <- as.matrix.data.frame(Project1)
### check the range of variables in dataset

# for(j in 1:209){
# print(c(range(Project2[,j]),j))
# }

# for(j in 1:209){
#   print(c(max(Project2[,j])-min(Project2[,j]),j))
# }

#---- Initialize variables ----

UCL<- NULL
LCL<- NULL
y<- NULL
out_up<-NULL
out_low<-NULL

n<- nrow(Project2)
p<- ncol(Project2)

S <- cov(Project2)
eigen(S)$vectors

prcomp(Project2)$rotation

#---- Decide number of Principle components ----


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



### Screeplot
pdf(file= "PC Analysis Cor - screeplot.pdf" )

project2.PCA <- prcomp(Project2, scale. = TRUE)
variance <- (project2.PCA$sdev)^2
var_explained = variance / sum(variance)
pcaCharts(project2.PCA)

dev.off() 

### MDL Analysis

#pdf(file= "PC Analysis Cor - MDL.pdf" )

MDL<-c(rep(0,208))
l<- c(1:208)
for (k in 1:208)
{
  
  al<- mean(variance[(k+1):209]) 
  gl<- exp(mean(log(variance[(k+1):209])))
  MDL[k]<- n*(209-k)*log(al/gl)+k*(2*209-k)*log(n)/2
  
}
plot(l,MDL, xlab = "l", ylab = "MDL", col="red",)
optimum_MDL<-which.min(MDL)
optimum_MDL

#dev.off()

#---- PCA Analysis using Covariance ----

pdf(file= "PCA Cor.pdf" )
par(mfrow=c(2,2))

p <- 4   #no of PC used

for (j in 1:30){

  n<- nrow(Project2)        # no of rows
  #S = cor(Project2)
  x1<- matrix(1:n,nrow = 1,ncol = n)
  project2.PCA <- prcomp(Project2, scale = TRUE)
  variance <- project2.PCA$sdev^2
  scores <- project2.PCA$x
  
  for (i in 1:p){
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
  Project2<-Project2[-outliers,]
  print(outliers)
}
dim(Project2)
project2.PCA2 <- prcomp(Project2, scale = TRUE)
var_explained2 = project2.PCA2$sdev^2 / sum(project2.PCA2$sdev^2)
sum(var_explained2[1:p])
sum(var_explained[1:p])
UCL
LCL
dev.off()

#---- mCUSUM Analysis ---- 

pdf(file= "mCUSUM cor.pdf" )
par(mfrow=c(2,2))

#k <- list(0.5,0.7,1.0,1.2,1.5, 1.7, 2)  # various values of k tested.

#for (j in 1:7){

  Project3 <- as.matrix.data.frame(Project1)
  project3.PCA <- prcomp(Project3, scale = TRUE)


  for (i in 1:10){
    
    project3.PCA <- prcomp(Project3, scale = TRUE)
    df.pca <- project3.PCA$x[,1:p]
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
print(sum(var_explained.cusum[1:p]))
#}
dev.off()


