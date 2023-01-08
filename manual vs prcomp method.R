set.seed(2)             #sets the seed for random number generation.
x <- 1:100              #creates a vector x with numbers from 1 to 100
ex <- rnorm(100, 0, 30) #100 normally distributed random numbers, mean=0, sd=30
ey <- rnorm(100, 0, 30) # 100 normally distributed random numbers, mean=0, sd=30
y <- 30 + 2 * x         #sets y to be a vector that is a linear function of x
x_obs <- x + ex         #adds "noise" to x
y_obs <- y + ey         #adds "noise" to y

# Bind both vectors in a matrix of toy data called P
P <- data.frame(x_obs=x_obs,y_obs=y_obs) #places points in matrix
summary(P)


## Manual Method
MCov <- cov(P) 
n<- nrow(P)        # no of rows
p<- ncol(P)        # no of variables
mu<-colMeans((P))  # 

mean_matrix<- matrix(rep(mu,n), nrow = n, ncol = p, byrow =TRUE)
y1<- t(eigen(MCov)$vectors[,1])%*%t(P - mean_matrix)
head(y1)


## prcomp method

pca<-prcomp(P)$x
pca[,1]
