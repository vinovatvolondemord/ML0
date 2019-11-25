dotsCounter <- 100

library(MASS)

sigma1 <- matrix(c(2, 0, 0, 2),2,2)
sigma2 <- matrix(c(1, 0, 0, 1),2,2)
mu1 <- c(0,0)
mu2 <- c(4,4)

x1 <- mvrnorm(n = dotsCounter, mu1, sigma1)
x2 <- mvrnorm(n = dotsCounter, mu2, sigma2)

xy1 <- cbind(x1,1) 
xy2 <- cbind(x2,2) 

xl <- rbind(xy1,xy2)
xl
colors <- c("blue", "red")
plot(xl[,1],xl[,2], pch = 21,main = "Наивный нормальный байесовский классификатор", col = colors[xl[,3]], asp = 1, bg=colors[xl[,3]])


baiesNaiv <- function(x, mu, sigma, lamda, P){
  n <- 2
  myfunction <- log(lamda*P)
  pyj<-0
  for(i in 1 : n){
    pyj <- pyj + log( (1/(sigma[i]*sqrt(2*pi))) * exp(-1 * ((x[i] - mu[i])^2)/(2*sigma[i]^2)))
    
  }
  myfunction <- myfunction + pyj
  return(myfunction)
}

muCreat <- function(xl) (c(sum(xl[,1])/(dim(xl)[1]), sum(xl[,2])/(dim(xl)[1])))

sigmaCreat <- function(xl, mu) (c(sum((xl[,1] - mu[1])^2)/(dim(xl)[1]), sum((xl[,2] - mu[2])^2)/(dim(xl)[1])))

mu1 <- muCreat(x1)
mu2 <- muCreat(x2)     

sigma1 <- sigmaCreat(x1, mu1)
sigma2 <- sigmaCreat(x2, mu2)

x1 <- -10;

while(x1 < 15){
  x2 <- -5;
  
  while(x2 < 10){          
    
    color <- 0;
    
    if(baiesNaiv(c(x1,x2), mu1, sigma1, 1, 0.5) > baiesNaiv(c(x1,x2), mu2, sigma2, 1, 0.5)){
      color <- 1
    } 
    else {
      color <- 2
    }
    
    points(x1, x2, pch = 21, col=colors[color], asp = 1)
    x2 <- x2 + 0.2
  }
  x1 <- x1 + 0.2
}
