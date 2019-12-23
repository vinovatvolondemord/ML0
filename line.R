library(MASS)
normalize = function(xl) {
  cols = dim(xl)[2]
  
  for (i in 1:cols) {
    xl[, i] = (xl[, i] - min(xl[, i])) / (max(xl[, i]) - min(xl[, i]))
  }
  return(xl)
}
for(i in 1:dim(xl)[1]){
  classes[i] = normalisation(i)	
}
classes
lstandart = function(x) x;
adaline = function(x)  (x - 1) ^ 2
adalineW = function(w, eta, xi, yi) w - eta * (sum(w * xi) - yi) * xi


perceptron = function(x)  max(-x, 0)
perceptronW = function(w, eta, xi, yi)   w + eta * yi * xi

regression = function(x)  log2(1 + exp(-x))
regressionW = function(w, eta, xi, yi) {
  sigmoid = function(z) {
    1 / (1 + exp(-z))
  }  
  w + eta * xi * yi * sigmoid(-sum(w * xi) * yi)
}

stohastGrad = function(xl,classes,L, rule){
  rows = dim(xl)[1]
  cols = dim(xl)[2]
  
  w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
  
  lambda = 1 / rows
  
  Q = 0
  for (i in 1:rows) {
    margin = sum(w * xl[i,]) * classes[i]
    Q[1] = Q[1] + L(margin)
  }    
  cnt = 0
  for(j in 2:1000) {
    cnt = cnt + 1

    margins = rep(0, rows)
    for (i in 1:rows) {
      xi = xl[i,]
      yi = classes[i]
      margins[i] = sum(w * xi) * yi
    }
    errorIndex = which(margins <= 0)
    

    
    if(length(errorIndex)!=0)
      i = sample(errorIndex, 1)
    else
      i = sample(1:rows,1)
    
    xi = xl[i,]
    yi = classes[i]
    

    margin = sum(w * xi) * yi
    error = L(margin)
 
    eta = 1 / cnt
    w = rule(w, eta, xi, yi)
    
    
    Q[j] = (1 - lambda) * Q[j-1] + lambda * error
    
    if (abs(Q[j-1] - Q[j]) / abs(max(Q[j-1], Q[j])) < 1e-5)
      break;
    drawLine(w, "green")
  }  
  return(w)
}
drawLine = function(w, color) {
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 1, col = color)
}
n <- 100
sigma1 <- matrix(c(1, 0, 0, 1),2,2)
sigma2 <- matrix(c(1, 0, 0, 1),2,2)

mu1 <- c(0,4)
mu2 <- c(4,0)

x1 <- mvrnorm(n = objectCounter, mu1, sigma1)
x2 <- mvrnorm(n = objectCounter, mu2, sigma2)
xl <- rbind(x1,x2)
xl <- normalize(xl)
xl <- cbind(xl, rep(-1, n+n))
xl <- cbind(xl, c(rep(-1, n), rep(1, n)))

colorsn <- c(rep(2, n), rep(1, n))
classes <- c(rep(-1, n), rep(1, n))
colors <- c("red", "blue")

plot(xl[,1],xl[,2], pch = 21,main = "ADALINE", col = colors[colorsn], asp = 1, bg=colors[colorsn])

w1 <- stohastGrad(xl, classes, adaline, adalineW)
#w2 = stohastGrad(xl, classes, regression, regressionW)
#drawLine(w2, "orange") 
drawLine(w1, "black")
