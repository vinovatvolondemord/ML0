objectCounter <- 100
library(MASS)
normalize = function(xl) {
  cols = dim(xl)[2]
  
  for (i in 1:cols) {
    xl[, i] = (xl[, i] - min(xl[, i])) / (max(xl[, i]) - min(xl[, i]))
  }
  return(xl)
}

sigma1 <- matrix(c(1, 0, 0, 1),2,2)
sigma2 <- matrix(c(1, 0, 0, 1),2,2)

mu1 <- c(0,4)
mu2 <- c(4,0)

x1 <- mvrnorm(n = objectCounter, mu1, sigma1)
x2 <- mvrnorm(n = objectCounter, mu2, sigma2)

xy1 <- cbind(x1,1) 
xy2 <- cbind(x2,2) 
  
xl <- rbind(xy1,xy2)
	
colors <- c("red", "blue")
plot(xl[,1],xl[,2], pch = 21,main = "Стохастического Градиента", col = colors[xl[,3]], asp = 1, bg=colors[xl[,3]])
normalisation = function(i){
	if (xl[i,3] == 1)
		return (-1)
	else 
		return (1)
}
for(i in 1:dim(xl)[1]){
	classes[i] = normalisation(i)	
}

lstandart = function(x) x;
adaline = function(x)  (x - 1) ^ 2
adalineW = function(w, eta, xi, yi) {
  w - eta * (sum(w * xi) - yi) * xi
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
      
      # выбрать объекты с ошибкой
      margins = rep(0, rows)
      for (i in 1:rows) {
        xi = xl[i,]
        yi = classes[i]
        margins[i] = sum(w * xi) * yi
      }
      errorIndex = which(margins <= 0)
     
      # выбираем случайный ошибочный объект
    
      if(length(errorIndex)!=0)
      i = sample(errorIndex, 1)
      else
      i = sample(1:rows,1)
      
      xi = xl[i,]
      yi = classes[i]
      
      # высчитываем ошибку
      margin = sum(w * xi) * yi
      error = L(margin)
      
      # обновляем веса
      eta = 1 / cnt
      w = rule(w, eta, xi, yi)
      
      # новое Q
	
      Q[j] = (1 - lambda) * Q[j-1] + lambda * error
      
      # выходим, если Q стабилизировалось
      if (abs(Q[j-1] - Q[j]) / abs(max(Q[j-1], Q[j])) < 1e-5)
        break;
	
    }  
    return(w)
  }
drawLine = function(w, color) {
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 2, col = color)
  }
w1 = stohastGrad(xl, classes, adaline, adalineW)
drawLine(w1, "brown")
