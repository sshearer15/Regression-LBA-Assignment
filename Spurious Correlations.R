# Sally Shearer

library("ggplot2")

par(mfrow=c(2,2))
n <- c(100, 250, 500, 1000)
for (j in n){
  mycor <- rep(0,1000)
  for (i in 1:1000){
    x <- rnorm(j)
    y <- rnorm(j)
    mycor[i] <- cor(x,y)
  }
  hist(mycor, xlim = c(-.4, .4), main = paste('Sample size:', j))
}

