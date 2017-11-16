# Sally Shearer

library("ggplot2")

# revised replication of professor's in class function
n <- c(100)
set.seed(1)
for (j in n){
  mycor <- rep(0,1000)
  for (i in 1:1000){
    x <- rnorm(j)
    y <- rnorm(j)
    mycor[i] <- cor(x,y)
  }
  hist(mycor, xlim = c(-.4, .4), main = paste('Sample size:', j))
}

#plotting qqplot
par(mfrow=c(3,3))
n <- c(10, 50, 100, 250, 1000, 10000)
set.seed(1)
for (j in n){
  mycor <- rep(0,1000)
  for (i in 1:1000){
    x <- rnorm(j)
    y <- rnorm(j)
    mycor[i]<- cor(x,y)
  }
  qqnorm(mycor, main = paste('Sample size:', j))
  qqline(mycor, col='red')
}

#plotting histograms w/ normal distribution curve
par(mfrow=c(2,3))
n <- c(10, 50, 100, 250, 1000, 10000)
set.seed(1)
for (j in n){
  mycor <- rep(0,1000)
  for (i in 1:1000){
    x <- rnorm(j)
    y <- rnorm(j)
    mycor[i] <- cor(x,y)
  }
  hist(mycor, probability = TRUE, xlim = c(-1, 1), ylim =c(0,8), main = paste('Sample size:', j))
  curve(dnorm(x, 0, sd(mycor)), add = TRUE, col = 'red')
}



