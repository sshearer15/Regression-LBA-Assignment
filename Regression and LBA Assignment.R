library("Matching")
data(lalonde)
head(lalonde)

regression.treat.predicts.re78 <- lm(lalonde$re78 ~ lalonde$treat)
regression.treat.predicts.re78
summary(regression.treat.predicts.re78)

#calculating R2
pred.re78 <- predict(regression.treat.predicts.re78, data.frame(lalonde$treat))
pred.re78

reg.error <- lalonde$re78 - pred.re78
reg.error

sqd.error <- reg.error*reg.error
sqd.error

sum(sqd.error)

mean(lalonde$re78) 

var.from.mean<- lalonde$re78 - mean(lalonde$re78)
var.from.mean

sqd.var.from.mean <- error2*error2
sqd.var.from.mean

sum(sqd.var.from.mean)

Rsquared <- 1 - ((sum(sqd.error)/sum(sqd.var.from.mean)))
Rsquared

#show caculated Rsquared is = to that produced using R function
idential(Rsquared, cor(lalonde$treat, lalonde$re78))



#add other regressors to model to evaluate R2
## two predictor variables (treatment and education)
regress.treat.and.educ.predicts.re78 <- lm(lalonde$re78 ~ lalonde$treat + lalonde$educ)
regress.treat.and.educ.predicts.re78
summary(regress.treat.and.educ.predicts.re78)

# three predictor variables (treatment, education, and age)
regress.treat.and.educ.and.age.predicts.re78 <-lm(lalonde$re78 ~ lalonde$treat + lalonde$educ + lalonde$age)
regress.treat.and.educ.and.age.predicts.re78
summary(regress.treat.and.educ.and.age.predicts.re78)


'''
“In raquetball, a player continues to serve as long as she is winning; 
a point is scored only when a player is serving and wins the volley. 
The first player to win 21 points wins the game. 
Assume that you serve first and have probability .6 of
winning a volley when you serve and probability .5 when 
your opponent serves. Estimate, by simulation, the probability that you will win a game.” 
'''

game <- rep(0,200)

for (i in 1:200){
  
  imserving <- TRUE
  mypoint <- 0
  oponent <- 0
  while (mypoint < 21 & oponent < 21){
    if (imserving){
      outcome <- sample(c(0,1), 1, prob = c(.4, .6))
      mypoint <- mypoint + outcome
      if (outcome == 0){
        imserving <- FALSE
      }
    }else{
      outcome <- sample(c(0,1), 1, prob=c(.5,.5))
      oponent <- oponent + outcome
      if (outcome == 0){
        imserving <- TRUE
      }
    }
  }
  game[i] <- mypoint > oponent
}

mean(game)