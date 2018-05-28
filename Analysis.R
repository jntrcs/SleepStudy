#Analysis

#choose a prior
curve(dgamma(x,2,scale=2), xlim=c(0, 100))

prior.pred<-rgamma(100000, rgamma(100000,2.5, scale=2), scale=rgamma(100000,2.5,scale=2))
plot(density(prior.pred), xlim=c(0, 100))

#My prior on shape and scale parameters will be Gamma(2.5, scale=2)     
loglike<- function(data, alpha, beta){
  left<-data[data$Awake==FALSE,]
  right<-data[data$Awake==T,]
  sum(log(pgamma(left$Delay, alpha, scale=beta)))+sum(log(pgamma(right$Delay, alpha, scale=beta, lower.tail = F)))
}

prior.alpha<-function(x)dgamma(x,2.5, scale=2)
prior.beta<-function(x)dgamma(x,2.5, scale=2)

posterior<-function(data,alpha, beta){
  likelihood(data, alpha, beta)*prior.alpha(alpha)*prior.beta(beta)
}

log.post<-function(data, alpha, beta){
  loglike(data, alpha, beta) + log(prior.alpha(alpha))+log(prior.beta(beta))
}


nIter <- 50000
acc <- 0
csig <-1.4


alpha <- rep(0,nIter)
beta <- rep(0,nIter)
alpha[1] <- 5
beta[1] <- 5


for(i in 2:nIter){
  cand.a <- rnorm(1,alpha[i-1],csig)
  cand.b<-rnorm(1, beta[i-1], csig)
  alpha[i]<-alpha[i-1]
  beta[i] <- beta[i-1]
  if(cand.a >0 & cand.b>0){
    laccept <- log.post(data, cand.a, cand.b) - log.post(data, alpha[i], beta[i])
    if(laccept > log(runif(1,0,1))){
      alpha[i]<-cand.a
      beta[i] <- cand.b
      acc <- acc + 1
    }
  }
}
accept <- acc/nIter

plot(alpha)
plot(beta[6000:7000])

plot(alpha~beta)
alpha<-alpha[-(1:1000)]
beta<-beta[-(1:1000)]

##Posterior predictive
#My sleep distribution
sleepTime<-rgamma(49000*10, alpha, scale=beta)
plot(density(sleepTime), xlim=c(0,100), main="Posterior Predictive", xlab="Minutes to Fall Asleep", ylim=c(0, .04))
lines(density(prior.pred), col="red", lty=2)

mean(prior.pred>100)
mean(sleepTime>100)
mean(prior.pred)
mean(sleepTime)

var(sleepTime) #Variance of our predicted next draw
mean(alpha*beta^2) #Variance of possible draws

