
#Slope from logistic regression
b1= 0.08511
#Intercept from logistic regression
b0 = -2.33184
fx <- function(x){
  b1*exp(b0+x*b1)/(exp(b0+x*b1)+1)^2
}

integrate(function(x)fx(x), -100, 100)
##This is the distribution of time it takes to fall asleep implied by the logistic regression
#(Ignoring the uncertainty in the parameter estimates)
curve(fx, from=-5, to=85)
curve(dlogis(x, location=-b0/b1, scale = 1/b1), add=T, col="blue")

#location = -b1*b2

#mean is -b0/b1
