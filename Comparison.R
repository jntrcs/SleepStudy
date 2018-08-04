#Comparison.R

#Create a graphic with the predicted probability that I'm asleep using my Bayesian model and my logistic regression

times<-1:160
probAsleep<-sapply(times, FUN=function(time) mean(pgamma(time, alpha, scale=beta, lower.tail = T)))
bayes<-data.frame(Time=times, AsleepProb=probAsleep)

ggplot(data)+geom_point(aes(x=Delay, y=Asleep))+geom_line(data=a, aes(x=Delay, y=preds, col="Logistic Regression")) +
  geom_line(data=bayes, aes(x=Time, y=AsleepProb, col="Censored Bayes"))+xlim(c(0, 90)) +ylab("Probability of being asleep")
