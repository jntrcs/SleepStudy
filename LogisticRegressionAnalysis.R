#Logistic regression
head(data)
data$Asleep<-ifelse(data$Awake, 0, 1)
model <- glm(Asleep ~Delay,family="binomial",data=data)
summary(model)
preds<-predict(model, newdata=data.frame(Delay=0:150), type="response")

a=data.frame(Delay=0:150, Asleep=preds)
ggplot(data)+geom_point(aes(x=Delay, y=Asleep))+geom_line(data=a, aes(x=Delay, y=preds))

a
