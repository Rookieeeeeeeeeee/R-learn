library(rms)
data("airquality")

dd <- datadist(airquality) #为后续程序设定数据环境
options(datadist='dd') #为后续程序设定数据环境
head(airquality)

fit<-ols( Ozone ~rcs( Wind, 4 ) + Temp, data = airquality)
summary(fit)
an<-anova(fit)
an


olsdata <- Predict(fit,Wind)
plot(Predict(fit,Wind),,anova=an, pval=T)

ggplot()+geom_line(data=olsdata, aes(Wind,yhat),linetype=1,size=1,alpha = 0.9,colour="red")+
  geom_ribbon(data=olsdata, aes(Wind,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+
  theme_classic()+ 
  labs(title = "RCS", x="Wind", y="Ozone")

