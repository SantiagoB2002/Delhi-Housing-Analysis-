
house=read.csv("Housing_train.csv")
head(house)
house=house[,-1]
lm(price~.,data=house)
str(house)
#convert characters to factors
#cleaning the data
house1=as.data.frame(unclass(house),stringsAsFactors = TRUE)
summary(house1)
house1$furnishingstatus=relevel(house1$furnishingstatus,ref = "unfurnished")
# multiple regression of full model
lm2= lm(price~ area + bedrooms + bathrooms + stories + mainroad + guestroom + basement + hotwaterheating + airconditioning + parking + prefarea + furnishingstatus, data = house1)
summary(lm2)
plot(price~area + bedrooms + bathrooms + stories + mainroad + guestroom + basement + hotwaterheating + airconditioning + parking + prefarea + furnishingstatus, data = house1)

#collinearity
lm0=lm(price~.,data=house1)
summary(lm0)
head(house1)

#colienarty one of the values is depened on the other 
cor.matrix=cor(house1[,sapply(house1,is.numeric)])
image(abs(cor.matrix))
library(reshape2)
library(ggplot2)
ggplot(data = melt(cor.matrix),aes(x=Var1,y=Var2,fill=value))+geom_tile()
library(faraway)
vif(lm0)
#If vif is lalrge the beta is large each preidictor regressed other bigger r squared if larger 5 or 10 muptile colineaartly 
#Muptile colienanry inflaltes the variance too many options can be replaced with other predicitors 
#######################Best subset selection
library(leaps)
house1$furnishingstatus=as.factor(house1$furnishingstatus)
house1$furnishingstatus=relevel(house1$furnishingstatus,ref="unfurnished")
lm.full=lm(price~.+area:stories ,data=house1)
plot(lm.full)
summary(lm.full)
regfit.full=regsubsets (price~.+area:stories ,data=house1,nvmax=12)
ncol(house1)
#max limits the predictors 
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
#can not use due to the largest one beign the largest too big predicition makes no sense at all
#can use adjusted due to accomaping size
reg.summary$adjr2
par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")
which.max (reg.summary$adjr2)
points (which.max (reg.summary$adjr2), reg.summary$adjr2[which.max (reg.summary$adjr2)], col ="red",cex =2, pch =20)
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",type="l")
which.min (reg.summary$cp )
points (which.min (reg.summary$cp ), reg.summary$cp [which.min (reg.summary$cp )], col ="red",cex =2, pch =20)
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",type="l")
which.min (reg.summary$bic )
points (which.min (reg.summary$bic ), reg.summary$bic [which.min (reg.summary$bic )], col =" red",cex =2, pch =20)
reg.summary$bic
plot(regfit.full ,scale ="r2")
plot(regfit.full ,scale ="bic")
coef.selected=coef(regfit.full ,which.min (reg.summary$bic ))
coef.selected
reg.summary$bic
which.min(reg.summary$bic)
#want BIC to be small 

############################
#only diffrence is the adding of the method 
regfit.fwd=regsubsets (price~.+area:stories,data=house1,nvmax=12,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets (price~.+area:stories,data=house1,nvmax=12,method="backward")
summary(regfit.bwd)
coef(regfit.full,11)
coef(regfit.fwd,11)
coef(regfit.bwd,11)
########################
set.seed (1)
#train=sample(c(TRUE,FALSE), nrow(Credit),rep=TRUE,prob = c(.8,.2))
#val=(! train)
train=sample(1:400,300,replace=F)
val=setdiff(1:400,train)
regfit.best=regsubsets (price~.+area:stories,data=house1[train,],nvmax =10)
val.mat=model.matrix(lm(price~.+area:stories,data=house1[val,]))
val.errors =rep(NA,10)
for(i in 1:10){
  coefi=coef(regfit.best ,id=i)
  predi=val.mat[,names(coefi)]%*% coefi
  #pred=predict.regsubsets(regfit.best,newdata=Credit[val,] ,id=i)
  val.errors[i]= mean((house1$price[val]-predi)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,4)

predict.regsubsets =function (object ,newdata ,id )
{
  form=as.formula (object$call [[2]])
  mat=model.matrix (form ,newdata )
  coefi =coef(object ,id=id)
  xvars =names(coefi)
  mat[,xvars]%*% coefi
}

lm.cv.final=lm(price~area+bathrooms+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+area:stories,data=house1)
summary(lm.cv.final)
par(mfrow=c(2,2))
plot(lm.cv.final)
test=read.csv("Housing_test.csv")
yhat=predict(lm.cv.final,newdata = test)
yhat
par(mfrow=c(1,1))
plot(yhat,test$price)
abline(a=0,b=1)
MSE=mean((yhat-test$price)^2)
MSE
sqrt(MSE)
