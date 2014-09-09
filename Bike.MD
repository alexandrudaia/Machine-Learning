BIKE SHARING   COMPETITION-


setwd("C:/Users/Home/Desktop/bikesharing")
library(caret)
library(ISLR)
library(kernlab)
data=read.table("train.csv",header=F,sep=",",skip=1)
cnames=readLines("train.csv",1)
cnames=strsplit(cnames,",",fixed=T)
names(data)=make.names(cnames[[1]])
train=read.table("train.csv",header=F,sep=",",skip=1)
cnames=readLines("train.csv",1)
cnames=strsplit(cnames,",",fixed=T)
names(train)=make.names(cnames[[1]])
#
test=read.table("test.csv",header=F,sep=",",skip=1)
cnames=readLines("test.csv",1)
cnames=strsplit(cnames,",",fixed=T)
names(test)=make.names(cnames[[1]])
#1  we  don't need  to remove any  column since  all  provides  information
#we may remove  casual and  reigstred and  holidady(according to  3')
train=train[,-c(3,10,11)]
test=test[,-3]
#get only the  hour from  date  time
train$datetime=as.POSIXlt(train$datetime)$hour
test$datetime=as.POSIXlt(test$datetime)$hour
#1'-simple prediction -  with lm  and one  predictor based on 3)
lm1=lm(count~atemp,data=train)
summary(lm1)
#2-data slicing
inTrain=createDataPartition(y=data$count,p=0.75,list=F)
train=data[inTrain,]
test=data[-inTrain,]
dim(data)
dim(train)
dim(test)
#3)Plotting predictos
featurePlot(x=train[,-12)],y=train$count,plot="pairs")
#explore  some  of the  predictors
qplot(train$temp,train$count,colour=train$weather,data=train)
qplot(train$temp,train$count,colour=train$season,data=train)#minima on 1 season
qplot(train$temp,train$count,colour=train$workingday,data=train)#maximum are  occuring only in working day
plot(train$atemp,train$count,pch=19,col="blue",xlab="atemp",ylab="count")#here is  sure a  relation
plot(train$datetime,train$count,pch=19,col="blue",xlab="hour",ylab="count")#nice correlation
#model fit based on 1'
lines(train$atemp,lm1$fitted,lwd=3)
# we   look at  the errors// root mean squared errors
sqrt(sum((lm1$fitted-train$count)^2))
#prediction of intervals  - the limits  in  wich  we could  expect  that  the  values  will fall
pred1=predict(lm1,newdata=test,interval="prediction")
ord=order(test$atemp)
plot(test$atemp[ord],test$count,pch=19,col="blue")
matlines(test$atemp[ord],pred1[ord,],type="l",,col=c(1,2,2),lty=c(1,1,1),lwd=3)
                                                #we   expect to fall  between  the  two  red lines
#3)'Removing  zero  covariates(with lowest  percent  unique)
nsv=nearZeroVar(train,saveMetrics=T)# resulting  holiday= resulting  very little   viability so  we delete  it

#4)covariate creation -qualitative to cantitative  - not the case


#5)Pre processing( feature scaling(x-mu)/sigma) and dealing with NA'S- not the case
#5')Pre Processing  with PCA/svd
#creating the correlation matrix(to see  what   features are strongly corellated)
#working  on processTreain  since  we need   numeric  var's
m=abs(cor(train[,-9]))
diag(m)=0
which(m>0.8,arr.ind=T)# resulting atemp  and  temp  are correlated 
plot(train[,"temp"],train[,"atemp"])#visualize the correlation
#we need  a combination  of this  two  that captures  the most information possible
# -->> SVD(reduce  noise and  number  of predictors)
#rorate  the plot
x=0.71*train$temp+0.71*train$atemp
y=0.71*train$temp-0.71*train$atemp
plot(x,y)

#Machine Learning  part

#6)Predicting  with  various methods
# with multiple covariates (linear model)

modelFitAll=train(count~.,data=train,method="lm")
print(modelFitAll$finalModel)

pred=predict(modelFitAll,test)
t=round(abs(pred))
p=as.integer(t)
write.table(p,file="pred.csv",sep=",",row.names=F)   
#Diagnostics
model=modelFitAll$finalModel
plot(model,1,pch=19,cex=0.5,col="#00000010")
#bagging  example  for ozone
library(ElemStatLearn)
data(ozone,package="ElemStatLearn")
ozone=ozone[order(order$ozone),]
ll=matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
  + ss=sample(1:dim(ozone)[1],replace=T)
  + ozone0=ozone[ss,];ozone0=ozone0[order(ozone0$ozone),]
  + loess0=loess(temperature~ozone,data=ozone0,span=0.2)
  + ll[i,]=predict(loess0,newdata=data.frame(ozone=1:155))
  + }
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10) {
lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)
#PREDICTING USING   BOOSTING  WITH  TREES
modFit=train(count~.,method="gbm",data=train,verbose=F)
pred=predict(modFit,test)

