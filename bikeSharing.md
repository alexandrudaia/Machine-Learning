BIKE SHARING-https://www.kaggle.com/c/bike-sharing-demand

setwd("C:/Users/Home/Desktop/bikesharing")
library(caret)
library(ISLR)
library(kernlab)
data=read.table("train.csv",header=F,sep=",",skip=1)
cnames=readLines("train.csv",1)
cnames=strsplit(cnames,",",fixed=T)
names(data)=make.names(cnames[[1]])
#1  we  don't need  to remove any  column since  all  provides  information
#we may remove  casual and  reigstred and  holidady(according to  3')
data=data[,-c(3,10,11)]
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
featurePlot(x=train[,c(-12,-1)],y=train$count,plot="pairs")
#explore  some  of the  predictors
plot(train$atemp,train$count,pch=19,col="blue",xlab="atemp",ylab="count")#here is  sure a  relation
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
m=abs(cor(train[,-c(1,12)]))
diag(m)=0
which(m>0.8,arr.ind=T)# resulting atemp  and  temp  are correlated 
plot(train[,"temp"],train[,"atemp"])#visualize the correlation
#we need  a combination  of this  two  that captures  the most information possible
# -->> SVD(reduce  noise and  number  of predictors)
#rorate  the plot
x=0.71*train$temp+0.71*train$atemp
y=0.71*train$temp-0.71*train$atemp 
plot(x,y)


 
