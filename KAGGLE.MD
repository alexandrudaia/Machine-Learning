TITANIC PROBLEM ----https://www.kaggle.com/c/titanic-gettingStarted/leaderboard

 #classsification problem ( discrette  values of output)
library(caret)
library(ISLR)
library(kernlab)
data=read.table("train.csv",header=F,sep=",",skip=1)
cnames=readLines("train.csv",1)
cnames=strsplit(cnames,",",fixed=T)
names(data)=make.names(cnames[[1]])
 
#1-removing   passangeR name , ticket number and cabin  hence  information=0
data=data[,-c(1,4,9,11)]
#1'-simple prediction on  sex  assuming   women   survived  more  than man
prediction=ifelse(data$Sex=="female",1,0)
accuracy_table=table(prediction,data$Survived)/length(data$Survived)
accuracy=accuracy_table[1]+accuracy_table[4]#aprox 0.78
                                            #details https://d396qusza40orc.cloudfront.net/predmachlearn/006typesOfErrors.pdf
#1''- ROC  curves - AUC=0.5 RANDOM  GUESSING AUC=0.8 OK  AUC=1  PERFECT
                         #Analyzing  and processing data
#2-data slicing
inTrain=createDataPartition(y=data$Survived,p=0.75,list=F)
train=data[inTrain,]
test=data[-inTrain,]
dim(data)
dim(train)
dim(test)
#3)Plotting predictos
featurePlot(x=train[,-2],y=train$Survived,plot="pairs")#uggly matrix 
qplot(Age,Survived,colour=Sex,data=train)#more  women survived
                                         #to  be analyzed  for all posible  vars
qplot(Survived,colour=Sex,data=train,geom="density")#works  for categorical data
                                         #interesting density
qplot(Survived,colour=Embarked,data=train,geom="density")# repr.

#3)'Removing  zero  covariates(with lowest  percent  unique)
nsv=nearZeroVar(train,saveMetrics=T)#no near  zero vars

#4)covariate creation -qualitative to cantitative 
train$Sex=as.numeric(train$Sex=="male")#for train
em=factor(train$Embarked)
emn=as.integer(em)
train$Embarked=emn
test$Sex=as.numeric(test$Sex=="male")#for test
em=factor(test$Embarked)
emn=as.integer(em)
test$Embarked=emn


      
#5)Pre processing( feature scaling(x-mu)/sigma) and dealing with NA'S
preObj=preProcess(train[,-1],method="knnImpute")#for train
processTrain=cbind(train[,1],predict(preObj,train[,-1]))
names(processTrain)=names(train)
processTest=cbind(test[,1],predict(preObj,test[,-1]))#for test
names(processTest)=names(test)
          #=>>>>> if  test.csv  id  read sep ->>processTest=predict(preObj,test)
                                              #names(processTest)=names(test)



#5')Pre Processing  with PCA
#creating the correlation matrix(to see  what   features are strongly corellated)
#working  on processTreain  since  we need   numeric  var's
m=abs(cor(processTrain[,-1]))
diag(m)=0
which(m>0.8,arr.ind=T)# resulting non corelated data
#if it was true -->> SVD(reduce  noise and  number  of predictors)
#rorate  the plot
x=0.71*train$var1+0.71*train$var2
y=0.71*train$var1-0.71*train$var2
#example on spam problem:
smallSpam=spam[,c(34,32)]
pca=prcomp(smallSpam)
plot(pca$x[,1],pca$x[,2])
#example with CARRET CA
preProcess=preProcess(log10(train[,-58]+1),method="pca",pcaComp=2)
trainPC=predict(preProcess,log10(train[,-58]+1))
modelFit=train(train$type~.,method="glm",data=trainPC)
testPC=predict(preProcess,log10(test[,-58]+1))
p=predict(modelFit,testPC)#!!!!!!it maybe done   directly in  the  machine  learging part


                      #Machine Learning  part

#6)Predicting  with  various methods

#WITH  GLM
modelFit=train(Survived~.,data=processTrain,method="glm")#aditional  we can put preProcess="pca"
                                                        #accuracy without PCA=0.7883
predictions=predict(modelFit,newdata=processTest)
t=round(predictions)
p=as.integer(t)
write.table(p,file="pred.csv",sep=",",row.names=F)        

#7)Analyzing  the prediction with conf. matrix  and ROC curves

confusionMatrix(test$Survived,p)#0.7793 accuracy with  pca
 

 
