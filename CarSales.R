############# **** Loading Data **** ############

library(readr)
car <- read.csv("E:/TopicsInBigData/car-sale-advertisements/car_ad.csv",na.strings = c("NA","","Other", "other"))

View(car)
str(car)

sapply(car,function(x){sum(is.na(x))})

############ **** Data Imputation **** #################

##Using the function KNN(k-Nearest Neighbour Imputation) in the package VIM to 
#imputate the missing values. K-Nearest Neighbour Imputation 
#based on a variation of the Gower Distance for numerical, categorical, ordered and semi-continous variables.

install.packages("VIM")
library(VIM,quietly = T,warn.conflicts = F)
carData<-kNN(car,variable=c("engV","engType","drive","body"),dist_var=c("car","price","engV","engType","model","drive"),imp_var = F)

sapply(carData,function(x){sum(is.na(x))})

## Deleting rows with zero price value
carData<-carData[!(carData$price==0),]
min(carData$price)

################ **** Data Transformation **** ################

## We will try to transform the caracter variable into the numeric variable to facilite the algorithm 
##training later. The method that I use is to reorder the levels of the variable by the mean of its price.

## make a function that transform that relevel the order of the level by the mean of the price.

f<-function(variable.number){
  list.option<-levels(carData[,variable.number])
  a<-c()
  for(i in 1:length(list.option)){
    mean.i<-mean(carData[carData[,variable.number]==list.option[i],"price"])
    a<-c(a,mean.i)
  }
  b<-order(a)
  new.order<-list.option[b]
  return(new.order)
}

## Transform variable "car" into numeric value.
new.level.car<-f(1)
levels(carData$car)<-new.level.car
levels(carData$car)
carData$car<-as.numeric(carData$car)


## Transform variable "body" into numeric value.
new.level.body<-f(3)
levels(carData$body)<-new.level.body
levels(carData$body)
carData$body<-as.numeric(carData$body)

## Transform variable "engType" into numeric value.
new.level.engType<-f(6)
levels(carData$engType)<-new.level.engType
levels(carData$engType)
carData$engType<-as.numeric(carData$engType)

## Transform variable "model" into numeric value.
new.level.model<-f(9)
levels(carData$model)<-new.level.model
levels(carData$model)
carData$model<-as.numeric(carData$model)

## Transform variable "drive" into numeric value.
new.level.drive<-f(10)
levels(carData$drive)<-new.level.drive
levels(carData$drive)
carData$drive<-as.numeric(carData$drive)

## Transform variable "registration" into numeric value.
carData$registration<-gsub("no","0",carData$registration)
carData$registration<-gsub("yes","1",carData$registration)


carData$registration<-as.numeric(as.character(carData$registration))

hist(carData$year, col="gray", labels = TRUE)

## Age transform.
carData$year<-2018-carData$year
carData$year

########## **** Exploratory Analysis **** ############

summary(carData)

par(mfrow=c(3,3))
for(i in 1:9){
  hist(carData[,i], main=names(carData)[i])
}

library(corrplot,quietly = T,warn.conflicts = F)

correlations<-cor(carData)
corrplot(correlations,method="circle")
hist(carData$year)

############# **** Data Splitting **** #######

library(caret,quietly = T,warn.conflicts = F)

## Split the dataset to training set and validation set. 

set.seed(5)
validationIndex<-createDataPartition(carData$price,p=0.8, list=F)
validation<-carData[-validationIndex,]
training<-carData[validationIndex,]


## Run algorithmes using 10-fold cross validation
set.seed(5)
trainControl<-trainControl(method = "repeatedcv",number=10,repeats = 3)
trainControl
## RMSE for regression training
metric<-"RMSE"
metric
## preprocessing
preProcess<-c("center","scale","BoxCox")
preProcess

## names(getModelInfo()) - To get model infos 

########### **** Modeling  **** ##############

## Generalised linear model
set.seed(5)
fit.glm<-train(price ~ ., data=training, method="glm", metric=metric, preProc=preProcess ,trControl=trainControl)
fit.glm

## GLMNET regularized Logistic Regression
#set.seed(5)
#fit.glmnet<-train(price~., data=training, method="glmnet", metric=metric, preProc=preProcess, trControl=trainControl)

  

## k-Nearest Neighbors
set.seed(5)
fit.knn<-train(price~., data=training, method="knn", metric=metric, preProc=preProcess, trControl=trainControl)
fit.knn

## SVM - Support Vector Machines with Radial Basis Function Kernel
set.seed(5)
fit.svm<-train(price~., data=training, method="svmRadial", metric=metric, preProc=preProcess, trControl=trainControl)
fit.svm

######## **** The Results **** #########        
results<-resamples(list(GLM=fit.glm, KNN=fit.knn, SVM=fit.svm))
summary(results)
dotplot(results)


## Fit neural network 

# install library
install.packages("neuralnet")

# load library
library(neuralnet)

# Neural Network
set.seed(5)
#fit.NN = neuralnet(price~car+body+mileage+engV+engType+registration+year+model+drive, training, hidden = 3 , linear.output = T )
fit.NN<-train(price~., data=training, method="neuralnet", metric=metric, preProc=preProcess, trControl=trainControl)
fit.NN


results<-resamples(list(GLM=fit.glm, KNN=fit.knn, SVM=fit.svm, NN=fit.NN))
summary(results)
dotplot(results)

############ ***** Prediction **** ############
set.seed(5)
GLMPrediction<-predict(fit.glm, validation)

set.seed(5)
KNNPrediction<-predict(fit.knn, validation)

set.seed(5)
SVMPrediction<-predict(fit.svm, validation)

set.seed(5)
NNPrediction<-predict(fit.NN,validation)

set.seed(5)
PredictionRMSEs<-c(RMSE(validation$price, GLMPrediction),RMSE(validation$price, KNNPrediction),RMSE(validation$price, SVMPrediction),RMSE(validation$price, NNPrediction))
lbl=c("GLM","KNN","SVM","NN")
plot(PredictionRMSEs, main="RMSE vs. Model", xlab="Modeling  Methods", ylab="RMSE", pch=18, col="blue")
text(PredictionRMSEs, labels = lbl , cex=1.0, pos=4, col="red")


PredictedRSquared<-c(R2(validation$price, GLMPrediction),R2(validation$price, KNNPrediction),R2(validation$price, SVM2Prediction),R2(validation$price, NNPrediction))
plot(PredictedRSquared, main="R-Squared vs. Model", xlab="Modeling  Methods", ylab="R-Squared", pch=18, col="blue")
text(PredictedRSquared, labels = lbl , cex=1.0, pos=4, col="red")
PredictedRSquared




old.par = par()
par(mfrow = c(1,4))
with(auto, {
  plot(GLMPrediction - validation$price, col="blue", main = "Generalized Linear Model")
  plot(KNNPrediction - validation$price, col="red", main = "K-Nearest Neighbors")
  plot(SVMPrediction - validation$price, col="blue", main = "Support Vector Machine")
  plot(NNPrediction - validation$price, col="red", main = "Neural Network")
} )

actuals_preds <- data.frame(cbind(actuals=validation$price, predicteds=KNNPrediction))
correlation_accuracy <- cor(actuals_preds) 
corrplot(correlation_accuracy,method="number")
correlation_accuracy



