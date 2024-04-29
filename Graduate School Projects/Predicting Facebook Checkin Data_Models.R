#loading the complete dataset
library(readr)
fbData <- read_csv("E:/kdd/project/orignal.csv")
summary(fbData)
sum(is.na(fbData))

#creating time variables
fbData$hour = round((fbData$time/60) %% 24)
fbData$weekday = ceiling((fbData$time/(60*24)) %% 7)
fbData$month = ceiling((fbData$time/(60*24*30)) %% 12)
fbData$year = round(fbData$time/(60*24*365))
fbData$day = round(fbData$time/(60*24) %% 365)
fbData$place_id<-as.factor(fbData$place_id)

#selecting top 6 place_ids checkedin
library(dplyr) #dataframe manipulation
place_id<-as.factor(fbData$place_id)
fb <- data_frame(place_id)
fb<-fb %>% group_by(place_id) %>% summarise(count=n())
fb$Freq<-fb$count
hist(fb$count,xlab="PlaceIDs",main="Histogram of PlaceID Counts")
top6<-fb%>% top_n(6,count)
indx<-(fbData$place_id %in% top6$place_id)
fbsubset<-fbData[indx,]
fb

#creating the subset
indx<-(fbData$place_id %in% top6$place_id)
fbsub<-fbData[indx,]
write.csv(fbSub[,-1], file = "MyData.csv")

#import sliced dataset
library(readr)
MyData <- read_csv("E:/kdd/project/MyData.csv")
View(MyData)
#normalization
MyData<-MyData[,-1]
MyData$x<-(MyData$x - min(MyData$x))/(max(MyData$x)-min(MyData$x))
MyData$y<-(MyData$y - min(MyData$y))/(max(MyData$y)-min(MyData$y))
MyData$accuracy<-(MyData$accuracy - min(MyData$accuracy))/(max(MyData$accuracy)-min(MyData$accuracy))
MyData$time<-(MyData$time - min(MyData$time))/(max(MyData$time)-min(MyData$time))
MyData$hour<-(MyData$hour - min(MyData$hour))/(max(MyData$hour)-min(MyData$hour))
MyData$weekday<-(MyData$weekday - min(MyData$weekday))/(max(MyData$weekday)-min(MyData$weekday))
MyData$day<-(MyData$day - min(MyData$day))/(max(MyData$day)-min(MyData$day))
MyData$month<-(MyData$month - min(MyData$month))/(max(MyData$month)-min(MyData$month))
MyData$year<-(MyData$year - min(MyData$year))/(max(MyData$year)-min(MyData$year))
MyData$place_id<-as.factor(MyData$place_id)

#spliting into training and test
set.seed(123)
index<-sort(sample(nrow(MyData),round(.30*nrow(MyData))))
training<-MyData[-index,]
testing<-MyData[index,]
test<-testing[,-5]

#knn
library(class)
train<-training[,-5]
test<-testing[,-5]
knn_fb<-knn(train, test, training$place_id , k = 3, use.all = TRUE)
table(knn_fb,place_id=testing$place_id)
knn_wrong<-sum(knn_fb!=testing$place_id)
knn_error_rate<-knn_wrong/length(knn_fb)
knn_error_rate
#error rate= 0.052


## Naive Bayes classification
library(e1071)
nBayes <- naiveBayes(place_id~x+y+accuracy+time, data =training)
category<-predict(nBayes, test)
table(NBayes=category,place_id=testing$place_id)
NB_wrong<-sum(category!=testing$place_id)
NB_error_rate<-NB_wrong/length(category)
NB_error_rate
#error rate  is 0.011

#random forest
library(ranger) #the random forest implementation
library(randomForest)
fit <- randomForest( place_id~x+y+accuracy+time, data=training, importance=TRUE, num.trees = 80)
#importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, testing[,c(1,2,3,4)])
View(Prediction)
Prediction<-round(Prediction)
table(actual=testing$place_id ,Prediction)
wrong<- (testing$place_id!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate
#with y error rate 0.001

training$place_id<-as.numeric(training$place_id)
testing$place_id<-as.numeric(testing$place_id)
library("neuralnet")
###ANN
NN = neuralnet(place_id~x+y+accuracy+time, training, hidden=2, threshold = 0.01)
plot(NN)
test1<-testing[,(1:4)]
results <- neuralnet::compute(NN, test1)
fbNN=as.numeric(results$net.result)
fbNN_round<-round(fbNN)
fbNN_round
fbNN
table(Actual=testing$place_id,fbNN_round)
wrong<- (testing$place_id!=fbNN_round)
rate<-sum(wrong)/length(wrong)
rate
#for hidden node 1= 0.36
#for hidden node 2= 0.026
