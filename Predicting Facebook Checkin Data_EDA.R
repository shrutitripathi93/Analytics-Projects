library(ggplot2)
library(dplyr) 
library(plotly)
install.packages("plot3D")
library("plot3D")
install.packages("xlsx")
library("xlsx")
library(randomForest)

fbCheckinData <- read.csv("C:/Users/shrutz/Desktop/BIA/Sem2/KDD/Final Project/train.csv")
View(fbCheckinData)
summary(fbCheckinData)
summary(as.factor(fbCheckinData$place_id))

#breaking time down to hour, weekday, month, year, day
fbCheckinData$hour = (fbCheckinData$time/60) %% 24
fbCheckinData$weekday = ceiling((fbCheckinData$time/(60*24)) %% 7)
fbCheckinData$month = ceiling((fbCheckinData$time/(60*24*30)) %% 12) 
fbCheckinData$year = round(fbCheckinData$time/(60*24*365))
fbCheckinData$day = round(fbCheckinData$time/(60*24) %% 365)

fbCheckinData$weekday <- as.numeric(fbCheckinData$weekday)

head(fbCheckinData, 3)

#density of time in dataset
fbCheckinData %>%
  sample_frac(0.1) %>%
  ggplot(aes(x = time)) +
  geom_density(color="darkblue", fill="lightblue")
#two major dips in time density

#density of accuracy in dataset
fbCheckinData %>%
  sample_frac(0.1) %>%
  ggplot(aes(x = accuracy)) +
  geom_density(color="darkblue", fill="lightblue")
# most density for values < 250

#ploting time vs accuracy and coloring by placeid
fbCheckinData %>%
  sample_frac(0.001) %>%
  ggplot(aes(time, accuracy )) +
  geom_point(aes(color = place_id)) + 
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("time vs accuracy colored by place_id")
#irrespective of time, accuracy has same range of values

#ploting x vs y and coloring by placeid
fbCheckinData %>%
  sample_frac(0.001) %>%
  ggplot(aes(x, y )) +
  geom_point(aes(color = place_id)) + 
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Check-ins colored by place_id")
#placeids homogenously distributed across locations


#slicing data by hour to analyze relation between x, y & hour
fbCheckinData %>%
  sample_frac(0.001) %>%
  filter(hour<1) -> fbCheckinDataSliceHour
View(fbCheckinDataSliceHour)
hist(fbCheckinDataSliceHour$hour)

#slicing data for placeids with high accuracy
fbCheckinData %>%
  filter(accuracy>950) -> fbCheckinDataSliceAccuracy
View(fbCheckinDataSliceAccuracy)
hist(fbCheckinDataSliceAccuracy$place_id)
#few placeids with high accuracy

plot_ly(data = fbCheckinDataSliceHour, x = ~x , y = ~y, z = ~hour, color = ~place_id,  type = "scatter3d", mode = "markers", marker=list(size= 5)) %>% layout(title = "Place_id's by position and Time of Day")
#single placeid made of multiple x-y coordinates


fbCheckinData %>%
  sample_frac(0.001) %>%
  hist(fbCheckinData$weekday)

# 3D scatter plot of x, y vs accuracy
fract<- sample_frac(fbCheckinData, 0.001)
x <- fract$x
y <- fract$y
accuracy <- fract$accuracy
scatter3D(x, y, accuracy, bty = "f", colkey = FALSE,
          main = "accuracy vs x - y", xlab = "x",ylab ="y", zlab = "accuracy")
#higher accuracies belong to very few place ids


#slicing data into to 3 different blocks to analyze distribution of placeid
fbCheckinData %>% filter(x >1, x <1.25, y >2.5, y < 2.75) -> fbCheckinDataSlice1
summary(as.factor(fbCheckinDataSlice1$place_id))
hist(fbCheckinDataSlice1$place_id, main="Frequency of placeid by location 1")
head(fb, 3)

fbCheckinData %>% filter(x >9.5, x <9.75, y >6.25, y < 6.5) -> fbCheckinDataSlice2
summary(as.factor(fbCheckinDataSlice2$place_id))
hist(fbCheckinDataSlice2$place_id, main="Frequency of placeid by location 2")

fbCheckinData %>% filter(x >5, x <5.25, y >6.5, y < 6.75) -> fbCheckinDataSlice3
summary(as.factor(fbCheckinDataSlice3$place_id))
hist(fbCheckinDataSlice3$place_id, main="Frequency of placeid by location 3")

#findings: 
# 1. placeids are unique identifiers for chains that can exist in multiple locations
# 2. same placeid has different number of checkins in different locations


fb <- data_frame(fbCheckinData$place_id)
fb<-fb %>% group_by(fbCheckinData$place_id) %>% summarise(count=n())
write.csv(fb, file = "placeIdCount.csv")
View(fb)
hist(fb$count)
fb%>% top_n(20,count)

fb <- data_frame(fbCheckinDataSliceAccuracy$place_id)
fb<-fb %>% group_by(fbCheckinDataSliceAccuracy$place_id) %>% summarise(count=n())
#write.csv(fb, file = "placeIdCountA.csv")
hist(fb$count)
View(fb)
fb%>% top_n(20,count)


#creating subset for top 6 place_ids
fbCheckinDataSubset<-subset(fbCheckinData,(fbCheckinData$place_id==8772469670|fbCheckinData$place_id==1623394281|fbCheckinData$place_id==1308450003|fbCheckinData$place_id==4823777529|fbCheckinData$place_id==9586338177|fbCheckinData$place_id==9129780742))
View(fbCheckinDataSubset)


fbCheckinDataSubset$row_id <- as.numeric(fbCheckinDataSubset$row_id)
fbCheckinDataSubset$x <- as.numeric(fbCheckinDataSubset$x)
fbCheckinDataSubset$y <- as.numeric(fbCheckinDataSubset$y)
fbCheckinDataSubset$accuracy <- as.numeric(fbCheckinDataSubset$accuracy)
fbCheckinDataSubset$time <- as.numeric(fbCheckinDataSubset$time)
fbCheckinDataSubset$place_id <- as.numeric(fbCheckinDataSubset$place_id)
fbCheckinDataSubset$hour<- as.numeric(fbCheckinDataSubset$hour)
fbCheckinDataSubset$weekday <- as.numeric(fbCheckinDataSubset$weekday)
fbCheckinDataSubset$month <- as.numeric(fbCheckinDataSubset$month)
fbCheckinDataSubset$year <- as.numeric(fbCheckinDataSubset$year)
fbCheckinDataSubset$day <- as.numeric(fbCheckinDataSubset$day)

#number of checkins for place ids according to hour
hist(fbCheckinDataSubset$hour, main="Checkins for place ids according to hour")

#number of checkins for place ids according to day
hist(fbCheckinDataSubset$weekday, main="Checkins for place ids according to weekday ")

fbCheckinDataSubset$row_id <- NULL






