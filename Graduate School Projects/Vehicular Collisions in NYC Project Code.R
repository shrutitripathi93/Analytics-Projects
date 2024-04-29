###Reading the data and doing Data cleaning
dt <- read.csv("NYPD_Motor_Vehicle_Collisions.csv")
str(dt$BOROUGH)
View(filter(dt, dt$BOROUGH == ""))
fill <- filter(dt, dt$BOROUGH == "")
dtcopy$DATE <- as.Date(dtcopy$DATE, "%m/%d/%Y")

##Cleaning data
dt1 <- select(dtcopy, 11:18)
dt1 <- select(dtcopy, 11:18)
dt1$sum <- rowSums(dt1)
r <- dt1$sum !=0
refined <- dtcopy[r, ]

###Forming the Datafrane for accidents
accidents1 <- data.frame(dtcopy$DATE)
library(lubridate)
accidents1$Day <- day(accidents1$dtcopy.DATE)
accidents1$Month <- month(accidents1$dtcopy.DATE)
accidents1$Year <- year(accidents1$dtcopy.DATE)
accidents1$Wday <- wday(accidents1$dtcopy.DATE)

###Summarising the accidents
sumed_accidents2 <- accidents1 %>% group_by(Day, Month, Year, Wday) %>% summarise(Crashes = n())
p <- vector(mode = "character")
for(i in 1:nrow(sumed_accidents)){
p[i] <- paste(as.character(c(sumed_accidents$Day[i],sumed_accidents$Month[i],
                          sumed_accidents$Year[i])), collapse = "/")
}
sumed_accidents$Date <- as.Date(p, "%d/%m/%Y")

library(dplyr)

##Data cleaning for Visualization
library(funModeling)
f <- freq(refined, unique(c("LATITUDE")))
lats <- vector()
longs <- vector()
for(i in 1:nrow(refined)){
  if(is.na(refined$LATITUDE[i])){
    lats[i] <- mean(refined$LATITUDE, na.rm = TRUE)
  }
}
##refined1 <- refined
##refined1[refined1$LATITUDE == NA] <- mean(refined1$LATITUDE, na.rm = T)

dt1 <- select(dt, 11:18)
dt1 <- select(dt, 11:18)
dt1$sum <- rowSums(dt1)
r <- dt1$sum !=0
refined <- dt[r, ]




###Forming Location datasets
just_location <- refined[, c(4,5,6)]
library(mice)
View(md.pattern(just_location))
just_location.m <- mice(just_location, m = 5, maxit = 1)
just_location.c <- complete(just_location.m, 2)
just_location.c1 <- read.csv("complete_location.csv")
just_location.c1 <- just_location.c1[,2:4]
injured <- refined[,c(11,13,15,17)]
injured$Total <- rowSums(injured)
killed <- refined[,c(12,14,16,18)]
killed$Total <- rowSums(killed)
just_location.c1$Injured <- injured$Total
just_location.c1$Killed <- killed$Total
library(dplyr)
just_location.c1.r <- just_location.c1[just_location.c1$BOROUGH != "", ]
zipcodes_accidents_b <- just_location.c1.r %>% group_by(ZIP.CODE, BOROUGH) %>% summarise(Crash = n(),
                                                                           Injured = sum(Injured),
                                                                           Killed = sum(Killed))
zipcodes_accidents <- just_location.c1 %>% group_by(ZIP.CODE, BOROUGH) %>% summarise(Crash = n(),
                                                                                         Injured = sum(Injured),
                                                                                         Killed = sum(Killed))
lat_long_accidents <- just_location.c %>% group_by(LATITUDE, LONGITUDE) %>% summarise(Crash = n(),
                                                                           Injured = sum(Injured),
                                                                           Killed = sum(Killed))
try_accidents <- just_location.c1 %>% group_by(ZIP.CODE) %>% summarise(LAT = mean(LATITUDE),
                                                                       LNG = mean(LONGITUDE),
                                                                       Crash = n(),
                                                                       Injured = sum(Injured),
                                                                       Killed = sum(Killed))
try_accidents_b <- just_location.c1.r %>% group_by(ZIP.CODE, BOROUGH) %>% summarise(LAT = mean(LATITUDE),
                                                                       LNG = mean(LONGITUDE),
                                                                       Crash = n(),
                                                                       Injured = sum(Injured),
                                                                       Killed = sum(Killed))
lat_long_accidents <- lat_long_accidents[-1, ]
##Creating Leaflet
library(leaflet)
lati <- try_accidents$LAT
longi <- try_accidents$LNG
map2 <- try_accidents %>% leaflet() %>% addTiles() %>%
  addCircles(weight = 4, radius = zipcodes_accidents$Injured * 0.5) %>%
  addMarkers(lat = lati, lng = longi, popup = toupper(paste(as.character(try_accidents$ZIP.CODE),
                                                      paste("No. of Person Injured: ", as.character(try_accidents$Injured)),
                                                      paste("No. of Person Killed: ", as.character(try_accidents$Killed)),
                                                      sep = "<br/>")),
  clusterOptions = markerClusterOptions())
map2


##Second viz
library(ggthemes) 
library(ggplot2)
min_lat <- min(lat_long_accidents$LATITUDE)
max_lat <- max(lat_long_accidents$LATITUDE)
min_long <- min(lat_long_accidents$LONGITUDE)
max_long <- max(lat_long_accidents$LONGITUDE)

ggplot(lat_long_accidents, aes(x=LONGITUDE, y=LATITUDE)) + geom_point(size = 0.06) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map()

###Summarising according to hour
injured$Time <- refined$TIME
injured$hour <- hour(as.POSIXct(injured$Time, format = "%H:%M"))
injured$BOROUGH <- boroughs$Bor
hour_accidents_b <- injured %>% group_by(BOROUGH, hour) %>% summarise(Injured = n())
hour_accidents_b <- hour_accidents_b[hour_accidents_b$BOROUGH != "", ]
ggplot(hour_accidents_b, aes(hour, Injured, fill = BOROUGH)) + geom_bar(stat = "identity", show.legend = FALSE) +
  theme_calc() +
  ggtitle("No. of People Injured in NYC Collision according to Hour") + facet_grid(BOROUGH ~ .)

##Summarising according to Month
library(ggthemes)
View(accidents)
accidents <- data.frame(accidents)
accidents$BOROUGH <- boroughs$Bor
crash_month_b <- accidents %>% group_by(BOROUGH, Month) %>% summarise(Crash = n())
crash_month_b <- crash_month_b[crash_month_b$BOROUGH != "", ]
crash_month_b$Month <- as.factor(crash_month_b$Month)
ggplot(crash_month_b, aes(Month, Crash, fill = BOROUGH, col = BOROUGH, size = Crash)) +
  geom_violin(alpha = 0.5, show.legend = FALSE) +
  theme_solarized_2() + ggtitle("NYC Collision according to Month from Year 2012 to 2017")+ 
  facet_grid(. ~ BOROUGH) + geom_point(show.legend = FALSE, size = 2.5) + scale_x_discrete(limits = c(4,8,12)) + scale_size_continuous(range = c(1,8))
###Summarising killed people according to Borough


killed$BOROUGH <- boroughs$Bor
killed2 <- killed[killed$Total != 0, ]
killed_bor_total <- killed2 %>% group_by(BOROUGH) %>% summarise(Killed = sum(Total))
killed_bor_total <- killed_bor_total[killed_bor_total$BOROUGH != "", ]
write.csv(killed_bor_total, "killed_borough.csv")
###reasons for accidents
reason <- just_location.c1
reason$Reason <- refined$CONTRIBUTING.FACTOR.VEHICLE.1
top10_reasons <- reason %>% group_by(Reason) %>% summarise(Total_accidents = n()) %>% top_n(20, Total_accidents)
top10_reasons <- top10_reasons[-19,]
top10_reasons <- top10_reasons[order(top10_reasons$Total_accidents, decreasing = TRUE),]


##Vehical type code which lead to max injuries
vehical <- just_location.c1[,c("BOROUGH", "Injured")]
vehical$Vehical_type <- refined$VEHICLE.TYPE.CODE.1
top10_type <- vehical %>% group_by(Vehical_type) %>% summarise(Injured = sum(Injured)) %>% top_n(20, Max_Injured)
top10_type <- top10_type[-c(1,4),]
top10_type <- top10_type[order(top10_type$Max_Injured, decreasing = T),]
###Trends in missing values
library(VIM)
mice_plot <- aggr(just_location, col=c('orange', 'brown'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(just_location), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

##Imputing Borough
boroughs <- just_location.c
boroughs$Bor <- refined$BOROUGH
boroughs$ZIP.CODE <- as.character(boroughs$ZIP.CODE)
for(i in 1:212057)
{
  if(grepl("104", boroughs[i,1])){
    boroughs[i,6]<-"BRONX"
  }
  else if(grepl("103", boroughs[i,1])){
    boroughs[i,6]<-"S"
  }
  else if(grepl("112", boroughs[i,1])){
    boroughs[i,6]<-"BROOKLYN"
  }
  else if(grepl("100", boroughs[i,1])){
    boroughs[i,6]<-"MANHATTAN"
  }
  else if(grepl("110", boroughs[i,1])||grepl("111", boroughs[i,1])||grepl("113", boroughs[i,1])||grepl("114", boroughs[i,1])||grepl("116", boroughs[i,1])){
    
    boroughs[i,6]<-"QUEENS"
  }
}
boroughs_r <- boroughs[boroughs$Bor != "", ]
just_location.c1$BOROUGH <- boroughs$Bor



###Creating animated maps
###creating a subset for animated maps
date_sub <- subset(refined, DATE > "2017-09-21" & DATE < "2017-09-30")
date_sub <- date_sub[,c(1,5,6)]
date_sub <- date_sub[complete.cases(date_sub), ]
d1 <- dplyr::filter(date_sub, DATE == "2017-09-22")
d2 <- dplyr::filter(date_sub, DATE <= "2017-09-23")
d3 <- dplyr::filter(date_sub, DATE <= "2017-09-24")
d4 <- dplyr::filter(date_sub, DATE <= "2017-09-25")
d5 <- dplyr::filter(date_sub, DATE <= "2017-09-26")
d6 <- dplyr::filter(date_sub, DATE <= "2017-09-27")
d7 <- dplyr::filter(date_sub, DATE <= "2017-09-28")
d8 <- dplyr::filter(date_sub, DATE <= "2017-09-29")

install.packages('ggmap')
library(ggmap)
lat <- range(date_sub$LATITUDE)
lng <- range(date_sub$LONGITUDE)
nyc <- get_map(location =c(mean(lng), mean(lat)), maptype = "terrain-lines", zoom = 10)
ggmap(nyc) + ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc22 <- ggmap(nyc) +  geom_point(data = d1, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
        size = 3, shape = 21, show.legend = F) + 
        ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
        ylab("LONGITUDE")
        
nyc23 <- ggmap(nyc) +  geom_point(data = d2, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc24 <- ggmap(nyc) +  geom_point(data = d3, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc25 <- ggmap(nyc) +  geom_point(data = d4, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc26 <- ggmap(nyc) +  geom_point(data = d5, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc27 <- ggmap(nyc) +  geom_point(data = d6, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") +xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc28 <- ggmap(nyc) +  geom_point(data = d7, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")

nyc29 <- ggmap(nyc) +  geom_point(data = d8, aes(x = LONGITUDE, y = LATITUDE, alpha = 0.5, fill = "red"),
                                  size = 3, shape = 21, show.legend = F) + 
  ggtitle("Vehicular Collisions in NYC from 22nd Sept, 2017 to 29th Sept, 2017") + xlab("LATITUDE")+
  ylab("LONGITUDE")


##///////////////////////////
ani_data <- dtcopy %>% filter(DATE >= "2017-09-22" & DATE <= "2017-09-29")
ani_data1 <- ani_data %>% group_by(DATE) %>% summarise(Crash = n())
i <- ani_data[,c(1,11,13,15,17)]
i$Injured <- rowSums(i[,-1])
i1 <- i[i$Injured != 0, ]
i_final <- i1 %>% group_by(DATE) %>% summarise(Injured = sum(Injured))

k <- ani_data[,c(1,12,14,16,18)]
k$Killed <- rowSums(k[,-1])
k1 <- k[k$Killed != 0, ]
k_final <- k1 %>% group_by(DATE) %>% summarise(Killed = sum(Killed))
k_final
i_final
