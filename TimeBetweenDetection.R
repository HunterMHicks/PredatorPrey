library("dplyr")
library("lubridate")
library("data.table")




######NOTES ABOUT CODE AND FINAL TABLE#########
#'When creating the datetime column, be sure to check the format and make sure its the same as the format in the csv file
#'TimeSince___ columns maybe calculating time differences between camera locations which may result in negative numbers
#'TimeSince___ data that is in the first row of the camera location may not be accurate, if it is prey, the time difference is not actually a valid data point so you would want to filter based on that fact
#'If looking at prey data make sure LastDetWasPred = 1 to avoid the first few prey data that do not have accurate time detections relating to predators






#read in the csv file
data <- read.csv("Bassing_AllDetections18-20_2021-09-13.csv")
#only takes the animal data
data <- data[(data$Animal == "true" | data$Animal == "TRUE" ),]

#Creates matrix with location, date, time, DateTime and species
data <- data[c("CameraLocation","Date","Time","DateTime","Species")]

########If datetime column is not already created, proceed with following code...
#creates a combined DateTime Column separated by a space
data$DateTime <- paste(data$Date,data$Time, sep=" ")

#creates a combined DateTimeCheck Column separated by a space for back up purposes
data$DateTimeCheck <- paste(data$Date,data$Time, sep=" ")

#Tells R that DateTime column is not just random numbers
data$DateTime <- as.POSIXct(data$DateTime, format= "%Y-%m-%d %H:%M:%S", tz= "America/Los_Angeles")



#'  Create a column identifying whether each image is an "independent" event
#'  Remove all Service and Empty images for this to work
dat <- filter(data, !is.na(Species)) %>%
  arrange(CameraLocation, DateTime)
#'  Empty vector to be filled
caps <- c()
#'  Fill first element of the vector to get it started (1st detection event)
caps[1] <- 1
#'  Giant for loop to run through the rest of the dataset
for (i in 2:nrow(dat)){
  #'  If species detected is different from previous row then give a unique     #'  value. If not then...
  if (dat$Species[i-1] != dat$Species[i]) caps[i] = i
  #'  Capture value is the same as that in the previous row
  else caps[i] = caps[i-1]
} # close loop
#'  Formate the caps vector as a factor  
caps <- as.factor(caps)
#'  Add new column to larger data set
capdata <- cbind(as.data.frame(dat), caps)

############# Remove non ungulate or predator animals
capdata <- capdata[capdata$Species == "Coyote" | capdata$Species == "Cougar" 
                   | capdata$Species == "Bobcat"| capdata$Species == "Black Bear"
                   | capdata$Species == "Lynx"| capdata$Species == "Wolf"
                   | capdata$Species == "Elk"| capdata$Species == "Mule Deer"
                   | capdata$Species == "White-tailed Deer"| capdata$Species == "Moose",]

#creates new column where predators get predator label and everything else is prey
capdata$PredPrey <- with(capdata, ifelse(capdata$Species == "Coyote" | capdata$Species == "Cougar" 
                                         | capdata$Species == "Bobcat"| capdata$Species == "Black Bear"
                                         | capdata$Species == "Lynx"| capdata$Species == "Wolf", 'Predator', 'Prey'))

#Create Pred and Prey data sets
Pred <- filter(capdata, capdata$PredPrey == "Predator")
Prey <- filter(capdata, capdata$PredPrey == "Prey")

#grouping pred data to be the last image detection
lastdetection <- Pred %>% 
  group_by(caps) %>% 
  slice_tail() %>%
  ungroup()

#grouping prey data to be the first image detection
firstdetection <- Prey %>% 
  group_by(caps) %>% 
  slice_head() %>%
  ungroup()

#merge
Mergedat <- merge(lastdetection, firstdetection, all = T)
TimeDetect <- Mergedat[order(Mergedat$CameraLocation),]

#Find the count since last predator detection
#'  Fill first element of the vector to get it started 
TimeDetect$DetectionsSincePred[1] <- 0
#'  Giant for loop to run through the rest of the dataset
for (i in 2:nrow(TimeDetect)){
  #'  If species is predator give value 0
  if (TimeDetect$PredPrey[i] == "Predator") TimeDetect$DetectionsSincePred[i] = 0
  #'  Otherwise add one to the DetectionsSincePred
  else  TimeDetect$DetectionsSincePred[i] = TimeDetect$DetectionsSincePred[i-1]+1
} # close loop


######Loops that calculate time since last predator

#set up empty vector to be filled
TimeDetect$TimeSinceLastPredSec <- c()
#'  Fill first element of the vector to get it started 
TimeDetect$TimeSinceLastPredSec[1] <- 0
#'  Giant for loop to run through the rest of the dataset
for (i in 2:nrow(TimeDetect)){
  #'  If last detection was a predator, the time difference equals difference between current row and last
  if (TimeDetect$PredPrey[i-1] == "Predator") TimeDetect$TimeSinceLastPredSec[i] = difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="sec")
  #'  Otherwise take if last detection wasn't a predator, take time difference between current row and last row and add that to the time since last predator in the previous row
  #'  *This will also give time elapsed between predator detection
  #'  *Does not give accurate calculations if predator hasn't been detected yet, so first few detection are not related to predator
  else  TimeDetect$TimeSinceLastPredSec[i] = (TimeDetect$TimeSinceLastPredSec[i-1] + difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="sec"))
}

#repeat for units= minutes
TimeDetect$TimeSinceLastPredMin <- c()
TimeDetect$TimeSinceLastPredMin[1] <- 0
for (i in 2:nrow(TimeDetect)){
  if (TimeDetect$PredPrey[i-1] == "Predator") TimeDetect$TimeSinceLastPredMin[i] = difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="min")
  else  TimeDetect$TimeSinceLastPredMin[i] = (TimeDetect$TimeSinceLastPredMin[i-1] + difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="min"))
}
#repeat for units=hours
TimeDetect$TimeSinceLastPredHour <- c()
TimeDetect$TimeSinceLastPredHour[1] <- 0
for (i in 2:nrow(TimeDetect)){
  if (TimeDetect$PredPrey[i-1] == "Predator") TimeDetect$TimeSinceLastPredHour[i] = difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="hour")
  else  TimeDetect$TimeSinceLastPredHour[i] = (TimeDetect$TimeSinceLastPredHour[i-1] + difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="hour"))
}
#repeat for units= days
TimeDetect$TimeSinceLastPredDay <- c()
TimeDetect$TimeSinceLastPredDay[1] <- 0
for (i in 2:nrow(TimeDetect)){
  if (TimeDetect$PredPrey[i-1] == "Predator") TimeDetect$TimeSinceLastPredDay[i] = difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="day")
  else  TimeDetect$TimeSinceLastPredDay[i] = (TimeDetect$TimeSinceLastPredDay[i-1] + difftime(TimeDetect$DateTime[i], TimeDetect$DateTime[i-1], units="day"))
}

#remove time differences between camera locations
for (i in 2:nrow(TimeDetect)){
  if (TimeDetect$CameraLocation[i] != TimeDetect$CameraLocation[i-1]) TimeDetect$TimeSinceLastPredSec[i] = 0
  else  TimeDetect$TimeSinceLastPredSec[i] = TimeDetect$TimeSinceLastPredSec[i]
}

#binomial : last detection = predator and in same camera location? yes=1 no =0
TimeDetect$LastDetWasPred <- with(TimeDetect, ifelse(lag(TimeDetect$PredPrey,1) == "Predator",1,0))


#print last predator in new column
TimeDetect$LastPredator[1] <- 0
for (i in 2:nrow(TimeDetect)){
  if (TimeDetect$PredPrey[i-1] == "Predator") TimeDetect$LastPredator[i] = TimeDetect$Species[i-1]
  else  TimeDetect$LastPredator[i] = TimeDetect$LastPredator[i-1]
} 

#difference in time since last detection
TimeDetect$TimeSinceLastDetectSec <- difftime(TimeDetect$DateTime, lag(TimeDetect$DateTime,1))
TimeDetect$TimeSinceLastDetectMin <- difftime(TimeDetect$DateTime, lag(TimeDetect$DateTime,1),units = "min")
TimeDetect$TimeSinceLastDetectHour <- difftime(TimeDetect$DateTime, lag(TimeDetect$DateTime,1),units = "hour")
TimeDetect$TimeSinceLastDetectDay <- difftime(TimeDetect$DateTime, lag(TimeDetect$DateTime,1),units = "day")

#removing data that is calculating values from different camera location
#is row within same camera location True or false
TimeDetect$CamPrevSame <- with(TimeDetect, ifelse(lag(TimeDetect$CameraLocation,1) == TimeDetect$CameraLocation,'True','False'))
#Remove all the data that is false
TimeDetect <- subset(TimeDetect, TimeDetect$CamPrevSame == "True" )

#Remove unnecessary columns
TimeDetect<- TimeDetect %>% select(-one_of("Date","Time","DateTimeCheck","caps","CamPrevSame"))

#write the final csv file that has all the predator and prey time differences
write.csv(TimeDetect, "Time_Between_Detections.csv",row.names = F)

