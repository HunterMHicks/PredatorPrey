
library(plotly)


TimeDetect1 <- read.csv("Time_Between_Detections.csv")

#set data to the first prey detection 
PredFirstPrey <- TimeDetect1[TimeDetect1$LastDetWasPred==1,]

#Transform the data to eliminate extremes
sqrt_Day <- sqrt(PredFirstPrey$TimeSinceLastPredDay)
#plot scatter of days til first predator
ggplot(PredFirstPrey, aes(x=LastPredator, y=sqrt_Day)) +
  geom_boxplot(fill='steelblue') + labs(x='Predator Species', y='SQRT Days til Prey Detected')



#Bar chart of average time until prey detected by predator
BlackBear <- PredFirstPrey[PredFirstPrey$LastPredator == "Black Bear" & PredFirstPrey$PredPrey == "Prey",] 
Bobcat <- PredFirstPrey[PredFirstPrey$LastPredator == "Bobcat" & PredFirstPrey$PredPrey == "Prey",]
Cougar<- PredFirstPrey[PredFirstPrey$LastPredator == "Cougar" & PredFirstPrey$PredPrey == "Prey",]
Coyote<- PredFirstPrey[PredFirstPrey$LastPredator == "Coyote" & PredFirstPrey$PredPrey == "Prey",]
Lynx<- PredFirstPrey[PredFirstPrey$LastPredator == "Lynx" & PredFirstPrey$PredPrey == "Prey",]
Wolf<- PredFirstPrey[PredFirstPrey$LastPredator == "Wolf" & PredFirstPrey$PredPrey == "Prey",]


meantime <- c(mean(BlackBear$TimeSinceLastPredDay),mean(Bobcat$TimeSinceLastPredDay),mean(Cougar$TimeSinceLastPredDay),mean(Coyote$TimeSinceLastPredDay),mean(Lynx$TimeSinceLastPredDay),mean(Wolf$TimeSinceLastPredDay))
Predators <- c("Black Bear", "Bobcat","Cougar","Coyote","Lynx","Wolf")
barplot(meantime, names.arg = Predators, xlab = "Predator", ylab=
          "Mean Days until Prey Detected", col = "blue", main = "Average Prey Reaction to Predators")




#Bar Plot of Mule Deer and predators with error bars
BlackBearMule <- PredFirstPrey[PredFirstPrey$LastPredator == "Black Bear" & PredFirstPrey$Species == "Mule Deer",] 
BobcatMule <- PredFirstPrey[PredFirstPrey$LastPredator == "Bobcat" & PredFirstPrey$Species == "Mule Deer",]
CougarMule<- PredFirstPrey[PredFirstPrey$LastPredator == "Cougar" & PredFirstPrey$Species == "Mule Deer",]
CoyoteMule<- PredFirstPrey[PredFirstPrey$LastPredator == "Coyote" & PredFirstPrey$Species == "Mule Deer",]
LynxMule<- PredFirstPrey[PredFirstPrey$LastPredator == "Lynx" & PredFirstPrey$Species == "Mule Deer",]
WolfMule<- PredFirstPrey[PredFirstPrey$LastPredator == "Wolf" & PredFirstPrey$Species == "Mule Deer",]
#means Mule
MeanBlackBearMule <- mean(BlackBearMule$TimeSinceLastPredDay)
MeanBobcatMule <- mean(BobcatMule$TimeSinceLastPredDay)
MeanCougarMule <- mean(CougarMule$TimeSinceLastPredDay)
MeanCoyoteMule <- mean(CoyoteMule$TimeSinceLastPredDay)
MeanLynxMule <- mean(LynxMule$TimeSinceLastPredDay)
MeanWolfMule <- mean(WolfMule$TimeSinceLastPredDay)
#STDEV Mule
SDBlackBearMule <- sd(BlackBearMule$TimeSinceLastPredDay)
SDBobcatMule <- sd(BobcatMule$TimeSinceLastPredDay)
SDCougarMule <- sd(CougarMule$TimeSinceLastPredDay)
SDCoyoteMule <- sd(CoyoteMule$TimeSinceLastPredDay)
SDLynxMule <- sd(LynxMule$TimeSinceLastPredDay)
SDWolfMule <- sd(WolfMule$TimeSinceLastPredDay)
#Standard Error Mule
SEBlackBearMule <- SDBlackBearMule / sqrt(length(BlackBearMule))
SEBobcatMule <- SDBobcatMule / sqrt(length(BobcatMule))
SECougarMule <- SDCougarMule / sqrt(length(CougarMule))
SECoyoteMule <- SDCoyoteMule / sqrt(length(CoyoteMule))
SELynxMule <- SDLynxMule / sqrt(length(LynxMule))
SEWolfMule <- SDWolfMule / sqrt(length(WolfMule))

Predators <- c("Black Bear", "Bobcat","Cougar","Coyote","Lynx","Wolf")
MeanMule <- c(MeanBlackBearMule,MeanBobcatMule,MeanCougarMule,MeanCoyoteMule,MeanLynxMule,MeanWolfMule)
SEMule <- c(SEBlackBearMule, SEBobcatMule,SECougarMule,SECoyoteMule,SELynxMule,SEWolfMule)
Species <- rep(c("Mule"),times=6)

MuleDat <- data.frame(Predators,MeanMule,SEMule,Species)


ggplot(MuleDat, aes(x=Predators,y=MeanMule,fill=Species)) +
  geom_bar(position = position_dodge(),stat = "identity", fill="steelblue") +
  geom_errorbar(aes(ymin=MeanMule-SEMule, ymax=MeanMule+SEMule),width=.2,position = position_dodge(.9)) +
  labs(x="Predator Species", y="Mean Number of Days til Mule Detected", title = "Average Time Between Predator and First Mule Detections")





#########Mean, SD, and SE for other prey########

#WT
BlackBearWT <- PredFirstPrey[PredFirstPrey$LastPredator == "Black Bear" & PredFirstPrey$Species == "White-tailed Deer",] 
BobcatWT <- PredFirstPrey[PredFirstPrey$LastPredator == "Bobcat" & PredFirstPrey$Species == "White-tailed Deer",]
CougarWT<- PredFirstPrey[PredFirstPrey$LastPredator == "Cougar" & PredFirstPrey$Species == "White-tailed Deer",]
CoyoteWT<- PredFirstPrey[PredFirstPrey$LastPredator == "Coyote" & PredFirstPrey$Species == "White-tailed Deer",]
LynxWT<- PredFirstPrey[PredFirstPrey$LastPredator == "Lynx" & PredFirstPrey$Species == "White-tailed Deer",]
WolfWT<- PredFirstPrey[PredFirstPrey$LastPredator == "Wolf" & PredFirstPrey$Species == "White-tailed Deer",]
#Mean WT
MeanBlackBearWT <- mean(BlackBearWT$TimeSinceLastPredDay)
MeanBobcatWT <- mean(BobcatWT$TimeSinceLastPredDay)
MeanCougarWT <- mean(CougarWT$TimeSinceLastPredDay)
MeanCoyoteWT <- mean(CoyoteWT$TimeSinceLastPredDay)
MeanLynxWT <- mean(LynxWT$TimeSinceLastPredDay)
MeanWolfWT <- mean(WolfWT$TimeSinceLastPredDay)
#STDEV WT
SDBlackBearWT <- sd(BlackBearWT$TimeSinceLastPredDay)
SDBobcatWT <- sd(BobcatWT$TimeSinceLastPredDay)
SDCougarWT <- sd(CougarWT$TimeSinceLastPredDay)
SDCoyoteWT <- sd(CoyoteWT$TimeSinceLastPredDay)
SDLynxWT <- sd(LynxWT$TimeSinceLastPredDay)
SDWolfWT <- sd(WolfWT$TimeSinceLastPredDay)
#Standard Error WT
SEBlackBearWT <- SDBlackBearWT / sqrt(length(BlackBearWT))
SEBobcatWT <- SDBobcatWT / sqrt(length(BobcatWT))
SECougarWT <- SDCougarWT / sqrt(length(CougarWT))
SECoyoteWT <- SDCoyoteWT / sqrt(length(CoyoteWT))
SELynxWT <- SDLynxWT / sqrt(length(LynxWT))
SEWolfWT <- SDWolfWT / sqrt(length(WolfWT))



#Moose
BlackBearMoose <- PredFirstPrey[PredFirstPrey$LastPredator == "Black Bear" & PredFirstPrey$Species == "Moose",] 
BobcatMoose <- PredFirstPrey[PredFirstPrey$LastPredator == "Bobcat" & PredFirstPrey$Species == "Moose",]
CougarMoose<- PredFirstPrey[PredFirstPrey$LastPredator == "Cougar" & PredFirstPrey$Species == "Moose",]
CoyoteMoose<- PredFirstPrey[PredFirstPrey$LastPredator == "Coyote" & PredFirstPrey$Species == "Moose",]
LynxMoose<- PredFirstPrey[PredFirstPrey$LastPredator == "Lynx" & PredFirstPrey$Species == "Moose",]
WolfMoose<- PredFirstPrey[PredFirstPrey$LastPredator == "Wolf" & PredFirstPrey$Species == "Moose",]
#Mean Moose
MeanBlackBearMoose <- mean(BlackBearMoose$TimeSinceLastPredDay)
MeanBobcatMoose <- mean(BobcatMoose$TimeSinceLastPredDay)
MeanCougarMoose <- mean(CougarMoose$TimeSinceLastPredDay)
MeanCoyoteMoose <- mean(CoyoteMoose$TimeSinceLastPredDay)
MeanLynxMoose <- mean(LynxMoose$TimeSinceLastPredDay)
MeanWolfMoose <- mean(WolfMoose$TimeSinceLastPredDay)
#STDEV Moose
SDBlackBearMoose <- sd(BlackBearMoose$TimeSinceLastPredDay)
SDBobcatMoose <- sd(BobcatMoose$TimeSinceLastPredDay)
SDCougarMoose <- sd(CougarMoose$TimeSinceLastPredDay)
SDCoyoteMoose <- sd(CoyoteMoose$TimeSinceLastPredDay)
SDLynxMoose <- sd(LynxMoose$TimeSinceLastPredDay)
SDWolfMoose <- sd(WolfMoose$TimeSinceLastPredDay)
#Standard Error Moose
SEBlackBearMoose <- SDBlackBearMoose / sqrt(length(BlackBearMoose))
SEBobcatMoose <- SDBobcatMoose / sqrt(length(BobcatMoose))
SECougarMoose <- SDCougarMoose / sqrt(length(CougarMoose))
SECoyoteMoose <- SDCoyoteMoose / sqrt(length(CoyoteMoose))
SELynxMoose <- SDLynxMoose / sqrt(length(LynxMoose))
SEWolfMoose <- SDWolfMoose / sqrt(length(WolfMoose))


#Elk
BlackBearElk <- PredFirstPrey[PredFirstPrey$LastPredator == "Black Bear" & PredFirstPrey$Species == "Elk",] 
BobcatElk <- PredFirstPrey[PredFirstPrey$LastPredator == "Bobcat" & PredFirstPrey$Species == "Elk",]
CougarElk<- PredFirstPrey[PredFirstPrey$LastPredator == "Cougar" & PredFirstPrey$Species == "Elk",]
CoyoteElk<- PredFirstPrey[PredFirstPrey$LastPredator == "Coyote" & PredFirstPrey$Species == "Elk",]
LynxElk<- PredFirstPrey[PredFirstPrey$LastPredator == "Lynx" & PredFirstPrey$Species == "Elk",]
WolfElk<- PredFirstPrey[PredFirstPrey$LastPredator == "Wolf" & PredFirstPrey$Species == "Elk",]
#Mean Elk
MeanBlackBearElk <- mean(BlackBearElk$TimeSinceLastPredDay)
MeanBobcatElk <- mean(BobcatElk$TimeSinceLastPredDay)
MeanCougarElk <- mean(CougarElk$TimeSinceLastPredDay)
MeanCoyoteElk <- mean(CoyoteElk$TimeSinceLastPredDay)
MeanLynxElk <- mean(LynxElk$TimeSinceLastPredDay)
MeanWolfElk <- mean(WolfElk$TimeSinceLastPredDay)
#STDEV Elk
SDBlackBearElk <- sd(BlackBearElk$TimeSinceLastPredDay)
SDBobcatElk <- sd(BobcatElk$TimeSinceLastPredDay)
SDCougarElk <- sd(CougarElk$TimeSinceLastPredDay)
SDCoyoteElk <- sd(CoyoteElk$TimeSinceLastPredDay)
SDLynxElk <- sd(LynxElk$TimeSinceLastPredDay)
SDWolfElk <- sd(WolfElk$TimeSinceLastPredDay)
#Standard Error Elk
SEBlackBearElk <- SDBlackBearElk / sqrt(length(BlackBearElk))
SEBobcatElk <- SDBobcatElk / sqrt(length(BobcatElk))
SECougarElk <- SDCougarElk / sqrt(length(CougarElk))
SECoyoteElk <- SDCoyoteElk / sqrt(length(CoyoteElk))
SELynxElk <- SDLynxElk / sqrt(length(LynxElk))
SEWolfElk <- SDWolfElk / sqrt(length(WolfElk))








###########Grouped plot

PredGroup <- (rep(c("Black Bear", "Bobcat","Cougar","Coyote","Lynx","Wolf"), each=4))
PreyGroup <- rep(c("Mule", "White-tailed Deer","Moose","Elk"), times=6)
PreyGroup
Mean <- c(MeanBlackBearMule,MeanBlackBearWT,MeanBlackBearMoose,MeanBlackBearElk,MeanBobcatMule,MeanBobcatWT,MeanBobcatMoose,MeanBobcatElk,
          MeanCougarMule,MeanCougarWT,MeanCougarMoose,MeanCougarElk,MeanCoyoteMule,MeanCoyoteWT,MeanCoyoteMoose,MeanCoyoteElk,
          MeanLynxMule,MeanLynxWT,MeanLynxMoose,MeanLynxElk,MeanWolfMule,MeanWolfWT,MeanWolfMoose,MeanWolfElk)
SE <- c(SEBlackBearMule,SEBlackBearWT,SEBlackBearMoose,SEBlackBearElk,SEBobcatMule,SEBobcatWT,SEBobcatMoose,SEBobcatElk,
        SECougarMule,SECougarWT,SECougarMoose,SECougarElk,SECoyoteMule,SECoyoteWT,SECoyoteMoose,SECoyoteElk,
        SELynxMule,SELynxWT,SELynxMoose,SELynxElk,SEWolfMule,SEWolfWT,SEWolfMoose,SEWolfElk)
GroupData <- data.frame(PredGroup,PreyGroup,Mean,SE)


ggplot(GroupData, aes(x=as.factor(PredGroup),y=Mean,fill=PreyGroup)) +
  geom_bar(position = position_dodge(),stat = "identity") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),width=.2,position = position_dodge(.9)) +
  labs(x="Predator Species", y="Mean Number of Days til Prey Detected", title = "Average Time Between Predator and First Prey Detections")

