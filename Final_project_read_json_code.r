# Code needed to read in the data for your final project

install.packages("jsonlite")   # Install the needed package to read a json file
library(jsonlite)  #library or check the installed jsonlite package in to use

mydata.list <- fromJSON("completeSurvey.json" ) #read in the json file called completeSurvey.json
survey <- data.frame(mydata.list) # Convert json file to a data.fram
str(survey) # start to get to know your data...many more get to know your data funcitons needed
View(survey)

install.packages("sqldf")
library(sqldf)

install.packages("ggplot2")
library(ggplot2)

survey$Age[is.na(survey$Age)]#make sure there are no NA data

####################detractors vs age######################
detractors<-subset(survey, Likelihood.to.recommend<7) 
#creating a subset of data where likelihood to recommend < 7 aka detractors
detractors
#creating a histogram to show the frequency of age groups that are detractors
detractors_age<-ggplot(detractors,aes(x=Age))+
  geom_histogram(color="black",fill="white",binwidth = 10)+ggtitle("Detractors vs. Age")
detractors_age
#creating a boxplot to show the distribution of the data, show the median
age_boxpolt <- ggplot(detractors, aes(x=Age)) + geom_boxplot()+ggtitle("Age vs Detractors")
age_boxpolt


####################promoters vs age######################
#creating a subset of data where likelihood to recommend is > 8 aka promoters
promoters<-subset(survey, Likelihood.to.recommend>8)
promoters
#creating a histogram to show the frequency of age groups that are promoters
promoters_age<-ggplot(promoters,aes(x=Age))+
  geom_histogram(color="black",fill="white",binwidth = 10)+ggtitle("Promoters vs. Age")
promoters_age
#creating a boxplot to show the distribution of the data, show the median
age_boxpoltp <- ggplot(promoters, aes(x=Age)) + geom_boxplot()+ggtitle("Age vs Promoters")
age_boxpoltp

####################Airline Status vs Likelihood.to.recommend######################
unique(survey$Airline.Status)
#[1] Silver   Gold     Blue     Platinum
#Levels: Blue Gold Platinum Silver
#here we set the airline status values as numeric
survey$Airline.Status <- as.numeric(as.factor(survey$Airline.Status))
View(survey)
#here we create a ggplot count between airline status and likelihood to recommend
ggplot(survey,aes(x=Airline.Status,y=Likelihood.to.recommend))+geom_count()+
  stat_summary(aes(y =Likelihood.to.recommend ,group=1), fun=mean, colour="blue", geom="line",group=1)+ggtitle("Status vs Satisfaction")

####################Gender vs Likelihood.to.recommend######################
for(i in 1:nrow(survey)){
  if (survey$Gender[i] == 'Male'){
    survey$Gender[i] <- 1
  } else if (survey$Gender[i] == 'Female'){
    survey$Gender [i] <- 0
  }
}
survey$Gender <- as.numeric(survey$Gender) #changing data type of gender to numeric and setting male = 1 with female = 0

#a ggplot showing the amount of detractors/promoters for each female & male
ggplot(survey,aes(x=Gender,y=Likelihood.to.recommend))+geom_count()+
  stat_summary(aes(y =Likelihood.to.recommend ,group=1), fun=mean, colour="blue", geom="line",group=1)+ggtitle("Gender vs Satisfaction")

#########################florida vs departure delay>5###########################
florida<-survey[which(survey$Origin.State=="Florida"),]
florida_Pro<-florida[which(florida$Likelihood.to.recommend>8),]
florida_Det<-florida[which(florida$Likelihood.to.recommend<7),]
florida_Pas<-florida[which(florida$Likelihood.to.recommend==8|florida$Likelihood.to.recommend==7),]

florida_Det<-florida_Det[which(florida_Det$Departure.Delay.in.Minutes>5),]
florida_Pro<-florida_Pro[which(florida_Pro$Departure.Delay.in.Minutes>5),]
florida_Pas<-florida_Pas[which(florida_Pas$Departure.Delay.in.Minutes>5),]
florida_Det$Factor<-"Det"
florida_Pro$Factor<-"Pro"
florida_Pas$Factor<-"Pas"
Det<-florida_Det[c("Factor","Departure.Delay.in.Minutes")]
Pas<-florida_Pas[c("Factor","Departure.Delay.in.Minutes")]
Pro<-florida_Pro[c("Factor","Departure.Delay.in.Minutes")]
florida_factors<-rbind(Det,Pas,Pro)
florida_factors

detractors_fl<-ggplot(florida_factors,aes(x=Factor,y=Departure.Delay.in.Minutes))+geom_count()+
  stat_summary(aes(y =Departure.Delay.in.Minutes ,group=1), fun=mean, colour="blue", geom="line",group=1)+ggtitle("Dep.Delay > 5 minutes vs Satisfaction in Florida")
detractors_fl

#########################florida vs arrival delay>5###########################
florida_Det2<-florida_Det[which(florida_Det$Arrival.Delay.in.Minutes>5),]
florida_Pro2<-florida_Pro[which(florida_Pro$Arrival.Delay.in.Minutes>5),]
florida_Pas2<-florida_Pas[which(florida_Pas$Arrival.Delay.in.Minutes>5),]
florida_Det2$Factor<-"Det"
florida_Pro2$Factor<-"Pro"
florida_Pas2$Factor<-"Pas"
Det<-florida_Det2[c("Factor","Arrival.Delay.in.Minutes")]
Pas<-florida_Pas2[c("Factor","Arrival.Delay.in.Minutes")]
Pro<-florida_Pro2[c("Factor","Arrival.Delay.in.Minutes")]
florida_factors2<-rbind(Det,Pas,Pro)
florida_factors2

detractors_fl2<-ggplot(florida_factors2,aes(x=Factor,y=Arrival.Delay.in.Minutes))+geom_count()+
  stat_summary(aes(y =Arrival.Delay.in.Minutes ,group=1), fun=mean, colour="blue", geom="line",group=1)+ggtitle("Arr.Delay > 5 minutes vs Satisfaction in Florida")
detractors_fl2

#########################florida vs arrival delay>30###########################
florida_Det3<-florida_Det[which(florida_Det$Arrival.Delay.in.Minutes>30),]
florida_Pro3<-florida_Pro[which(florida_Pro$Arrival.Delay.in.Minutes>30),]
florida_Pas3<-florida_Pas[which(florida_Pas$Arrival.Delay.in.Minutes>30),]
florida_Det3$Factor<-"Det"
florida_Pro3$Factor<-"Pro"
florida_Pas3$Factor<-"Pas"
Det<-florida_Det3[c("Factor","Arrival.Delay.in.Minutes")]
Pas<-florida_Pas3[c("Factor","Arrival.Delay.in.Minutes")]
Pro<-florida_Pro3[c("Factor","Arrival.Delay.in.Minutes")]
florida_factors3<-rbind(Det,Pas,Pro)
florida_factors3

detractors_fl3<-ggplot(florida_factors3,aes(x=Factor,y=Arrival.Delay.in.Minutes))+geom_count()+
  stat_summary(aes(y =Arrival.Delay.in.Minutes ,group=1), fun=mean, colour="blue", geom="line",group=1)+ggtitle("Arr.Delay > 30 minutes vs Satisfaction in Florida")
detractors_fl3

#########################florida vs departure delay>30###########################
florida_Det4<-florida_Det[which(florida_Det$Departure.Delay.in.Minutes>30),]
florida_Pro4<-florida_Pro[which(florida_Pro$Departure.Delay.in.Minutes>30),]
florida_Pas4<-florida_Pas[which(florida_Pas$Departure.Delay.in.Minutes>30),]
florida_Det4$Factor<-"Det"
florida_Pro4$Factor<-"Pro"
florida_Pas4$Factor<-"Pas"
Det<-florida_Det4[c("Factor","Departure.Delay.in.Minutes")]
Pas<-florida_Pas4[c("Factor","Departure.Delay.in.Minutes")]
Pro<-florida_Pro4[c("Factor","Departure.Delay.in.Minutes")]
florida_factors4<-rbind(Det,Pas,Pro)
florida_factors4

detractors_fl4<-ggplot(florida_factors4,aes(x=Factor,y=Departure.Delay.in.Minutes))+geom_count()+
  stat_summary(aes(y =Departure.Delay.in.Minutes ,group=1), fun=mean, colour="blue", geom="line",group=1)+ggtitle("Dep.Delay > 30 minutes vs Satisfaction in Florida")
detractors_fl4
