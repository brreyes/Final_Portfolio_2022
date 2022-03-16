#
# author: Brandon Reyes
# Final Poster Project
#

setwd("~/Desktop/IST719")
list.files()
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

stats <- read.csv("nbastats19-21.csv"
                  , header = TRUE
                  , stringsAsFactors = FALSE)

View(stats)
stats$YEAR[1:50]<-c("2021","2021","2021","2021","2021","2021",
                    "2021","2021","2021","2021","2021","2021",
                    "2021","2021","2021","2021","2021","2021",
                    "2021","2021","2021","2021","2021","2021","2021","2020",
                    "2020","2020","2020","2020","2020","2020",
                    "2020","2020","2020","2020","2020","2020",
                    "2020","2020","2020","2020","2020","2020",
                    "2020","2020","2020","2020","2020","2020")


#Points per year by players
pts.by.year <- aggregate(stats$PTS,
                               list (reg = stats$Player,
                                     type = stats$YEAR), sum)
colnames(pts.by.year)[1] <- "Players"
colnames(pts.by.year)[2] <- "Year"
colnames(pts.by.year)[3] <- "Points"

ggplot(pts.by.year) +
  aes(x = Players, y = Points, fill = as.factor(Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkorange2", "darkcyan")) +
  theme_classic() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
  + ggtitle("Points Scored Per Game 2020 vs 2021")

# Games played
ggplot(stats) +
  aes(x = Player, y = as.factor(G), color = as.factor(YEAR)) +
  geom_point() +
  scale_color_manual(values = c("darkorange2", "darkcyan")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Player performance rose 2021
data<-stats[1:25,]
data1<-data.frame(t(data))
data2<-data1[1:29,]
View(data2)
colnames(data2)=data$Player
data2$group=row.names(data2)
data3=melt(data2,id="group")
View(data3)
data3$value=as.numeric(data3$value)
head(data3)
data3<-data3[!data3$group=='Age',]
View(data3)

ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",width=1,colour="black",size=0.1) + 
  coord_polar() +
  xlab("") + ylab("")

#Player performance rose 2020
data<-stats[26:50,]
data1<-data.frame(t(data))
data2<-data1[1:29,]
View(data2)
colnames(data2)=data$Player
data2$group=row.names(data2)
data3=melt(data2,id="group")
View(data3)
data3$value=as.numeric(data3$value)
head(data3)
data3<-data3[!data3$group=='Age',]
View(data3)

ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",width=1,colour="black",size=0.1) + 
  coord_polar() +
  xlab("") + ylab("")

#Free throw percentages by year
ggplot(stats, aes(x = Player, y = FT., group = YEAR, color=YEAR)) + 
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = stats$Player, labels = stats$Player) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#8 out of 25 players have dropped in ft%


str(stats$YEAR)
as.factor(as.numeric(stats$YEAR))
str(stats)

#######barplot showing games played in 2020#######
lastyear<-stats[stats$YEAR=="2020",]
lastyeardata1<-as.data.frame(lastyear)
#View(lastyeardata1)
lastyeardata<-as.data.frame(table(lastyear$Player))
#View(lastyeardata)
colnames(lastyeardata)<- c("Player", "Frequency")
lastyeardata$GamesPlayed<-lastyeardata1$G
lastyeardata$GamesPlayedProb<-lastyeardata$GamesPlayed/73
rownames(lastyeardata) <- lastyeardata[,1]
#lastyeardata<-lastyeardata[,-1]
#lastyeardata<-lastyeardata[,-1]
barplot(lastyeardata$GamesPlayedProb, 
        names=thisyear$Player, 
        xlab = "Percentage of games played in 2020",
        ylim=c(0,25), xlim = c(0,1), 
        col="#008B8B", horiz=TRUE) 

###########barplot showing decline in games played

thisyear<-stats[stats$YEAR=="2021",]
thisyeardata1<-as.data.frame(thisyear)
#View(thisyeardata1)
thisyeardata<-as.data.frame(table(thisyear$Player))
#View(thisyeardata)
colnames(thisyeardata)<- c("Player", "Frequency")
thisyeardata$GamesPlayed<-thisyeardata1$G
thisyeardata$GamesPlayedProb<-thisyeardata$GamesPlayed/72
rownames(thisyeardata) <- thisyeardata[,1]
thisyeardata<-thisyeardata[,-1]
thisyeardata<-thisyeardata[,-1]
par(las=1, mar=c(5,8,5,3))
barplot(thisyeardata$GamesPlayedProb, 
        names=thisyear$Player, 
        xlab = "Percentage of games played in 2021",
        ylim=c(0,25), xlim = c(0,1), col="#EE7600", horiz=TRUE , las=1)




levels(stats$YEAR) <- c("Semi-urban", "Rural") 

ggplot(data = stats,          # Use the ghana dataset.       
       aes(x = TOV,           # The x axis aesthetic is BMI.
           fill = YEAR)      # The fill aesthetic is rural.
) +                    # Add in more features.
  geom_density(alpha = 0.2) + # Add a density based on the aesthetics.
  theme_bw(base_size = 24) +  # black and white background with big font
  labs(fill = "") +           # no label for the fill variable above the key
  scale_fill_manual(values = c("darkorange2","darkcyan")) + # fill colors
  xlab("Turnovers Per Game") +    # label for x axis
  ylab ("Proportion of Players") +                 # label for y axis
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

###########

install.packages("ggalt")
library(ggalt)
theme_set(theme_classic())

stats$FT. <- factor(stats$FT., levels=as.character(stats$FT.))  # for right ordering of the dumbells

gg <- ggplot(stats, aes(x=FT.,xend=FT., y=Player, group=YEAR)) + 
  geom_dumbbell(color="darkorange2", 
                size=0.75, 
                point.color="darkcyan") + 
  scale_x_continuous(label=percent) + 
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart", 
       subtitle="FT Pct Change: 2020 vs 2021") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Minutes Played in 2020
hist(lastyear$MP, breaks = 10, prob = T, main = "Minutes Played in 2020", col = "darkorange2",
     xlim = c(26,38), ylim = c(0,0.3))
points(density(lastyear$MP),type="l",col="darkcyan", lwd = 3)
rug(lastyear$MP,col="red")

#Minutes Played in 2021
hist(thisyear$MP, breaks = 10, prob = T, main = "Minutes Played in 2021", col = "darkcyan",
     xlim = c(13,40), ylim = c(0,0.15))
points(density(thisyear$MP),type="l",col="darkorange2", lwd = 3)
rug(lastyear$MP,col="red")