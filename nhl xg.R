library(rvest)
library(stringr)
library(tm)
library(reshape2)
library(dplyr)
library(tidyr)
library(readimages)
library(png)
library(grid)
library(ggplot2)
library(jpeg)
library(Rcrawler)
library(jsonlite)
library(purrr)
library(Rmisc)
library(gridExtra)
library(Rcrawler)
library(purrr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(cowplot)
load("xGmodelver6.rda")
test<-readJPEG("kaukalo.jpg")
JsonData <- jsonlite::fromJSON("http://statsapi.web.nhl.com/api/v1/game/2018020158/feed/live",flatten=TRUE )

#range(json_data_frame$coordinates.x)
json_data_frame <- JsonData$liveData$plays$allPlays
hometeam<-JsonData$gameData$teams$home$id
awayteam<-JsonData$gameData$teams$away$id
fenwick_events <- c('SHOT', 'MISSED_SHOT', 'GOAL')
json_data_frame$result.secondaryType <- as.character(as.factor(json_data_frame$result.secondaryType))
json_data_frame$result.secondaryType<-ifelse(is.na(json_data_frame$result.secondaryType),'Slap Shot',json_data_frame$result.secondaryType)
print(hometeam)
unique(json_data_frame$result.secondaryType)
json_data_frame<-json_data_frame[!(json_data_frame$about.periodType=="OVERTIME"),]
filter_shootout <- function(dataframe){
  return (dataframe[dataframe$about.period < 5,])
}
json_data_frame<-filter(json_data_frame, coordinates.x !='NA'& coordinates.y !='NA')
json_data_frame$coordinates.x <- as.integer(as.numeric(json_data_frame$coordinates.x))
json_data_frame$coordinates.y <- as.integer(as.numeric(json_data_frame$coordinates.y))
json_data_frame <- json_data_frame[json_data_frame$about.period < 5,]
dim(json_data_frame)
datetime=JsonData$gameData$datetime$dateTime
hometeamlong=JsonData$gameData$teams$home$name
awayteamlong=JsonData$gameData$teams$away$name
is_home <- function(dataframe){
  dataframe$is_home <- ifelse(dataframe$team.id == 
                                hometeam, 1 , 0)
  return(dataframe)
}

is_goal <- function(dataframe){
  dataframe$is_goal <- ifelse(dataframe$result.eventTypeId == "GOAL", 1, 0)
  return(dataframe)
}

json_data_frame <- is_home(json_data_frame)
json_data_frame <- is_goal(json_data_frame)
json_data_frame$about.periodTime <- lapply(json_data_frame$about.periodTime, gsub, pattern = ":", replacement = ".", fixed = TRUE)
json_data_frame$about.periodTime <- as.numeric(as.character(json_data_frame$about.periodTime),na.rm=TRUE)
json_data_frame <- json_data_frame  %>%
  mutate(time_diff = about.periodTime - lag(about.periodTime))


json_data_frame$time_diff[is.na(json_data_frame$time_diff)] <- 0
json_data_frame$is_home[is.na(json_data_frame$is_home)] <- 0
json_data_frame$is_rebound <- ifelse(json_data_frame$time_diff < 0.03 & 
                                      json_data_frame$result.eventTypeId %in% fenwick_events &
                                      json_data_frame$team.id == 
                                      lag(json_data_frame$team.id),
                                    1, 0)

json_data_frame$is_rush <- ifelse(json_data_frame$time_diff < 0.04 &
                                   lag(abs(json_data_frame$coordinates.x)) < 25 &
                                   json_data_frame$result.eventTypeId %in% fenwick_events,
                                 1, 0)

json_data_frame$is_rebound[is.na(json_data_frame$time_diff)] <- 00.00
json_data_frame$is_rush[is.na(json_data_frame$is_rush)] <- 00.00
json_data_frame<- filter(json_data_frame, result.eventTypeId %in% c("SHOT", "MISSED_SHOT", "GOAL"))

unique(json_data_frame$result.secondaryType)
unique(json_data_frame$coordinates.x)
unique(json_data_frame$coordinates.y)
json_data_frame$x<-json_data_frame$coordinates.x
json_data_frame$y<-json_data_frame$coordinates.y
json_data_frame$result.secondaryType<- as.factor(json_data_frame$result.secondaryType)




json_data_frame$coordinates.y <- ifelse(json_data_frame$coordinates.x < 0,
                                      -1 * json_data_frame$coordinates.y, json_data_frame$coordinates.y)

json_data_frame$coordinates.x <- abs(json_data_frame$coordinates.x)


json_data_frame$shot_angle <- (asin(abs(json_data_frame$coordinates.y)/sqrt((87.95 
                                                                              - abs(json_data_frame$coordinates.x))^2
                                                                             + json_data_frame$coordinates.y^2))*180)/ 3.14

json_data_frame$shot_angle <- ifelse(abs(json_data_frame$coordinates.x) > 88, 90 + 
                                          (180-(90 + json_data_frame$shot_angle)), 
                                     json_data_frame$shot_angle)

json_data_frame$distance <- sqrt((87.95 - abs(json_data_frame$coordinates.x))^2 + json_data_frame$coordinates.y^2)

is_homex <- function(dataframe){
  dataframe$homex <- ifelse(hometeam == 
                              dataframe$team.id, json_data_frame$coordinates.x , NA)
  return(dataframe)
}

is_homey <- function(dataframe){
  dataframe$homey <- ifelse(hometeam== dataframe$team.id, json_data_frame$coordinates.y, NA)
  return(dataframe)
}

json_data_frame <- is_homex(json_data_frame)
json_data_frame <- is_homey(json_data_frame)

is_awayx <- function(dataframe){
  dataframe$awayx <- ifelse(awayteam == 
                              dataframe$team.id, json_data_frame$coordinates.x , NA)
  return(dataframe)
}

is_awayy <- function(dataframe){
  dataframe$awayy <- ifelse(awayteam== dataframe$team.id, json_data_frame$coordinates.y, NA)
  return(dataframe)
}
json_data_frame <- is_awayx(json_data_frame)
json_data_frame <- is_awayy(json_data_frame)



json_data_frame$awayx = -abs(json_data_frame$awayx)
json_data_frame$away1<- json_data_frame$awayy *-1
json_data_frame$homey *-1



json_data_frame$xG <- predict(xGmodel,json_data_frame, type = "response",na.rm=TRUE)

is_homexg <- function(dataframe){
  dataframe$homexg <- ifelse(dataframe$team.id == 
                               hometeam, json_data_frame$xG , 0)
  return(dataframe)
}

is_awayxg <- function(dataframe){
  dataframe$awayxg <- ifelse(awayteam== dataframe$team.id, json_data_frame$xG, 0)
  return(dataframe)
}
json_data_frame <- is_homexg(json_data_frame)
json_data_frame <- is_awayxg(json_data_frame)

hometeamname =JsonData$gameData$teams$home$abbreviation
awayteamname = JsonData$gameData$teams$away$abbreviation
hometeamshots= JsonData$liveData$boxscore$teams$home$teamStats$teamSkaterStats$shots
awayteamshots= JsonData$liveData$boxscore$teams$away$teamStats$teamSkaterStats$shots
meanhomexg=median(json_data_frame$homexg[json_data_frame$homexg!=0])
meanawayxg=median(json_data_frame$awayxg[json_data_frame$awayxg!=0])
xghomesum = sum(json_data_frame$homexg)
xgawaysum = sum(json_data_frame$awayxg)
xgkahome=xghomesum/hometeamshots
xgkaaway=xgawaysum/awayteamshots
homegoals=JsonData$liveData$boxscore$teams$home$teamStats$teamSkaterStats$goals
awaygoals=JsonData$liveData$boxscore$teams$away$teamStats$teamSkaterStats$goals
json_data_frame$about.periodTimeRemaining <- lapply(json_data_frame$about.periodTimeRemaining, gsub, pattern = ":", replacement = ".", fixed = TRUE)
json_data_frame$about.periodTimeRemaining <- as.numeric(as.character(json_data_frame$about.periodTimeRemaining),na.rm=TRUE)




is_round1 <- function(dataframe){
  dataframe$round1 <- ifelse(dataframe$about.period == 
                               1, dataframe$about.periodTime , 0)
  return(dataframe)
}

is_round2 <- function(dataframe){
  dataframe$round2 <- ifelse(dataframe$about.period == 
                               2, dataframe$about.periodTime , 0)
  return(dataframe)
}

is_round3 <- function(dataframe){
  dataframe$round3 <- ifelse(dataframe$about.period == 
                               3, dataframe$about.periodTime , 0)
  return(dataframe)
}




json_data_frame <- is_round1(json_data_frame)
json_data_frame <- is_round2(json_data_frame)
json_data_frame <- is_round3(json_data_frame)

round1<-json_data_frame$round1
round2<-json_data_frame$round2+20
round3<-json_data_frame$round3+40






erä=list(round1, round2,round3)
eraa=unlist(erä)
eraa<-data.frame(eraa)

colnames(eraa)[1] <- "tere"

xline=subset(eraa, tere!=40.00&tere!=0.00&tere!=20.00)

xline<-data.frame(xline)

deitti<-strsplit(datetime, "T")

deitti<-data.frame(deitti)
colnames(deitti)[1] <- "tere"


homeTeam = hometeamname
awayTeam = awayteamname
outputHomeTeam = c()
outputAwayTeam = c()
numberOfTimesToRunSim = 1
randomNumber = NULL
countGoalsScoredByHomeTeam = 0
countGoalsScoredByAwayTeam = 0
matchOutcome=NULL
outputHomeTeam=NULL
outputAwayTeam=NULL
percGamesDrawn=NULL
percGamesWonByAwayTeam=NULL
percGamesWonByHomeTeam=NULL

#ff=1000

for(f in 1:5000){
  
  
  for (x in 1:numberOfTimesToRunSim) {
    for (i in 1:length(json_data_frame$team.triCode)) {
      if(json_data_frame$team.triCode[i]==homeTeam){
        randomNumber=sample(1:100, 1)
        if (randomNumber<json_data_frame$xG[i]*100){
          countGoalsScoredByHomeTeam =countGoalsScoredByHomeTeam +1
        }
      }else if (json_data_frame$team.triCode[i]==awayTeam){
        randomNumber = sample(1:100, 1)
        if(randomNumber<json_data_frame$xG[i]*100){
          countGoalsScoredByAwayTeam=countGoalsScoredByAwayTeam+1
        }
      }
    }
  } # end of numberOfTimesToRunSimLoop
  # save results
  outputHomeTeam=c(outputHomeTeam, countGoalsScoredByHomeTeam)
  outputAwayTeam=c(outputAwayTeam, countGoalsScoredByAwayTeam)
  
  countGoalsScoredByHomeTeam=0
  countGoalsScoredByAwayTeam=0
  randomNumber=NULL
}
#for (s in 1:length(outputHomeTeam)){
#  cat("Home score is ", outputHomeTeam[s], " and away team score is ", outputAwayTeam[s], "\n")
#}
cat("\014")
# check to see if the game was won i.e. if more goals scored by either team
homeTeamGamesWon = 0
awayTeamGamesWon = 0

for (y in 1:length(outputHomeTeam)){
  
  if(outputHomeTeam[y]>outputAwayTeam[y]){
    
    homeTeamGamesWon = homeTeamGamesWon+1
    
  }else if (outputHomeTeam[y]<outputAwayTeam[y]){
    
    awayTeamGamesWon = awayTeamGamesWon+1
  }
  
}
draw="draw"
percGamesWonByHomeTeam = (homeTeamGamesWon*1.0/5000)*100
percGamesWonByAwayTeam = (awayTeamGamesWon*1.0/5000)*100
percGamesDrawn = 100-(percGamesWonByHomeTeam+percGamesWonByAwayTeam)

# objects to use in our barplot
matchTeams = c(homeTeam, draw, awayTeam)
matchOutcome = c(percGamesWonByHomeTeam,percGamesDrawn, percGamesWonByAwayTeam)



json_data_frame$xlinj<-xline$tere
unique(json_data_frame$xlinj)
p1<-ggplot(json_data_frame, aes(xline$tere, cumsum(homexg),colour=hometeamname))+
  ylab("Expected goals") + xlab("Aika(min)")+theme(axis.title.y = element_text(size=8))+theme(axis.title.x = element_text(size=8))+
  geom_step() +
  geom_step(aes(xline$tere, cumsum(awayxg),colour=awayteamname)) + theme(legend.title = element_blank()) +
  ggtitle( paste(hometeamlong, "-" ,awayteamlong,deitti$tere[1],'\n',"Tulos:",homegoals,"-", awaygoals,'\n',"Laskettu xG:","Koti",round(xghomesum,digits = 2),"-","Vieras",round(xgawaysum,digits = 2)))+theme(plot.title=element_text(size=10)) +theme(legend.title = element_blank()) +
   annotate("text", x = max(xline$tere)-15, y = 1, label = paste0("simuloitu 1x2\n",paste(hometeamname,"=",percGamesWonByHomeTeam,"% "),paste(draw,"=",percGamesDrawn,"% "),paste(awayteamname,"=",percGamesWonByAwayTeam,"%")),size=2)
#labs(title=metatiijot$otsikko)+labs(title=tulos)






#Create data : we take a subset of the mtcars dataset provided by R:

tiedotkoti=c(round(xghomesum,digits =2),round(meanhomexg,digits=3),round(xgkahome,digits=3),round(hometeamshots,digits=0),homegoals)
tiedotvieras=c(round(xgawaysum,digits =2),round(meanawayxg,digits=3),round(xgkaaway,digits=3),awayteamshots,awaygoals)

mydata=data.frame(Koti=tiedotkoti,Vieras=tiedotvieras)
mytable <- cbind(Tiedot=c("xG","xG-median","Shot Quality","Laukaukset","TULOS"),mydata[1:5,])
# --- Graph 1 : If you want ONLY the table in your image :
# First I create an empty graph with absolutely nothing :
p2<-qplot(1:5,1:5, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  # Then I add my table :
  annotation_custom(grob = tableGrob(mytable))





#with(plottaus,plot(homex, homey,xlab='',ylab ='', xlim=c(0, 60), ylim=c(28, 0), axes=FALSE, bg="steelblue2"))
#  rasterImage(test,0, 28, 60, 0) 
#g<-rasterGrob(test, interpolate=TRUE)

#Add your plot
# minCex <- 1.5
#points(plottaus$awayx, plottaus$awayy, col='blue',cex = pmax(minCex, plottaus$awayxg *11),pch=16, lwd=1)
#points(plottaus$homex, plottaus$homey, col='red',cex = pmax(minCex, plottaus$homexg *11),pch=16, lws



p3<-ggplot(json_data_frame, aes(homex, homey,color="red",alpha=0.5)) +annotation_custom(rasterGrob(test,width=unit(1,"npc"),height=unit(1,"npc")), -105, 105, -42.5,42.5)+geom_point(aes(json_data_frame$awayx,json_data_frame$away1,size = json_data_frame$xG),colour="blue",alpha=0.5)+xlim(-105,105)+ylim(-42.5,42.5)+geom_point(aes(size = json_data_frame$xG))+ theme(    
  axis.line = element_blank(), axis.ticks = element_blank(),
  axis.text.x = element_blank(), axis.text.y = element_blank(), 
  axis.title.x = element_blank(), axis.title.y = element_blank(),
 legend.position = "none", 
 # legend.title = "Joukkueet",
  panel.background = element_blank(), 
  panel.border = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  plot.title = element_text("Laukauksen laatu pisteen koon mukaan"))+scale_color_identity()
#multiplot(p1,p2,p3)
pvm<-deitti$tere[(1)]
tulokset=cbind("Koti"=hometeamname,"Vieras"=awayteamname,"xG koti"=xghomesum,"xG Vieras"=xgawaysum,"Median Koti"=meanhomexg,"Median vieras"=meanawayxg,"Shot quality koti"=xgkahome,"Shot quality vieras"=xgkaaway,"Kotimaalit"=homegoals,"Vierasmaalit"=awaygoals,"Kotilaukaukset"=hometeamshots,"Vieraslaukaukset"=awayteamshots,"PVM"=deitti$tere[1])

tulokset<-as.data.frame(tulokset)
write.table(tulokset, file="tulokset.csv",append = TRUE,quote = FALSE,dec=",",col.names = FALSE,sep="+")
library(cowplot)
plot_grid(p1,plot_grid(p2,p3),nrow=2)

#ggplot(json_data_frame, aes(homex, homey))+ geom_point()+geom_point(aes(json_data_frame$awayx,awayy))+   
#stat_density2d(aes(x=homex, y=homey, z=json_data_frame$xG,fill=..density..), geom="tile", 
                    #                                                                 contour = FALSE) +
 # scale_fill_gradientn(colours=rainbow(100)) +stat_density2d(aes(x=awayx, y=awayy, z=json_data_frame$xG,fill=..density..), geom="tile", 
                      #                                       contour = FALSE) +
  #scale_fill_gradientn(colours=rainbow(100))+geom_point()
