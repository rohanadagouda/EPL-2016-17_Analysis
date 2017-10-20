library(ggplot2)
library(plyr)
library(dplyr)
library(ggrepel)
library(Hmisc)
library(lubridate)
library(stringr)
library(png)
library(ggthemes)
library(data.table)
library(grid)

Strata <- read.csv("Chances.csv",header = TRUE)
Strata_PNG <- readPNG("StrataBet Logo.png")
rast <- rasterGrob(Strata_PNG)

str(Strata)

#Converting the values from Factor to Numeric
Strata$location_x <- as.numeric(as.character(Strata$location_x))
Strata$location_y <- as.numeric(as.character(Strata$location_y))
Strata$primaryLocation_x <- as.numeric(as.character(Strata$primaryLocation_x))
Strata$primaryLocation_y <- as.numeric(as.character(Strata$primaryLocation_y))
Strata$primaryPlayer <- as.character(Strata$primaryPlayer)
Strata$player <- as.character(Strata$player)

str(Strata)

#Converting Characters to Lower Case 
Strata$type <- tolower(as.character(Strata$type))

listofColors <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
                  "#8A7C64", "#599861")



#============Chances created during open play and goals scored===============
Chance <- Strata %>% filter(type %in% c("open play"),chanceRating!="-")%>% select(team,type,chanceRating)
Chance <- as.data.frame(table(Chance))
Chance <- Chance %>% select(team,Freq) %>% group_by(team) %>% summarise(N =sum(Freq))
Goal <- Strata  %>% filter(icon %in% c("goal") & type %in% ("open play"))%>% select(team,icon,type)
Goal <- as.data.frame(table(Goal))
Goal <- Goal %>% select(team,Freq) %>% group_by(team) %>% summarise(N =sum(Freq))

#Ggplot for the Chances created
ChancesPerTeam_OpenPlay <- ggplot() + 
  geom_bar(data=Chance,aes(x =team,y=N),fill="black",stat = "identity")+
  geom_text(data=Chance,aes(x =team,y=N, label =N), size=4, vjust=-1)+
  geom_line(data=Goal,aes(x=team,y=N,group = 1,color=N))+
  geom_text(data=Goal,aes(x =team,y=N, label = N), color="white",size=3, vjust=-1)+
  annotation_custom(rast,ymin = 1,xmin = 3,xmax = 10)+
  scale_y_continuous(breaks = seq(0,650,50))+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(colour="black",angle = 35, hjust = 1),
        axis.text.y=element_text(colour="black"))+ylab("Chances Created")+labs(color="Goals") +ggtitle(label = "Chances Created During Open-Play")

#Saving the GGPLOT into a PNG
ggsave(filename = "ChancesPerTeam_OpenPlay.png",device = "png",width = 10,height = 8,units = "in",
       limitsize = FALSE)





#=======================Kevin De Bruyne Assists=============

#Assists by KDB
Assists <-  Strata %>% 
  select(type,primaryPlayer,primaryType,primaryLocation_x,primaryLocation_y,player,location_x,location_y,icon)
Assister <- "K. De Bruyne"
Player <- subset(Assists,primaryPlayer== Assister & player != Assister & icon %in% c("goal"))
for (i in 1:nrow(Player)) {
  Pass_Map <- ggplot()+
    coord_cartesian(xlim=c(-136,136), ylim=c(-15,420)) +
    geom_point(data = Player, aes(x=primaryLocation_x,y=primaryLocation_y,shape=4), color = "black", size = 2.5,alpha = 1)+
    scale_shape_identity() + #Shape for the point
    geom_segment(data = Player,aes(x=primaryLocation_x,y=primaryLocation_y,xend=location_x,yend=location_y,color=primaryType)
                 ,na.rm = TRUE,arrow=arrow(length=unit(0.16,"cm"),angle = 30 ,type = "closed" )) 
}
KDB_Assists <- Pass_Map+ annotation_custom(rast,ymin = 380,ymax = 420,xmin = 10,xmax = 136)+
  geom_segment(aes(x = -136, xend = 136, y = 210, yend = 210),color="white",size=0.8)+
  geom_segment(aes(x = -37, xend = 37, y = 22, yend = 22),color="white",size=0.6) +
  geom_segment(aes(x = -81, xend = 81, y = 66, yend = 66),color="white",size=0.6)+
  geom_segment(aes(x = 37, xend = 37, y = 0, yend = 22),color="white",size=0.6)+
  geom_segment(aes(x = -37, xend = -37, y = 0, yend = 22),color="white",size=0.6)+
  geom_segment(aes(x = 81, xend = 81, y = 0, yend = 66),color="white",size=0.6)+
  geom_segment(aes(x = -81, xend = -81, y = 0, yend = 66),color="white",size=0.6)+
  geom_segment(aes(x = -15, xend = -15, y = 0, yend = -15),color="white",size=0.6)+
  geom_segment(aes(x = 15, xend = 15, y = 0, yend = -15),color="white",size=0.6)+
  geom_segment(aes(x = -15, xend = 15, y = -15, yend = -15),color="white",size=0.6)+
  geom_segment(aes(x = -136, xend = -136, y = 0, yend = 420),color="white",size=0.6)+
  geom_segment(aes(x = 136, xend = 136, y = 0, yend = 420),color="white",size=0.6)+
  geom_segment(aes(x = -136, xend = 136, y = 0, yend = 0),color="white",size=0.6)+
  geom_segment(aes(x = -136, xend = 136, y = 420, yend = 420),color="white",size=0.6)+
  annotate("text", x = 0, y = -6, label = "Goal",size=3)+
  ggtitle(label = Assister,subtitle = "Assists")+
  theme(rect = element_blank(),line = element_blank(),panel.background = element_rect(fill = 'lightgreen'),
        axis.title.y =element_blank(),axis.title.x = element_blank(),axis.text.y = element_blank(),
        axis.text.x = element_blank()) + labs(colour="Type of Assist")+
  scale_color_brewer(palette="Dark2")

#Saving the GGPLOT into a PNG
ggsave(filename = "KDB_Assists.png",device = "png",width = 10,height = 8,units = "in",
       limitsize = FALSE)



#=========================Chances Created by Top Six=============
library(treemapify)
library(magick)
#Filter to get Top six Teams along with players and Chance Rating
Chances_Top_Six <- Strata%>% 
  filter(chanceRating %nin% c("-","Penalty","poorchance"),primaryPlayer %nin% c("-"),
         team %in% c("Arsenal","Chelsea","Liverpool","Manchester City","Manchester United","Tottenham Hotspur") )%>% 
  select(team,primaryPlayer) %>% 
  group_by(primaryPlayer,team) %>% 
  summarise(N=n()) 

#Top Six Teams in EPL
Top_Six <- c("Arsenal","Chelsea","Liverpool","Manchester City","Manchester United","Tottenham Hotspur")
TColors <- rep(c("red","darkblue","black"),3)

# Make plots
plot_list = list()
for (i in 1:NROW(Top_Six)) {
Chances_10 <- Chances_Top_Six %>% filter(N >= 10)   
Chances_10 <- Chances_10 %>% filter(team %in% Top_Six[i])
p <- ggplot(Chances_10, aes(area = N ,fill=N,label = primaryPlayer)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    reflow = TRUE)+
  scale_fill_gradient(low = TColors[i],high = TColors[i+1])+
  labs( title = Top_Six[i], fill = "Chances Created" )
plot_list[[i]] = p
}

#create png where each page is a separate plot
png("p%02d.png")
for (i in 1:NROW(Top_Six)) {
  print(plot_list[[i]])
}
dev.off()

png.files <- sprintf("p%02d.png", 1:NROW(Top_Six))
#Need to create  magick image object for animation and gif.Since using Magick Package
animation <- image_read(png.files)
animation <- image_animate(animation,fps = 1)
#print(animation)
image_write(animation, "Chances_by_Top_Six.gif")


#=============================Own Goal Gained by Teams================================

#Own Goal
OG <- Strata%>%
  filter(icon=='owngoal')%>%
  select(team,time,player)

#Calculatin the Total Own goals
OG <- ddply(OG,.(team),summarise,Own.Goals.Gained=length(player))

Own_Goals <- ggplot(OG, aes(x=Own.Goals.Gained,y=team, label=Own.Goals.Gained)) + 
  geom_count(show.legend=F,size=5)+
  geom_segment(aes(y=team,yend=team, x=1,xend=Own.Goals.Gained))+
  geom_text(color="white", size=2.5)+
  annotation_custom(rast,xmin = 3,xmax = 5,ymin = 3)+
  labs(title="Own Goals Gained Plot")+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(color="blue"))

#Saving the GGPLOT into a PNG
ggsave(filename = "Own_Goals.png",device = "png",width = 10,height = 8,units = "in",
       limitsize = FALSE)
