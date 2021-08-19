## REQUIRED PACKAGES
require(grid)
library(ggplot2)
require(plyr)

############################################
### CUSTOMIZE APPEARANCE WITH THESE     ####
############################################
blankRows<-2    # blank rows under boxplot
titleSize<-4
dataSize<-4
############################################
############################################

## BASIC THEMES (SO TO PLOT BLANK GRID)
theme_grid <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  axis.ticks.length = unit(0.0001, "mm"),
  legend.position = "none", 
  panel.background = element_rect(fill = "transparent"), 
  panel.border = element_blank(), 
  panel.grid.major = element_line(colour="grey"), 
  panel.grid.minor = element_line(colour="grey"), 
  panel.spacing = unit(c(-0.1,-0.1,-0.1,-0.1), "mm"), 
  plot.margin = unit(c(5,0,5,0.01), "mm")
)

theme_bare <- theme_grid +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

## LOAD GROUP DATA AND P values from csv file
groupData<-read.csv(file="/shared_data/google-drive/Projekte/visR_tabletwo/data/groupdata.csv",header=T)

## SYNTHESIZE SOME PLOT DATA - you can load csv instead
## EXPECTS 2 columns - integer for 'ID' matching groupdatacsv
## AND 'HR' Hazard Rate
hazardData<-expand.grid(ID=1:nrow(groupData),HR=1:6)
hazardData$HR<-1.3-runif(nrow(hazardData))*0.9
hazardData<-rbind(hazardData,ddply(groupData,.(Group),summarize,ID=max(ID)+0.1,HR=NA)[,2:3])
hazardData<-rbind(hazardData,data.frame(ID=c(0,-1:(-2-blankRows),max(groupData$ID)+1,max(groupData$ID)+2),HR=NA))

## Make the min/max mean labels
hrlabels<-ddply(hazardData[!is.na(hazardData$HR),],.(ID),summarize,lab=paste(round(mean(HR),2)," (",round(min(HR),2),"-",round(max(HR),2),")",sep=""))

## Points to plot on the log scale
scaledata<-data.frame(ID=0,HR=c(0.2,0.6,0.8,1.2,1.8))

## Pull out the Groups & P values
group_p<-ddply(groupData,.(Group),summarize,P=mean(P_G),y=max(ID)+0.1)

## identify the rows to be highlighted, and 
## build a function to add the layers
hl_rows<-data.frame(ID=(1:floor(length(unique(hazardData$ID[which(hazardData$ID>0)]))/2))*2,col="lightgrey")
hl_rows$ID<-hl_rows$ID+blankRows+1
hl_rect<-function(col="white",alpha=0.5){
  rectGrob(   x = 0, y = 0, width = 1, height = 1, just = c("left","bottom"), gp=gpar(alpha=alpha, fill=col))
}

## DATA FOR TEXT LABELS
RtLabels<-data.frame(x=c(rep(length(unique(hazardData$ID))-0.2,times=3)),
                     y=c(0.6,6,10),
                     lab=c("Hazard Ratio\n(95% CI)","P Value","P Value for\nInteraction"))

LfLabels<-data.frame(x=c(rep(length(unique(hazardData$ID))-0.2,times=2)),
                     y=c(0.5,4),
                     lab=c("Subgroup","No. of\nPatients"))

LegendLabels<-data.frame(x=c(rep(1,times=2)),
                         y=c(0.5,1.8),
                         lab=c("Off-Pump CABG Better","On-Pump CABG Better"))

## BASIC PLOT
haz<-ggplot(hazardData,aes(factor(ID),HR))+ labs(x=NULL, y=NULL)

## RIGHT PANEL WITH LOG SCALE
rightPanel<-haz + 
  apply(hl_rows,1,function(x)annotation_custom(hl_rect(x["col"],alpha=0.4),as.numeric(x["ID"])-0.5,as.numeric(x["ID"])+0.5,-20,20)) +
  geom_hline(aes(yintercept=1),linetype=2, size=0.5)+
  stat_summary(fun=median, geom="point", fill = "black",shape=22,size=3) +
  stat_boxplot(geom ='errorbar') + 
  scale_y_log10() + coord_flip() +
  geom_text(data=scaledata,aes(3,HR,label=HR), vjust=0.5, size=dataSize) +
  geom_text(data=RtLabels,aes(x,y,label=lab, fontface="bold"), vjust=0.5, size=titleSize) +
  geom_text(data=hrlabels,aes(factor(ID),4,label=lab),vjust=0.5, hjust=1, size=dataSize) +
  geom_text(data=group_p,aes(factor(y),11,label=P, fontface="bold"),vjust=0.5, hjust=1, size=dataSize) +
  geom_text(data=groupData,aes(factor(ID),6.5,label=P_S),vjust=0.5, hjust=1, size=dataSize) +
  geom_text(data=LegendLabels,aes(x,y,label=lab, fontface="bold"),hjust=0.5, vjust=1, size=titleSize) +
  geom_point(data=scaledata,aes(2.5,HR),shape=3,size=3) + 
  geom_point(aes(2,12),shape=3,alpha=0) + 
  geom_segment(aes(x = 2.5, y = 0, xend = 2.5, yend = 13)) + 
  theme_bare

## LEFT PANEL WITH NORMAL SCALE
leftPanel<-haz + 
  apply(hl_rows,1,function(x)annotation_custom(hl_rect(x["col"],alpha=0.4),as.numeric(x["ID"])-0.5,as.numeric(x["ID"])+0.5,-20,20)) +
  coord_flip(ylim=c(0,5.5)) +
  geom_point(aes(x=factor(ID),y=1),shape=3,alpha=0) + 
  geom_text(data=group_p,aes(factor(y),0.5,label=Group, fontface="bold"), hjust=0, size=dataSize) +
  geom_text(data=groupData,aes(factor(ID),1,label=Subgroup), hjust=0, size=dataSize) +
  geom_text(data=groupData,aes(factor(ID),5,label=NoP), hjust=1, size=dataSize) +
  geom_text(data=LfLabels,aes(x,y,label=lab, fontface="bold"), hjust=0, size=4, size=titleSize) +
  geom_segment(aes(x = 2.5, y = 0, xend = 2.5, yend = 5.5)) + 
  theme_bare

## PLOT THEM BOTH IN A GRID SO THEY MATCH UP
gridExtra::grid.arrange(leftPanel,rightPanel, widths=c(1,3), ncol=2, nrow=1)
