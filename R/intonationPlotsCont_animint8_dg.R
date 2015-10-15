library(reshape)
library(ggplot2)
library(plyr)
library(animint)
library(car)
library(RColorBrewer)

setwd("~/Dropbox/cont/interactive/animintContours")

convertVariables <- function(df){
  
  # columns that are usually read as numeric but should be factors:
  factorCols=c("experiment","item","condition","participant","playlist","word","woiLabel")
  
  # columns that are usually read as factors but should be numeric:
  numericCols=c("duration", 
                "silence", "duraSil", "phoneLength", "meanPitch", "maxPitch", 
                "maxPitTime", "minPitch", "minPitTime", "pitch1", "pitch1_time", 
                "pitch2", "pitch2_time", "pitch3", "pitch3_time", "pitch4", "pitch4_time", 
                "pitch5", "pitch5_time", "pitch6", "pitch6_time", "pitch7", "pitch7_time", 
                "pitch8", "pitch8_time", "pitch9", "pitch9_time", "pitch10", 
                "pitch10_time", "meanIntensity", "maxIntensity", "maxIntTime", 
                "intensity1", "intensity1_time", "intensity2", "intensity2_time", 
                "intensity3", "intensity3_time", "intensity4", "intensity4_time", 
                "intensity5", "intensity5_time", "intensity6", "intensity6_time", 
                "intensity7", "intensity7_time", "intensity8", "intensity8_time", 
                "intensity9", "intensity9_time", "intensity10", "intensity10_time", 
                "zstart", "zend", "zDuration", "zPhonelength", "zmeanPitch", 
                "zmaxPitch", "zmaxPitTime", "zminPitch", "zminPitTime", "zmeanIntensity", 
                "zmaxIntensity", "zmaxIntTime","response","duration", "silence", "durasil", 
"meanpitch", "maxpitch", "maxPitTime", "minPitch", "minPitTime", 
"firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", 
"maxIntensity", "zduration", "zbeginzone", "zendzone", "zphonelength", 
"zmeanpitch", "zmaxpitch", "zmaxPitTime", "zminPitch", "zminPitTime", 
"zfirstpitch", "zsecondpitch", "zthirdpitch", "zfourthpitch", 
"zmeanIntensity", "zmaxIntensity", "durasil", "meanpit", "maxpitch", "maxPitTime", 
"minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", 
"fourthpitch", "meanIntensity", "maxIntensity", "firstF1", "firstF2", 
"firstdif", "secondF1", "secondF2", "seconddif", "thirdF1", "thirdF2", 
"thirddif", "fourthF1", "fourthF2", "fourthdif", "fifthF1", "fifthF2", 
"fifthdif")
  
  nColumns=ncol(df)
  
  for (i in 1:nColumns) {
    if (colnames(df)[i] %in% factorCols) {
      df[,i]<- as.factor(as.character(df[,i]))
    } 
    if (colnames(df)[i]  %in% numericCols) {
      df[,i]<- as.numeric(as.character(df[,i]))
    } 
  }
  return(df)
}








#
# Dot plot of contour annotation
#

d= read.csv("contour_responses.txt",sep = "\t")

d=convertVariables(d)

# anno = choice ("categories",8)
# option ("fall")
# option ("fall with upstep")
# option ("risefallrise")
# option ("contradiction")
# option ("inredulity")
# option ("yesnoRise")
# option("continuation")
# option ("otherContour")
# option ("unclear")
# option ("problematic")
# anno2=choice("prominence",3)
# option ("subject")
# option ("verb")
# option ("object")
# option ("unclear")
# clicked = endPause("Continue",1)

d$Context[d$condition=="3"]="Intended Incomplete Response"
d$Context[d$condition=="2"]="Intended Incredulity"
d$Context[d$condition=="1"]="Intended Contradiction"
d$Context=factor(d$Context)

d$Contour=NA
d$Contour[d$Michael_tune=="1"]="Fall"
d$Contour[d$Michael_tune=="2"]="Contrastive Fall"
d$Contour[d$Michael_tune=="3"]="RFR"
d$Contour[d$Michael_tune=="4"]="Contradiction Contour"
d$Contour[d$Michael_tune=="5"]="Incredulity Contour"
d$Contour[d$Michael_tune=="6"]="Yes/No Rise"
d$Contour[d$Michael_tune=="7"]="Continuation Rise"
d$Contour[d$Michael_tune=="8"]="Other Contour"
d$Contour[d$Michael_tune=="9"]="Unclear"

# Falling Contradictory Tune?
otherc=c("contour_15_9_2.wav","contour_930_5_1.wav","contour_1072_9_3.wav","contour_1094_4_1.wav","contour_1094_9_3.wav","contour_1322_1_1.wav","contour_1322_3_1.wav","contour_1322_4_1.wav","contour_1322_7_1.wav","contour_1323_5_1.wav","contour_1323_6_3.wav","contour_1451_1_1.wav","contour_1451_3_1.wav","contour_1451_4_1.wav","contour_1451_6_1.wav","contour_1600_8_1.wav","contour_1619_9_1.wav","contour_1620_1_1.wav","contour_1645_2_1.wav","contour_1645_7_1.wav","contour_1647_3_1.wav","contour_1647_6_1.wav","contour_1647_7_1.wav","contour_1648_6_1.wav","contour_1649_2_1.wav","contour_1649_4_1.wav","contour_1649_6_1.wav","contour_1649_7_1.wav","contour_1649_8_1.wav","contour_1654_2_1.wav","contour_1654_3_1.wav","contour_1656_1_1.wav","contour_1656_7_1.wav","contour_1658_3_1.wav","contour_1658_5_1.wav","contour_1663_5_1.wav","contour_1677_2_1.wav","contour_1677_3_3.wav","contour_1677_4_1.wav","contour_1684_7_1.wav")

d$Contour[d$recordedFile %in% otherc]="Falling Contradiction Contour?"

# incredulity?
otherc=c("contour_1094_7_3.wav","contour_1646_1_1.wav","contour_1646_4_1.wav","contour_1646_7_1.wav","contour_1646_7_2.wav","contour_1646_9_2.wav")

d$Contour[d$recordedFile %in% otherc]="Incredulity"

# Undershot Yes/No Rise?
otherc=c("contour_15_3_2.wav","contour_15_5_2.wav","contour_1322_3_2.wav","contour_1322_9_2.wav","contour_1471_3_2.wav","contour_1619_2_2.wav","contour_1619_3_2.wav","contour_1619_4_2.wav","contour_1619_5_2.wav","contour_1620_4_2.wav","contour_1648_5_2.wav","contour_1649_4_2.wav","contour_1649_8_2.wav","contour_1654_3_2.wav","contour_1654_5_2.wav","contour_1654_8_2.wav","contour_1654_9_2.wav","contour_1656_6_2.wav","contour_1657_1_2.wav","contour_1657_5_2.wav","contour_1658_1_2.wav","contour_1658_4_2.wav","contour_1658_5_2.wav","contour_1659_5_2.wav","contour_1659_6_2.wav","contour_1659_7_1.wav","contour_1659_7_2.wav","contour_1663_3_2.wav")

d$Contour[d$recordedFile %in% otherc]="Undershot Yes/NO Rise?"


# prominence: Essentially, there were only prominence shifts when the contour was a contrastive fall
# and there was never a prominence shift to the subject:

table(d$Contour,d$Michael_prominence)

d$Contour[d$Contour=="Contrastive Fall"]="Contrastive Fall (Object)"
d$Contour[d$Contour=="Contrastive Fall (Object)"&d$Michael_prominence=="2"]="Contrastive Fall (Verb)"

d=d[!is.na(d$Contour),]


# recode to only include categories in poster
# levels=c("Fall","Contrastive Fall (Object)","Contrastive Fall (Verb)","RFR","Contradiction Contour","Falling Contradiction Contour?","Incredulity Contour","Yes/No Rise","Undershot Yes/NO Rise?","Continuation Rise","Other Contour","Unclear"))


# narrow down to most important contours for now
d$Contour=recode(as.character(d$Contour),"'Contrastive Fall (Object)'='Other';'Contrastive Fall (Verb)'='Verum Focus';'Falling Contradiction Contour?'='Falling Contradiction';'Undershot Yes/NO Rise?'='Yes/No Rise';'Continuation Rise'='Other';'Other Contour'='Other';'Unclear'='Other'")
d$Contour=factor(d$Contour)
#c("Contradiction Contour", "Fall", "Falling Contradiction", "Incredulity Contour","Other", "RFR", "Verum Focus", "Yes/No Rise")
d$Contour=factor(d$Contour,levels=c("Fall", "Contradiction Contour","Falling Contradiction","Verum Focus","RFR","Yes/No Rise","Incredulity Contour","Other"))



d=ddply(d,.(Context),transform,ContextCount=length(Context))

ptabl= ddply(d,.(Context,Contour),summarise,Count=length(ContextCount),Percentage=round((length(Context)/mean(ContextCount)*100),1))

#
ptabl$Contour=factor(ptabl$Contour,levels=c("Fall","Contradiction Contour","Falling Contradiction","Verum Focus","RFR","Yes/No Rise","Incredulity Contour","Other"))
#

ptabl=ptabl[order(ptabl$Contour),]
ptabl$Contour = factor(ptabl$Contour,levels(ptabl$Contour)[c(8:1)])

# Figure 1: Michael's annotation

#flipped
ggplot(subset(ptabl,!is.na(ptabl$Contour)), aes(x=Contour, y=Percentage)) + theme_bw(base_size=18) + scale_y_continuous(breaks=seq(0, 100, 10),limits = c(0,100)) +  geom_point(stat="identity",size=4) + coord_flip() + facet_grid (. ~ Context) + xlab("") + ylab("")+ geom_text(aes(label=paste(Percentage,"% ","(",Count,")",sep="")), size=4,  position=position_dodge(width=0.9), hjust=-0.5)



#
#
# Plot average time-normalized curves 
# 
#


# read in acoustic measure and merge with annotations and other experimental information


acoustics <- read.csv("cont.txt",sep = "\t")
acoustics=convertVariables(acoustics)

acoustics = merge(acoustics,d, all.x=TRUE,by=c("item","condition","participant"), suffixes=c("",".other"))

#dput(names(acoustics))
acoustics=subset(acoustics,!is.na(recordedFile))
acoustics=subset(acoustics,!is.na(Contour))
#
# Turn a bunch of columns into rows, with the columnnames as level names
# Basic idea: use all columns you don't want to touch as id columns
# in the example below, the complicating factor is that there are two sets
# of columns that are supposed to end up in different variables.
# The solution might not be the most elegant one...

smelt=melt(acoustics,id=c("item", "condition", "participant", "experiment", "fileName", 
"word", "wordLabel", "woiLabel", "wordOnset", "wordOffset", "duration", 
"silence", "duraSil", "phoneLength", "meanPitch", "maxPitch", 
"maxPitTime", "minPitch", "minPitTime", "meanIntensity", "maxIntensity", "maxIntTime", 
"intensity1", "intensity1_time", "intensity2", "intensity2_time", 
"intensity3", "intensity3_time", "intensity4", "intensity4_time", 
"intensity5", "intensity5_time", "intensity6", "intensity6_time", 
"intensity7", "intensity7_time", "intensity8", "intensity8_time", 
"intensity9", "intensity9_time", "intensity10", "intensity10_time", 
"zstart", "zend", "zDuration", "zPhonelength", "zmeanPitch", 
"zmaxPitch", "zmaxPitTime", "zminPitch", "zminPitTime", "zmeanIntensity", 
"zmaxIntensity", "zmaxIntTime", "zLabel", "experiment.other", 
"Contour", "context", "setup", "text", "woi", "contextFile", 
"playlist", "order", "trialN", "session", "recordedFile", "trialDuration", 
"date", "trialStart", "trialEnd", "X.", "Test", "Lauren", "Lauren.1", 
"Michael_tune", "Michael_prominence", "Context","ContextCount"))

# rearrange data
subs=subset(smelt, grepl("_time",variable))
smelt=subset(smelt, !grepl("_time",variable))
smelt$time=subs$value
names(smelt)[names(smelt) == 'value'] <- 'pitch'
names(smelt)[names(smelt) == 'variable'] <- 'slice'

# smooth measures for each utterance (is this desirable?)

fils=unique(smelt$recordedFile)
smelt=smelt[with(smelt, order(time)), ]
for (i in 1:length(fils)) {
	subs=subset(smelt,recordedFile==fils[i])
	if (!is.na(mean(subs$pitch,na.rm=T))) {
		smoothed=loess(pitch ~ time, data=subs)	
		smoothedPitch=predict(smoothed, subs$time, se = TRUE)$fit
		smelt$smoothedPitch[smelt$recordedFile==fils[i]]=smoothedPitch
	}
}	

#
# Calculate average time points and locations for word labels

smelt$sliceNumber=as.numeric(gsub("pitch","",smelt$slice))

smelt$sliceTime=smelt$wordOnset+smelt$sliceNumber*(smelt$duration/10)

# averages for all
averageAll=ddply(smelt,.(item, Contour, word),transform,wordOnset.new = mean(wordOnset,na.rm=T),wordOffset.new=mean(wordOffset,na.rm=T),duration.new=mean(duration,na.rm=T))

averageAll$sliceTimeAv=averageAll$wordOnset.new+averageAll$sliceNumber*(averageAll$duration.new/10)

timeline=ddply(averageAll,.(item,Contour,word), summarise,wordOnset=mean(wordOnset.new,na.rm=T),wordOffset=mean(wordOffset.new,na.rm=T),word=paste(unique(wordLabel), collapse = ''),duration=mean(duration.new,na.rm=T))




#
#
#  Plots with current data from perception experiment
#

perception1=read.csv("contPer_responses.txt",sep = "\t")
attach(perception1)
perception1=data.frame(experiment,item,condition,originalItem,originalCondition,contextFile,speaker,playlistOld,orderOld,trialNOld,sessionOld,answerFile,response,Context)
detach(perception1)

perception2=read.csv("contPer2_responses.txt",sep = "\t")
attach(perception2)
experiment
perception2=data.frame(experiment,item,condition,originalItem,originalCondition,contextFile,speaker,playlistOld=originalplaylist,orderOld=originalorder,trialNOld=originaltrialN,sessionOld=originalsession,answerFile,response,Context=recode(OldContext,"'Intended Inredulity'='Intended Incredulity'"))
detach(perception2)


# perception3=read.csv("contPer3_responses.txt",sep = "\t")

perception=subset(rbind(perception1,perception2), response<9)

# add contour annotation
annotatedContour=data.frame(answerFile=d$recordedFile,Contour=d$Contour)
perception=merge(perception,annotatedContour, all.x=TRUE,by=c("answerFile"))

# recode Context
perception$Context=recode(perception$Context,"'Intended Contradiction'='Contradiction';'Intended Incomplete Response'='Incomplete Response';'Intended Incredulity'='Incredulity'")

# overall boxplot (but boxplot not supported by animint)
ggplot(perception, aes(x=Context, y=response))  + geom_boxplot() + theme_bw(base_size=16) + xlab('Perceptual ratings by naive participants') + ylab('Naturalness Rating') + facet_grid(Contour ~.)

#
# animint: current working example
#

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# plot with warped time, without segments and text:
# contTrack=ggplot()+ theme_bw(base_size = 22) + geom_line(aes(x=sliceTimeAv, y=smoothedPitch,group=recordedFile,colour=Contour,showSelected = Contour,clickSelects=recordedFile,tooltip=recordedFile),data=averageAll) + geom_smooth(aes(x=sliceTimeAv, y=smoothedPitch),colour="black",size=0.7,data=averageAll) + xlab("") + ylab("Pitch (Hz)") + facet_grid(.~Context) + geom_text(aes(x=0.5,y=-80,showSelected=recordedFile,label=recordedFile,href=paste0("http://prosodylab.org/~chael/www/sounds/bestiary/", recordedFile)),data=averageAll) + scale_colour_manual(values=rev(gg_color_hue(8))) + first=list(Contour="Contradiction Contour")
write.csv(averageAll,'contour_data.txt')
contTrack=ggplot()+ theme_bw(base_size = 22) + 
  geom_line(aes(x=sliceTimeAv, y=smoothedPitch,group=recordedFile,colour=Contour,showSelected = Contour,clickSelects=recordedFile,tooltip=recordedFile),data=averageAll) + geom_smooth(aes(x=sliceTimeAv, y=smoothedPitch),colour="black",size=0.7,data=averageAll) + xlab("") + ylab("Pitch (Hz)") + facet_grid(.~Context) + geom_text(aes(x=0.5,y=-80,showSelected=recordedFile,label=recordedFile,href=paste0("http://prosodylab.org/~chael/www/bestiary/soundfiles/", recordedFile)),data=averageAll) + scale_colour_manual(values=rev(gg_color_hue(8)))

props=subset(ptabl,!is.na(ptabl$Contour))
write.csv(props,'props_data.txt')
contProp=ggplot() + geom_point(aes(x=Contour, y=Percentage,clickSelects = Contour,colour=Contour), stat="identity",size=5, data=props) + coord_flip() + theme_bw(base_size=12) + scale_y_continuous(breaks=seq(0, 100, 20), limits = c(-50,140)) + facet_grid (. ~ Context) + xlab("") + ylab("") + geom_text(data=props, mapping=aes(x=Contour, y=Percentage, label=paste(Percentage,"% ","(",Count,")",sep="")),size=8,  position=position_dodge(width=0.9),hjust=1) + theme(legend.position="none") # + theme(panel.margin=grid::unit(0, "cm"))

# boxplots are not supported by animint!
#contPer=ggplot(perception, aes(x=Context, y=response))  + geom_boxplot() + theme_bw(base_size=16) + xlab('Perceptual ratings by naive participants') + ylab('Naturalness Rating')

write.csv(perception,'perception_data.txt')
contPer=ggplot(perception, aes(x=Context, y=response,showSelected = Contour))  + stat_summary(fun.y = "mean", geom="bar")  + stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) + theme_bw(base_size=22) + xlab('Perceptual ratings by naive participants') + ylab('Naturalness Rating') 

# put on animint webpage 
#contTrack <- contTrack + theme_animint(width = 970)
threePlots <- list(contTrack = contTrack, contProp = contProp, contPer=contPer, width=list(contTrack = 1000, contProp = 650, contPer=350),  height=list(contTrack=500,contProp=300,contPer=250)) 
animint2dir(threePlots, "threePlots")
servr::httd("~/Dropbox/cont/interactive/animintContours/threePlots")


# todo:

# add by item plot (choose item and then generate plot?)
# add choice of annotator 
   #shiny: https://cpsievert.shinyapps.io/animintShiny)
   # or markdown? : https://cpsievert.shinyapps.io/animintRmarkdown
# add 'select all' and 'deslect all' buttons (shiny?)
# add ratings for individual soundfiles base don clickSelect


# issues
# geom_smooth not implemented (but geom_line with smooth might work...)
# boxplots are not implemented, but there are workarounds...
# errors bars not implemented? maybe different way of plotting errors bars would work
# theme(legend.position="left") not implemented (but "none" works!)
# how can i adjust horizontal alignment of plot? (proportion plot is clipped)
# 


# link to online files:
# http://kferris10.github.io/AnimintBlog/Hour-by-Hour-Visualization-of-the-Ridge-Hike/




# # example of plot for one paritcular item with text and word boundaries:
# 
# #### with time normalized plot for one particular item:
# 
# subsItem=subset(averageAll,item==showItem)
# subsTimeline=subset(timeline,item==showItem)
# # plot average contours for all items in black, and individual utterances in grey
# contTrack=ggplot()+ theme_bw(base_size = 16) + 
#   geom_line(aes(x=sliceTimeAv, y=smoothedPitch,group=participant,showSelected = Contour,colour=Contour),data=subsItem) +  geom_smooth(aes(x=sliceTimeAv, y=pitch),colour="black",size=0.7,data=subsItem) + xlab("") + ylab("Pitch (Hz)") + 
#   geom_segment(aes(y=-20,yend=400,x=wordOffset,xend=wordOffset),data=subsTimeline,size=0.15) + 
#   geom_text(aes(label=word, x=wordOnset+(duration)/2, y=0),data=subsTimeline,size=6) + geom_segment(aes(y=-20,yend=400,x=wordOnset,xend=wordOnset),data=subsTimeline,size=0.15) + facet_grid(Contour~.)
# 
# props=subset(ptabl,!is.na(ptabl$Contour))
# contProp=ggplot() + geom_point(aes(x=Contour, y=Percentage,clickSelects = Contour), stat="identity",size=3, data=props) + coord_flip() + theme_bw(base_size=12) + scale_y_continuous(breaks=seq(0, 100, 20), limits = c(-50,140)) + facet_grid (. ~ Context) + xlab("") + ylab("") + geom_text(data=props, mapping=aes(x=Contour, y=Percentage, label=paste(Percentage,"%")),size=8,  position=position_dodge(width=0.9),hjust=1)
# 
# 
# contPer=ggplot(perception, aes(x=Context, y=response, clickSelects = Contour))  + geom_boxplot() + theme_bw(base_size=16) + xlab('Perceptual ratings by naive participants') + ylab('Naturalness Rating')
# 
# contTrack <- contTrack + theme_animint(width = 970)
# bothPlots <- list(contTrack = contTrack,contProp = contProp, width=list(contTrack = 900, contProp = 900),  height=list(contTrack=500,contProp=300)) 
# animint2dir(bothPlots, "bothPlots")
# servr::httd("/Users/chael/Dropbox/cont/1_initial analysis & data/3_intonationplots/bothPlots")
# 



