library(shiny)
library(animint)
library(reshape)
library(ggplot2)
library(plyr)
library(car)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  
  #
  # animint: current working example
  #
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  
  
  averageAll = read.csv('contour_data.txt')
  props = read.csv('props_data.txt')
  perception = read.csv('perception_data.txt')
  
  contTrack <- ggplot()+ theme_bw(base_size = 22) + 
      geom_line(aes(x=sliceTimeAv, y=smoothedPitch,group=recordedFile,colour=Contour,showSelected = Contour,clickSelects=recordedFile,tooltip=recordedFile),data=averageAll) + geom_smooth(aes(x=sliceTimeAv, y=smoothedPitch),colour="black",size=0.7,data=averageAll) + xlab("") + ylab("Pitch (Hz)") + facet_grid(.~Context) + geom_text(aes(x=0.5,y=-80,showSelected=recordedFile,label=recordedFile,href=paste0("http://prosodylab.org/~chael/www/bestiary/soundfiles/", recordedFile)),data=averageAll) + scale_colour_manual(values=rev(gg_color_hue(8)))
  
  contPer= ggplot(perception, aes(x=Context, y=response,showSelected = Contour))  + stat_summary(fun.y = "mean", geom="bar")  + stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size=0.6, width=.15) + theme_bw(base_size=22) + xlab('Perceptual ratings by naive participants') + ylab('Naturalness Rating')
  
    contProp= ggplot() + geom_point(aes(x=Contour, y=Percentage,clickSelects = Contour,colour=Contour), stat="identity",size=5, data=props) + coord_flip() + theme_bw(base_size=12) + scale_y_continuous(breaks=seq(0, 100, 20), limits = c(-50,140)) + facet_grid (. ~ Context) + xlab("") + ylab("") + geom_text(data=props, mapping=aes(x=Contour, y=Percentage, label=paste(Percentage,"% ","(",Count,")",sep="")),size=8,  position=position_dodge(width=0.9),hjust=1) + theme(legend.position="none")
  
  getPlotList <- reactive({list(contTrack = contTrack, contPer=contPer)})
  
  output$animint<-
    renderAnimint({
    getPlotList()
    })
  
  
  
})