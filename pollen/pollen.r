require(ggplot2)
require(reshape2)
#require(PairViz)
require(GGally)
library(scales) # for muted

#load data from csv-file
#df1<-read.csv2(file="data/pollen.txt",header=TRUE,sep=";",quote = "")
#df1 <- apply(df1, MARGIN=2, as.character)
#df1 <- apply(df1, MARGIN=2, as.numeric)
#df1 <- data.frame(df1$NUM, df1)
#df1[,7] <- NULL
#nam <- names(df1)
#nam[1] <- "No"
#names(df1) <- nam
#df1 <- data.frame(df1)
#save(df1,file="data.Rda")

source(file = "pollenFunc.r")



# Definitions
# ------------------------------------------------------------------------------------------------------------
useDataScaling <- T
xLab<-1.5
yLab<- 1.5
xText <- 1.5
yText <- 1.5

colOrdering <- c(1:5)
useWhiteBackground <- TRUE




# Explore displayed line in the middle of the data
useSubset <- TRUE
nubMin <- 0.45
nubMax <- 0.53
crackMin <- 0.49
crackMax <- 0.52
weightMin <- 0.45
weightMax <- 0.55
densityMin <- 0.4
densityMax <- 0.614557
ridgeMin <- 0.465
ridgeMax <- 0.59

if(! exists("df1"))
  load("data.Rda")

summary(df1[,-1])


# ==============================================================================
# PLOT 1
#===============================================================================
# Bring the data in a form that can be plotted with parCoordPlot()
# Plot the data: first with alpha = 1.0
data_m<-prepareData(df1[,-1], colOrdering, NULL)
p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 1.0, xText, yText, 0.1)
#ggsave(paste("../pdf/pollen", "pollen1.pdf",sep="/"),width=12.0,height=8.0);

p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 1.0, xText, yText, 0.6)
# ggsave(paste("../pdf/pollen", "pollen1.png",sep="/"),width=12.0,height=8.0, dpi=300);

# ==============================================================================
# Plot 2
# ==============================================================================
p <- ggplot(data_m[,-1], aes(x=value, colour=variable)) + geom_density(size = 1.0)
p <- p + theme_bw()
p <- p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
p <- p + theme(legend.text= element_text(size = rel(1.5), angle = 0))
p <- p + theme(legend.title= element_text(size = rel(1.2), angle = 0))
p <- p + theme(legend.position=c(0.85, 0.5))
#ggsave(paste("../pdf/pollen", "density.pdf",sep="/"), width=12.0, height=8.0);
# ggsave(paste("../pdf/pollen", "density.png",sep="/"), width=12.0, height=8.0, dpi=1200);

# ==============================================================================
# Plot 3
# ==============================================================================
data_m <- prepareData(df1[,-1], colOrdering, NULL)
p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText, 0.1)
#ggsave(paste("../pdf/pollen", "pollen2.pdf",sep="/"), width=12.0, height=8.0);

p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText, 0.6)
# ggsave(paste("../pdf/pollen", "pollen2.png",sep="/"), width=12.0, height=8.0, dpi=800);

# scale data
dfScale <- df1
if(useDataScaling)
  dfScale <- data.frame(df1[,1], apply(df1[,-1], MARGIN = 2, rescale))
nam <- names(dfScale)
nam[1] <- "No"
names(dfScale) <- nam




dfSub <- dfScale 
if(useSubset)
{
  # Zoom in data first
  dfSub <- subset(x=dfScale, NUB > 0.4 & NUB < 0.6)
  dfSub <- subset(x=dfSub, CRACK > 0.4 & CRACK < 0.6)
  dfSub <- subset(x=dfSub, WEIGHT > 0.4 & WEIGHT < 0.6)
  dfSub <- subset(x=dfSub, RIDGE > 0.4 & RIDGE < 0.6)
  dfSub <- subset(x=dfSub, DENSITY > 0.4 & DENSITY < 0.6)
  
  
  # ==============================================================================
  # Plot 4 (first zoom in the data)
  # ==============================================================================
  data_m<-prepareData(dfSub[,-1], colOrdering, NULL)
  p <- parCoordPlot(data_m, useWhiteBackground, xLab+1, yLab+1, 0.2, xText+1, yText+1, 0.1)
  #ggsave(paste("../pdf/pollen", "pollenZoom1.pdf",sep="/"), width = 12.0, height = 8.0)
  
  p <- parCoordPlot(data_m, useWhiteBackground, xLab+1, yLab+1, 0.2, xText+1, yText+1, 0.6)
  # ggsave(paste("../pdf/pollen", "pollenZoom1.png",sep="/"), width = 12.0, height = 8.0, dpi=800)
  
  # more precise subset
  dfSub <- subset(x=dfScale, NUB > nubMin & NUB < nubMax)
  dfSub <- subset(x=dfSub, CRACK > crackMin & CRACK < crackMax)
  dfSub <- subset(x=dfSub, WEIGHT > weightMin & WEIGHT < weightMax)
  dfSub <- subset(x=dfSub, RIDGE > ridgeMin & RIDGE < ridgeMax)
  dfSub <- subset(x=dfSub, DENSITY > densityMin & DENSITY < densityMax)

  # ==============================================================================
  # Plot 5 (Second zoom in the data)
  # ==============================================================================
  data_m <- prepareData(dfSub[,-1], colOrdering, NULL)
  p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.3, xText, yText, 0.1)
  #ggsave(paste("../pdf/pollen", "pollenZoom2.pdf",sep="/"), width = 12.0, height = 8.0)
  p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.3, xText, yText, 0.6)
  # ggsave(paste("../pdf/pollen", "pollenZoom2.png",sep="/"), width = 12.0, height = 8.0, dpi=800)
  
  subIndex <- dfSub$No
  
  # higher alpha for PC-plot
  p <- plotmatrix(dfSub[,-1], colour = "#009999")
  
  p
  

  eureka <- df1[subIndex, c(5,6)]
  
  
  # eureka is mirrored, Turn arround
  eureka$WEIGHT <- 2.0 - eureka$WEIGHT
  
  # ==============================================================================
  # Plot 6 (Scatter plot of the data)
  # ==============================================================================
  p <- ggplot(eureka) + geom_point(aes(x = DENSITY, y = WEIGHT), size=5, alpha = 0.5)
  p <- p + theme_bw()
  p <- p + theme(axis.title.y = element_text(size = rel(yLab+2), angle = 90))
  p <- p + theme(axis.title.x = element_text(size = rel(xLab+2), angle = 0))
  p <- p + theme(axis.text.y = element_text(size = rel(yText+2), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText+2), angle = 0))
  # ggsave(paste("../pdf/pollen", "pollenSactter.pdf",sep="/"), width = 8.0, height = 12.3)
  # ggsave(paste("../pdf/pollen", "pollenSactter.png",sep="/"), width = 12.0, height = 8.0)
}

