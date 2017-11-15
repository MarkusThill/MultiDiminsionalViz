#TODO: There are no negative values... so do scaling from 0..1 and not -1..+1

require(ggplot2)
require(reshape2)
require(PairViz)
require(GGally)
library(scales) # for muted

source(file = "wineFunc.r")

# White wine
#df1<-read.csv2(file="data/winequality-white.txt",header=TRUE, sep=";",quote = "")
#names(df1)<-c("f_acid",  "v_acid", "c_acid",  "sugar",	"chlorides",	"fsul_diox",	"tsul_diox",	"density",	"pH",	"sulphates",	"alcohol",	"quality")
#df1 <- apply(df1, MARGIN=2, as.character)
#df1 <- apply(df1, MARGIN=2, as.numeric)
#df1 <- data.frame(df1)
#save(df1,file="dataWhite.Rda")

# Red wine
#df2<-read.csv2(file="data/winequality-red.txt",header=TRUE, sep=";",quote = "")
#names(df2)<-c("f_acid",  "v_acid", "c_acid",  "sugar",  "chlorides",	"fsul_diox",	"tsul_diox",	"density",	"pH",	"sulphates",	"alcohol",	"quality")
#df2 <- apply(df2, MARGIN=2, as.character)
#df2 <- apply(df2, MARGIN=2, as.numeric)
#df2 <- data.frame(df2)
#save(df2,file="dataRed.Rda")


#Definitions
# ------------------------------------------------------------------------------------------------------------------
sampleSize <- 4898 # max. 4898 for the white-wine dataset
# Definitions
# ------------------------------------------------------------------------------------------------------------
useDataScaling <- FALSE
xLab<-1.5
yLab<- 1.5
xText <- 1.5
yText <- 1.5
colOrdering <- c(1:5)
useWhiteBackground <- TRUE
globalAlpha = 0.01
roundLabelDigits = 2
pngDPI <- 100
# -----------------------------------------------------------------------------------------------------------------


#if(! exists("df1"))
  load("dataWhite.Rda")

#if(! exists("df2"))
  load("dataRed.Rda")

summary(df1)
summary(df2)



# ==============================================================================
# PLOT 0: Complete data
#===============================================================================
# Remove outliers from df1 (not extreme cases like quality==9)
rmOutlDataIndex <- unlist(apply(df1[,-12], MARGIN =2, remove_outliers, perc=0.005))
imp <- which(df1$quality > 8 | df1$quality < 4)
rmOutlDataIndex <- setdiff(rmOutlDataIndex, imp)
df1o <- df1[-rmOutlDataIndex,]

df1o <- subset(df1o, fsul_diox < 150)

# Remove outliers from df2 (not extreme cases like quality==8)
rmOutlDataIndex <- unlist(apply(df2[,-12], MARGIN =2, remove_outliers, perc=0.005))
imp <- which(df2$quality > 7 | df2$quality < 4)
rmOutlDataIndex <- setdiff(rmOutlDataIndex, imp)
df2o <- df2[-rmOutlDataIndex,]
df2o <- subset(df2o, sulphates < 1.9)

max0Df1 <- round(apply(df1o, MARGIN=2, max), digits = roundLabelDigits)
min0Df1 <- round(apply(df1o, MARGIN=2, min), digits = roundLabelDigits)
max0Df2 <- round(apply(df2o, MARGIN=2, max), digits = roundLabelDigits)
min0Df2 <- round(apply(df2o, MARGIN=2, min), digits = roundLabelDigits)
# White wine
df <- data.frame(apply(df1, MARGIN = 2, rescale))
data_m<-prepareData(df, c(1:12), NULL)
p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.02, xText, yText, max0Df1, min0Df1, 0.1)
#ggsave(paste("../pdf/wine", "white0.pdf", sep="/"), width=12.0, height=8.0);

p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.02, xText, yText, max0Df1, min0Df1, 0.6)
ggsave(paste("../pdf/wine", "white0.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);


#Red-Wine
df <- data.frame(apply(df2, MARGIN = 2, rescale))
data_m<-prepareData(df, c(1:12), NULL)
p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.05, xText, yText, max0Df2, min0Df2, 0.1)
#ggsave(paste("../pdf/wine", "red0.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.05, xText, yText, max0Df2, min0Df2, 0.6)
ggsave(paste("../pdf/wine", "red0.png", sep="/"), width=12.0, height=8.0, dpi = pngDPI);



# calculate min/max for every attribute
maxDf1 <- round(apply(df1o, MARGIN=2, max), digits = roundLabelDigits)
minDf1 <- round(apply(df1o, MARGIN=2, min), digits = roundLabelDigits)
maxDf2 <- round(apply(df2o, MARGIN=2, max), digits = roundLabelDigits)
minDf2 <- round(apply(df2o, MARGIN=2, min), digits = roundLabelDigits)


#data has to be scaled, Otherwise we get a horrible plot
df1s <- data.frame(apply(df1o, MARGIN = 2, rescale))
df2s <- data.frame(apply(df2o, MARGIN = 2, rescale))








# ==============================================================================
# PLOT 1: With removed outliers
#===============================================================================
# White wine
data_m<-prepareData(df1s, c(1:12), NULL)
p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.02, xText, yText, maxDf1, minDf1, 0.1)
#ggsave(paste("../pdf/wine", "white1.pdf", sep="/"), width=12.0, height=8.0);
p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.02, xText, yText, maxDf1, minDf1, 0.6)
ggsave(paste("../pdf/wine", "white1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);

#Red-Wine
data_m<-prepareData(df2s, c(1:12), NULL)
p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.05, xText, yText, maxDf2, minDf2, 0.1)
#ggsave(paste("../pdf/wine", "red1.pdf", sep="/"), width=12.0, height=8.0);
p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.05, xText, yText, maxDf2, minDf2, 0.6)
ggsave(paste("../pdf/wine", "red1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);





# ==============================================================================
# PLOT 2: White-Wine: Use colors to represent quality
#===============================================================================
# Importance: Increasing
# v_acid: Brushing shows that the top lines are also bad quality #also Best/worst plot shows this
# density 
# alcohol
# -----------------------------------------------------------------
df <- data.frame(df1s, df1o$quality)
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))

lsize = 0.7
cols <- rev(rainbow(9, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]
#cquality:     3     4    5     6     7     8     9
alphaVec <- c(0.35, 0.1, 0.07, 0.01, 0.02, 0.05, 0.15)
data_m<-prepareData(df, c(1:13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxDf1, minDf1, cols, alphaVec, lsize)
#ggsave(paste("../pdf/wine", "whiteColored1.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxDf1, minDf1, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "whiteColored1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);





# ==============================================================================
# PLOT 2.1: White-Wine: Use colors to represent quality (increasing Importance)
#===============================================================================
# Importance: Increasing
# f_acid, sulphates, c_acid, pH, fsul_diox,tsul_diox,chlorides,sugar,v_acid ,density,alcohol
# -----------------------------------------------------------------

dfOrder <- c(1,10, 3,9, 6,7,5,4,2,8,11,12)
df <- data.frame(df1s, df1o$quality)
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))


xlabels1 <- maxDf1[dfOrder]
xlabels2 <- minDf1[dfOrder]

lsize = 0.7
cols <- rev(rainbow(9, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]
#cquality:     3     4    5     6     7     8     9
alphaVec <- c(0.35, 0.1, 0.07, 0.01, 0.02, 0.05, 0.15)
data_m<-prepareData(df, c(dfOrder, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "whiteColoredOrder1.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "whiteColoredOrder1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);




# ==============================================================================
# PLOT 2.2: White-Wine: Use colors to represent quality (increasing Importance)
# Invert density
#===============================================================================
# Importance: Increasing
# f_acid, sulphates, c_acid, pH, fsul_diox,tsul_diox,chlorides,sugar,v_acid ,density,alcohol
# -----------------------------------------------------------------

dfOrder <- c(1,10, 3,9, 6,7,5,4,2,8,11,12)
df <- data.frame(df1s, df1o$quality)
df$density <- 1- df$density
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))


#density is col 10 now
xlabels1 <- maxDf1[dfOrder]
xlabels2 <- minDf1[dfOrder]
tmp <- xlabels1[10]
xlabels1[10] <- xlabels2[10]
xlabels2[10] <- tmp


lsize = 0.7
cols <- rev(rainbow(9, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]
#cquality:     3     4    5     6     7     8     9
alphaVec <- c(0.35, 0.1, 0.07, 0.01, 0.02, 0.05, 0.15)
data_m<-prepareData(df, c(dfOrder, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "whiteColoredDensInv1.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "whiteColoredDensInv1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);







# ==============================================================================
# PLOT 2.2: White-Wine: Use colors to represent quality (increasing Importance)
# Invert density and v_acid
#===============================================================================
# Importance: Increasing
# f_acid, sulphates, c_acid, pH, fsul_diox,tsul_diox,chlorides,sugar,v_acid ,density,alcohol
# -----------------------------------------------------------------

dfOrder <- c(1,10, 3,9,6,7,5,4,2,8,11,12)
df <- data.frame(df1s, df1o$quality)
df$density <- 1- df$density
df$v_acid <- 1 - df$v_acid
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))


#density is col 10 now
xlabels1 <- maxDf1[dfOrder]
xlabels2 <- minDf1[dfOrder]
tmp <- xlabels1[10]
xlabels1[10] <- xlabels2[10]
xlabels2[10] <- tmp

#v_acid is col 9 now
tmp <- xlabels1[9]
xlabels1[9] <- xlabels2[9]
xlabels2[9] <- tmp

lsize = 0.7
cols <- rev(rainbow(9, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]
#cquality:     3     4    5     6     7     8     9
alphaVec <- c(0.35, 0.1, 0.07, 0.01, 0.02, 0.05, 0.15)
data_m<-prepareData(df, c(dfOrder, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "whiteColoredDensInv_VAC.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "whiteColoredDensInv_VAC.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);





# ==============================================================================
# PLOT 2.3: White-Wine: Use colors to represent quality (increasing Importance)
# Invert density and v_acid
#===============================================================================
# Importance: Increasing
# f_acid, sulphates, c_acid, pH, fsul_diox,tsul_diox,chlorides,sugar,v_acid ,density,alcohol
# -----------------------------------------------------------------

dfOrder <- c(1,10,7,4,8,11,12,2,5,6,3,9)
df <- data.frame(df1s, df1o$quality)
df$density <- 1- df$density
df$v_acid <- 1 - df$v_acid
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))



xlabels1 <- maxDf1[dfOrder]
xlabels2 <- minDf1[dfOrder]
#density is col 5 now
tmp <- xlabels1[5]
xlabels1[5] <- xlabels2[5]
xlabels2[5] <- tmp

#v_acid is col 8 now
tmp <- xlabels1[8]
xlabels1[8] <- xlabels2[8]
xlabels2[8] <- tmp

lsize = 0.7
cols <- rev(rainbow(9, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]
#cquality:     3     4    5     6     7     8     9
alphaVec <- c(0.35, 0.1, 0.07, 0.01, 0.02, 0.05, 0.15)
data_m<-prepareData(df, c(dfOrder, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "plot2_3.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, xlabels1, xlabels2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "plot2_3.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);



# ==============================================================================
# PLOT 2.4: White-Wine: Use colors to represent quality (increasing Importance)
# Invert density and v_acid
#===============================================================================
# Importance: Increasing
# f_acid, sulphates, c_acid, pH, fsul_diox,tsul_diox,chlorides,sugar,v_acid ,density,alcohol
# -----------------------------------------------------------------

dfOrder <- c(10,7,4,8,11,12,2,5,6,3,9,1)
df <- data.frame(df1s, df1o$quality)
df$density <- 1- df$density
df$v_acid <- 1 - df$v_acid
df$sugar <- 1 - df$sugar
df$f_acid <- 1 - df$f_acid
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))


myLab1 <- maxDf1
myLab2 <- minDf1

swapLabels <- function(x) {
  no <- grep(x, colnames(df))
  tmp <- myLab1[no]
  myLab1[no] <<- myLab2[no]
  myLab2[no] <<- tmp
}

swapLabels("density")
swapLabels("v_acid")
swapLabels("sugar")
swapLabels("f_acid")

myLab1 <- myLab1[dfOrder]
myLab2 <- myLab2[dfOrder]

lsize = 0.7
cols <- rev(rainbow(9, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]
#cquality:     3     4    5     6     7     8     9
alphaVec <- c(0.35, 0.1, 0.07, 0.01, 0.02, 0.05, 0.15)
data_m<-prepareData(df, c(dfOrder, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, myLab1, myLab2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "plot2_4.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, myLab1, myLab2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "plot2_4.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);


#require(tree)
#tt = tree(quality ~ f_acid + v_acid  +c_acid	+sugar+	chlorides	+fsul_diox+	tsul_diox	+density+	pH+	sulphates+	alcohol, data = df1)
#summary(tt)
#tt


#wines with low quality have which average alcohol
gg <- subset(df1, quality<=5)
mean(gg$alcohol)
mean(gg$density)

#wines with low quality have which average alcohol
gg <- subset(df1, quality>=7)
mean(gg$density)

# wines with sulphates > 0.75
gg <- subset(df1, sulphates > 0.85)
mean(gg$quality)

# wines with chlorides< 0.01
gg <- subset(df1, chlorides < 0.02)
mean(gg$quality)

# ==============================================================================
# PLOT 3: Red-Wine: Use colors to represent quality
#===============================================================================
dfOrder <- c(4,8,10,11,12,2,7,5,6,3,9,1)
df <- data.frame(df2s, df2o$quality)
df$tsul_diox <- 1 - df$tsul_diox

myLab1 <- maxDf2
myLab2 <- minDf2

swapLabels <- function(x) {
  no <- grep(x, colnames(df))
  tmp <- myLab1[no]
  myLab1[no] <<- myLab2[no]
  myLab2[no] <<- tmp
}

swapLabels("tsul_diox")

myLab1 <- myLab1[dfOrder]
myLab2 <- myLab2[dfOrder]

nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df$cquality <- as.factor(as.character(df$cquality))

lsize = 0.8
cols <- rev(rainbow(8, start = 0.00, end=0.16))
cols <- cols[-c(4,5)]

#cquality:     3     4    5     6     7    8
alphaVec <- c(0.3, 0.1, 0.04, 0.04, 0.1, 0.4)
data_m<-prepareData(df, c(dfOrder, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, myLab1, myLab2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "redColored1.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, myLab1, myLab2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "redColored1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);



#===============================================================================
# Plot 4: White Wine: Print only two best and two worst
#===============================================================================
# Most important:
# v_acid    
# density 
# fsul_diox
# alcohol
# ----------------------------------------------------
df <- data.frame(df1s, df1o$quality)
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df <- subset(df, df$cquality<=4 | df$cquality>=8)

df$cquality <- as.factor(as.character(df$cquality))
lsize = 0.7
cols <- rev(rainbow(6, start = 0.02, end=0.17))
cols <- cols[-c(3,4)]
#cquality:     3     4    8     9
alphaVec <- c(0.5, 0.35, 0.25, 0.5)
data_m<-prepareData(df, c(1:13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxDf1, minDf1, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "whiteBestWorst.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxDf1, minDf1, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "whiteBestWorst.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);


#===============================================================================
# Plot 5: Red Wine: Print only two best and two worst
#===============================================================================
# Importance: Increasing
# alcohol
# v_acid
# sulphates
# ------------------------------------------------------
df <- data.frame(df2s, df2o$quality)
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam
df <- subset(df, df$cquality<=4 | df$cquality>=7)

df$cquality <- as.factor(as.character(df$cquality))
lsize = 0.7
cols <- rev(rainbow(6, start = 0.02, end=0.17))
cols <- cols[-c(3,4)]
#cquality:     3     4    8     9
alphaVec <- c(0.6, 0.3, 0.2, 0.5)
data_m<-prepareData(df, c(1:13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxDf2, minDf2, cols, alphaVec, lsize)
#ggsave(paste("../pdf/wine", "redBestWorst.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxDf2, minDf2, cols, alphaVec, 1.2)
ggsave(paste("../pdf/wine", "redBestWorst.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);



# ===============================================================================
# Plot 6: General differences between Red and Whitewine
# ===============================================================================
num <- 4
tmp1 <- data.frame()
tmp2 <- data.frame()
df <- data.frame()
interval1 <- floor(nrow(df1o)/num)
interval2 <- floor(nrow(df2o)/num)
c1 <- 1
c2 <- 1
for(i in 1:num) {
  if(i==num){
    interval1 <- nrow(df1o) - c1
    interval2 <- nrow(df2o) - c2
  }
  tmp <- data.frame(rep(paste(i, "_white", sep=""), interval1))
  names(tmp) <- "cquality"
  tmp1 <- rbind(tmp1, tmp)
  df <- rbind(df, df1o[c1:(c1+interval1-1),])
  
  tmp <- data.frame(rep(paste(i, "_red", sep=""), interval2))
  names(tmp) <- "cquality"
  tmp1 <- rbind(tmp1, tmp)
  df <- rbind(df, df2o[c2:(c2+interval2-1),])
  c1 <- c1 + interval1
  c2 <- c2 + interval2
}
df <- data.frame(df, tmp1)

# calculate min/max for every attribute
maxDf <- round(apply(df[,-13], MARGIN=2, max), digits = roundLabelDigits)
minDf <- round(apply(df[,-13], MARGIN=2, min), digits = roundLabelDigits)

df <- data.frame(apply(df[,-13], MARGIN = 2, rescale), df[,13])
nam <- names(df)
nam[13] <- "cquality"
names(df) <- nam


df$f_acid <- 1 - df$f_acid
df$density <- 1 - df$density
df$v_acid <- 1 - df$v_acid
df$alcohol <- 1 - df$alcohol

myLab1 <- maxDf
myLab2 <- minDf

swapLabels <- function(x) {
  no <- grep(x, colnames(df))
  tmp <- myLab1[no]
  myLab1[no] <<- myLab2[no]
  myLab2[no] <<- tmp
}

swapLabels("f_acid")
swapLabels("density")
swapLabels("v_acid")
swapLabels("alcohol")

colOrdering <- c(7,4,6,1,8,2,3,5,9,10,11,12)
myLab1 <- myLab1[colOrdering]
myLab2 <- myLab2[colOrdering]

lsize = 0.1
cols <- rep(c("#5555CC", "#CC5555"), num)
alphaVec <- rep(c(0.02, 2*0.02), num)


data_m<-prepareData(df, c(colOrdering, 13), 'cquality')
p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, myLab1, myLab2, cols, alphaVec, lsize)
ggsave(paste("../pdf/wine", "compareBoth.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotCol(data_m, useWhiteBackground, xLab, yLab, xText, yText, myLab1, myLab2, cols, alphaVec, 0.6)
ggsave(paste("../pdf/wine", "compareBoth.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);




# ===============================================================================
# Plot 7 
# ===============================================================================
myOrder <- c(7,2,8,6,1,12,4,11,9,10,3,5)
df <- df1s
df$sugar <- 1 - df$sugar
df$pH <- 1 - df$pH
df$c_acid <- 1 - df$c_acid
df$f_acid <- 1 - df$f_acid
df$fsul_diox <- 1 - df$fsul_diox
df$v_acid <- 1 - df$v_acid

# Labels have to be swapped for certain axes
myLab1 <- maxDf1
myLab2 <- minDf1

swapLabels <- function(x) {
  no <- grep(x, colnames(df))
  tmp <- myLab1[no]
  myLab1[no] <<- myLab2[no]
  myLab2[no] <<- tmp
}

swapLabels("sugar")
swapLabels("pH")
swapLabels("c_acid")
swapLabels("f_acid")
swapLabels("fsul_diox")
swapLabels("v_acid")

# White wine
data_m<-prepareData(df, myOrder, NULL)
p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.02, xText, yText, myLab1[myOrder], myLab2[myOrder], 0.1)
ggsave(paste("../pdf/wine", "whiteOrderedInv.pdf", sep="/"), width=12.0, height=8.0);

p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.02, xText, yText, myLab1[myOrder], myLab2[myOrder], 0.6)
ggsave(paste("../pdf/wine", "whiteOrderedInv.png", sep="/"), width=12.0, height=8.0, dpi=3*pngDPI);

#Red-Wine
data_m<-prepareData(df2s, c(1:12), NULL)
p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.05, xText, yText, maxDf2, minDf2, 0.1)
#ggsave(paste("../pdf/wine", "red1.pdf", sep="/"), width=12.0, height=8.0);
p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.05, xText, yText, maxDf2, minDf2, 0.6)
ggsave(paste("../pdf/wine", "red1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);

# ===============================================================================
# Variables actually used in tree construction:
# [1] "tsul_diox" "sugar"     "fsul_diox" "f_acid"    "density"  
# Number of terminal nodes:  14 
# Residual mean deviance:  0.3538 = 2275 / 6431 
# Misclassification error rate: 0.05477 = 353 / 6445 
# ===============================================================================
# tmp <- rep("white", nrow(df1s))
# tmp <- data.frame(df1s, tmp)
# nam <- names(tmp)
# nam[length(nam)] <- "class"
# names(tmp) <- nam

# tmp1 <- rep("red", nrow(df2s))
# tmp1 <- data.frame(df2s, tmp1)
# nam <- names(tmp1)
# nam[length(nam)] <- "class"
# names(tmp1) <- nam

# tmp <- rbind(tmp, tmp1)

# require(tree)
# tt = tree(class ~ f_acid + v_acid	+c_acid	+sugar+	chlorides	+fsul_diox+	tsul_diox	+density+	pH+	sulphates+	alcohol, data = tmp)
# summary(tt)
# tt
# ==============================================================================
# Build RandomForest
#===============================================================================
# just check, which variables are important
# WHITE-Wine
#---------------------------------------------------
#library(randomForest)
#rfData<-df1o
#rfData$quality <-as.factor(as.character(rfData$quality))
#rf<-randomForest (rfData[,-12],rfData[,12], ntree = 1000  ,importance = TRUE, proximity=FALSE) 
#rf1<-rf
#save(rf1, file="rfWhite.Rda")

# load("rfWhite.Rda")
# importance<-data.frame(rf1$importance) #importance
# importance[order(importance$MeanDecreaseAccuracy),]
# varImpPlot(rf1)
# rf1$confusion

# f_acid    -0.0004638889 0.02410611 0.04647668 0.03670797 0.05548734 0.08554183  0.0011666667           0.04415877         245.0974
# sulphates -0.0004345238 0.03148464 0.04389646 0.03853982 0.06153013 0.09568090 -0.0033333333           0.04592470         263.0733
# c_acid     0.0004543651 0.06061737 0.05854916 0.04286296 0.06907629 0.08683613  0.0008333333           0.05419011         265.3222
# pH        -0.0008230159 0.03366380 0.06110875 0.04109717 0.08650435 0.09459412  0.0020833333           0.05674433         282.8991
# fsul_diox  0.0022261905 0.08953551 0.06034893 0.05203734 0.07132691 0.10059513  0.0001666667           0.06065922         310.1526
# tsul_diox -0.0004432540 0.04840800 0.07101108 0.04898277 0.09661760 0.10736509 -0.0001666667           0.06597856         299.7084
# chlorides  0.0010884921 0.05052234 0.07175975 0.04851856 0.12327853 0.12463547  0.0060000000           0.07150486         279.2380
# sugar      0.0010067460 0.03886705 0.07083069 0.06639127 0.09495038 0.10777053  0.0008333333           0.07316719         290.1753
# v_acid    -0.0008869048 0.07971720 0.10446589 0.07388266 0.12040858 0.15065216  0.0035000000           0.09397413         324.7394
# density    0.0001694444 0.04577071 0.10664190 0.07880800 0.14583786 0.15358834  0.0033333333           0.10041983         342.0368
# alcohol   -0.0041142857 0.06609808 0.17185789 0.08276928 0.21289006 0.23522143 -0.0001666667           0.13726051         377.6320





# RED-Wine
#----------------------------------------------------

#sugar      0.0001833333 -0.002143044 0.02704053 0.02620316 0.04158689 0.01646825           0.02727756         71.27033
#fsul_diox -0.0007833333  0.004208975 0.03157303 0.02785446 0.03938179 0.01753023           0.02976817         66.87049
#pH        -0.0020000000  0.005563309 0.03157658 0.02759931 0.04238179 0.01875527           0.03011609         75.24786
#chlorides -0.0016166667 -0.006276586 0.04317110 0.03017753 0.04964607 0.01725209           0.03662551         82.61163
#f_acid     0.0007666667 -0.004373764 0.03931584 0.03140129 0.06297637 0.02393344           0.03722613         76.87751
#c_acid     0.0029500000  0.007996488 0.03516687 0.03105306 0.09172902 0.03393589           0.03939682         74.68470
#density    0.0009666667 -0.008256124 0.05021313 0.05460245 0.08598363 0.02044289           0.05391503         91.84898
#tsul_diox  0.0041333333  0.007665966 0.06238724 0.05164141 0.08330587 0.02862587           0.05817352        106.42963
#v_acid     0.0030000000  0.027487490 0.05878796 0.04175152 0.13766075 0.04595213           0.06018614        104.87931
#sulphates  0.0012000000  0.021773511 0.07090933 0.06130853 0.14053743 0.04928424           0.07339960        112.23663
#alcohol    0.0051333333  0.007810932 0.13510520 0.07630813 0.19195934 0.06322792           0.11290501        149.26035

#library(randomForest)
#rfData<-df2o
#rfData$quality <-as.factor(as.character(rfData$quality))
#rf2<-randomForest (rfData[,-12],rfData[,12], ntree = 1000  ,importance = TRUE, proximity=FALSE) 
#save(rf2, file="rfRed.Rda")

#load("rfRed.Rda")
#importance<-data.frame(rf2$importance) #importance
#importance[order(importance$MeanDecreaseAccuracy),]
#varImpPlot(rf2)
#rf2$confusion


# # WHITE-Wine (Best/Worst)
# #-------------------------------------------------------
# library(randomForest)
# rfData<-subset(df1o, quality <=4 | quality>=8)
# rfData$quality <-as.factor(as.character(rfData$quality))
# rf3<-randomForest (rfData[,-12],rfData[,12], ntree = 1000  ,importance = TRUE, proximity=FALSE) 
# importance<-data.frame(rf3$importance) #importance
# importance[order(importance$MeanDecreaseAccuracy),]
# varImpPlot(rf3)
# rf3$confusion
# 
# 
# # Red-Wine (Best/Worst)
# #-------------------------------------------------------
# library(randomForest)
# rfData<-subset(df2o, quality <=4 | quality>=7)
# rfData$quality <-as.factor(as.character(rfData$quality))
# rf4<-randomForest (rfData[,-12],rfData[,12], ntree = 1000  ,importance = TRUE, proximity=FALSE) 
# importance<-data.frame(rf4$importance) #importance
# importance[order(importance$MeanDecreaseAccuracy),]
# varImpPlot(rf4)
# rf4$confusion
