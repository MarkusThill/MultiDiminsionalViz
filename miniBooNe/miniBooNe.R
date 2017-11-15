require(ggplot2)
require(reshape2)
require(PairViz)
require(GGally)
library(scales) # for muted


source(file = "miniBooNeFunc.r", chdir=TRUE)


#number of observations: 130064
# Definitions
noSignalEvents <- 36499 # First 36499 lines are Signal-Events 
noBackgroundEvents<- 93565 #Last 93565 Lines are Background-Events (Sample equally??????????????????????????)

sampleSize <- 20000
equallySampleClasses <- TRUE
useDataScaling <- TRUE
colOrdering <- 1:51
useWhiteBackground <- TRUE

xLab<-1.5
yLab<- 1.5
xText <- 1.5
yText <- 1.5
axisLab <- 7

dpiQuality<-100 #CHANGE

# --------------------------------------------------------------------------------------------------------------
# Only needed once
# ===================================================================================
# load data from csv-file
#df1<-read.csv2(file="data/MiniBooNE_PID1.txt",header=FALSE,sep="",quote = "")
#df1 <- apply(df1, MARGIN=2, as.character)
#df1 <- apply(df1, MARGIN=2, as.numeric)
#df1 <- data.frame(df1)

# classify data
#classSignal <- data.frame(rep('SE', noSignalEvents))
#names(classSignal) <- c("V51")
#classBackGrnd <- data.frame(rep('BG', noBackgroundEvents))
#names(classBackGrnd) <- c("V51")
#classCol <- rbind(classSignal, classBackGrnd)

#df1 <- cbind(df1, classCol)
#save(df1,file="data.Rda")
# ----------------------------------------------------------------------------------------------------------------

# data in R-format. Faster to load...

if(! exists("df1"))
  load("data.Rda")

# sample data
if(equallySampleClasses) {
  samp1 <- sample(1:noSignalEvents, sampleSize/2, replace = FALSE, prob = NULL)
  samp2 <- sample(noSignalEvents+1:noBackgroundEvents, sampleSize/2, replace = FALSE, prob = NULL)
  samp1<-sort(samp1)
  samp2<-sort(samp2)
  data_samp<-df1[c(samp1, samp2),]
} else {
  samp<-sample(1:nrow(df1), sampleSize, replace = FALSE, prob = NULL)
  samp<-sort(samp)
  data_samp<-df1[samp,]
}

######TODO: RANDOM FOREST IS WRONG.....

# just check, which variables are important
#library(randomForest)
#rfData<-df1
#rf<-randomForest (rfData[,-51],rfData[,51], ntree =500  ,importance=TRUE) 

#if(! exists("rf"))
#  load("rf.Rda")
#importance<-data.frame(rf$importance) #importance
#importance[order(importance$MeanDecreaseAccuracy),]
#rf$confusion
#varImpPlot(rf)



# boxplot and summary
#boxplot(data_samp[,-51]) # a lot of outliers 
summary(data_samp) # many observations with the value -999.0 in certain attributes
#apply(data_samp, MARGIN=2, boxplot)

######################################################################################################################
# Divide every class into further subclasses (SE_1, SE_2,...), to make plots nicers                                  #
######################################################################################################################
nSubClass <- 5
dfSE <- subset(data_samp, V51 == "SE")
dfBG <- subset(data_samp, V51 == "BG")

interval <- nrow(dfSE) / nSubClass
dfNew  <- data.frame()
for(i in 0:(nSubClass-1)){
  range <- (i*interval+1):((i+1)*interval)
  tmp <- dfSE[range,]
  tmp$V51 <- rep(paste(i,"_SE",i, sep=""), interval)
  dfNew <- rbind(dfNew, tmp)
  
  tmp <- dfBG[range,]
  tmp$V51 <- rep(paste(i,"_BG",i, sep=""), interval)
  dfNew <- rbind(dfNew, tmp)
}

# --------------------------------------------------------------------------------------------------------------------




######################################################################################################################
# Further Data preparation                                                                                           #
######################################################################################################################
# Looks like there are records were the values for certain attributes are -999.0
# No. of valid entries for full dataset: 129593
valid_data <- dfNew[apply(dfNew[, -51], MARGIN = 1, function(x) all(x > -998)), ]
# boxplot(valid_data[,-51])
summary(valid_data)

# Remove outliers, max 1% for every attribute
rmOutlDataIndex <- unlist(apply(valid_data[,-51], MARGIN =2, remove_outliers, 0.01))
names(rmOutlDataIndex) <- NA
rmOutlDataIndex <- unique(rmOutlDataIndex)

rmOutlData <- valid_data[-rmOutlDataIndex,]
# boxplot(rmOutlData[,-51])
# --------------------------------------------------------------------------------------------------------------------

roundLabelDigits <-  2
maxDfs <- round(apply(rmOutlData[,-51], MARGIN=2, max), digits = roundLabelDigits)
minDfs <- round(apply(rmOutlData[,-51], MARGIN=2, min), digits = roundLabelDigits)

maxDfs <- signif(maxDfs, digits = 4)
minDfs <- signif(minDfs, digits = 4)


######################################################################################################################
# Scale data                                                                                                         #
######################################################################################################################
dfs <- data.frame(apply(rmOutlData[,-51], MARGIN = 2, rescale), rmOutlData$V51)
nam <- names(dfs)
nam[51] <- "V51"
names(dfs) <- nam

summary(dfs)
# --------------------------------------------------------------------------------------------------------------------




######################################################################################################################
# TODO: Save the data again...                                                                                       #
######################################################################################################################

######################################################################################################################
# PLOT 1:                                                                                                            #
######################################################################################################################
#colVec <- rep(c("blue", "red"), nSubClass)
#alphaVec <- rep(c(0.01,0.01), nSubClass)
#data_m<-prepareData(dfs, c(1:51), 'V51')
#p <- parCoordPlotCol(data_m, TRUE, xLab, yLab, xText, yText, maxDfs, minDfs, colVec, alphaVec, 0.1)
#ggsave(paste("", "tmp.pdf", sep=""), width=25.0, height=8.0);

######################################################################################################################
# PLOT 1: Seperate plot into two plots  (in black)                                                      
######################################################################################################################
dfs1 <- dfs[,c(1:26,51)]
dfs2 <- dfs[,25:51]
colVec <- rep(c("black", "black"), nSubClass)
alphaVec <- rep(c(0.01,0.01), nSubClass)
#line1
data_m<-prepareData(dfs1, c(1:27), 'V51')
p <- parCoordPlotCol(data_m, TRUE, xLab+1, yLab+1, xText+0.5, yText+0.5, maxDfs, minDfs, colVec, alphaVec, 0.1)
p <- p + annotate("text", x = 1:26, y = -0.03, label = minDfs[1:26], size = rel(axisLab))
p <- p + annotate("text", x = 1:26, y = 1.03, label = maxDfs[1:26], size = rel(axisLab))
ggsave(paste("../pdf/MiniBooNe", "plot1_1.pdf", sep="/"), width=20.0, height=8.0);


#line2
data_m<-prepareData(dfs2, c(1:27), 'V51')
p <- parCoordPlotCol(data_m, TRUE, xLab+1, yLab+1, xText+0.5, yText+0.5, maxDfs, minDfs, colVec, alphaVec, 0.1)
p <- p + annotate("text", x = 1:26, y = -0.03, label = minDfs[25:50], size = rel(axisLab))
p <- p + annotate("text", x = 1:26, y = 1.03, label = maxDfs[25:50], size = rel(axisLab))
ggsave(paste("../pdf/MiniBooNe", "plot1_2.pdf", sep="/"), width=20.0, height=8.0);


######################################################################################################################
# PLOT 2: Seperate plot into two plots                                                       
######################################################################################################################
dfs1 <- dfs[,c(1:26,51)]
dfs2 <- dfs[,25:51]
colVec <- rep(c("#3333CC", "#CC3333"), nSubClass)
alphaVec <- rep(c(0.02,0.03), nSubClass)
#line1
data_m<-prepareData(dfs1, c(1:27), 'V51')
p <- parCoordPlotCol(data_m, TRUE, xLab+1, yLab+1, xText+0.5, yText+0.5, maxDfs, minDfs, colVec, alphaVec, 0.1)
p <- p + annotate("text", x = 1:26, y = -0.03, label = minDfs[1:26], size = rel(axisLab))
p <- p + annotate("text", x = 1:26, y = 1.03, label = maxDfs[1:26], size = rel(axisLab))
ggsave(paste("../pdf/MiniBooNe", "plot0_1.pdf", sep="/"), width=20.0, height=8.0);


#line2
data_m<-prepareData(dfs2, c(1:27), 'V51')
p <- parCoordPlotCol(data_m, TRUE, xLab+1, yLab+1, xText+0.5, yText+0.5, maxDfs, minDfs, colVec, alphaVec, 0.1)
p <- p + annotate("text", x = 1:26, y = -0.03, label = minDfs[25:50], size = rel(axisLab))
p <- p + annotate("text", x = 1:26, y = 1.03, label = maxDfs[25:50], size = rel(axisLab))
ggsave(paste("../pdf/MiniBooNe", "plot0_2.pdf", sep="/"), width=20.0, height=8.0);



######################################################################################################################
# PLOT 3: Only plot most important variables                                                   
######################################################################################################################
df <- dfs
selCol <- c(1,16,17,23,6,3,27,2,32,4,26,7,28)
colVec <- rep(c("#3333CC", "#CC3333"), nSubClass)
alphaVec <- rep(c(0.007,0.015), nSubClass)

df$V1 <- 1- df$V1
df$V32 <- 1- df$V32
df$V7 <- 1- df$V7
#df$V27 <- 1- df$V27
#df$V18 <- 1- df$V18

# Labels have to be swapped for certain axes
myLab1 <- maxDfs
myLab2 <- minDfs

swapLabels <- function(x) {
  no <- grep(x, colnames(df))
  tmp <- myLab1[no]
  myLab1[no] <<- myLab2[no]
  myLab2[no] <<- tmp
}

swapLabels("V1")
swapLabels("V32")
swapLabels("V7")


dfsImp <- df[,c(selCol, 51)]
data_m<-prepareData(dfsImp, c(1:14), 'V51')
myLab1 <- myLab1[selCol]
myLab2 <- myLab2[selCol]
#p <- parCoordPlotCol(data_m, TRUE, xLab, yLab, xText, yText, maxDfs, minDfs, colVec, alphaVec, 0.1)
p <- parCoordPlotCol(data_m, TRUE, xLab+1, yLab+1, xText+0.5, yText+0.5, myLab1, myLab2, colVec, alphaVec, 0.1)
p <- p + annotate("text", x = 1:13, y = -0.03, label = myLab2, size = axisLab*0.9)
p <- p + annotate("text", x = 1:13, y = 1.03, label = myLab1, size = axisLab*0.9)
ggsave(paste("../pdf/MiniBooNe", "plot3.pdf", sep="/"), width=12.0, height=8.0);



######################################################################################################################
# PLOT 4: Only plot LEAST important variables                                                   
######################################################################################################################
p <- ggplot(dfs, aes(x=dfs$V3)) + geom_density(size = 1.0)
p

selCol <- c(50,22,34,41,12,19,43,39,11,36)
dfsImp <- dfs[,c(selCol, 51)]
data_m<-prepareData(dfsImp, c(1:11), 'V51')
p <- parCoordPlotCol(data_m, TRUE, xLab, yLab, xText, yText, maxDfs, minDfs, colVec, alphaVec, 0.1)
p <- p + annotate("text", x = 1:10, y = -0.03, label = minDfs[selCol])
p <- p + annotate("text", x = 1:10, y = 1.03, label = maxDfs[selCol])
ggsave(paste("", "tmp.pdf", sep=""), width=20.0, height=8.0);

# # Matrix not for all columns
# #Gini: 1,17,3,16,27,2,6,32,23,13,4,7,18,15,28,14,26,21,10,25,37,20,48,8,38,9,39,42,24,31
# #Mean: 3,13,27,14,1,32,20,11,40,4,10,18,19,28,17,16,39,21,26,37,5,6,44,29,46,7,45,47,23,2
# #dfMat <- valid_data[, c(17,1,16,3,2,23,32,27,28,13,4,14,15,6, 51)]
