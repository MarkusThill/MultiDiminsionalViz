# Author: Markus Thill
# Date: 06.01.2013
# Initial Tests with parallel Coordinates
# Plot the banknote authentication Data Set 
# URL: http://archive.ics.uci.edu/ml/datasets/banknote+authentication

#source("http://bioconductor.org/biocLite.R")
#biocLite("graph")

# install.packages("GGally", dependencies=TRUE)

# Librarys
-----------------------------------------------------------------------
require(ggplot2)
require(reshape2)
require(PairViz)
require(GGally)

source(file = "banknoteFunc.r")

# Definitions
#----------------------------------------------------------------------
# Scale all axis into the range -1..+1
useDataScaling <- FALSE
excludeColScaling <- c(5)

# which size shall the sampled data have (1-nrow(data))
sampleSize<-1372 #max. 1372

# White background?
useWhiteBackground = FALSE

# x lable size
xLab<-1.3 

# y lable size
yLab<-1.3 

# Column Ordering: 
# 1 - variance
# 2 - skewness
# 3 - curtosis
# 4 - entropy
# 5 - class
colOrdering <- c(4,3,2,1,5)

# plot only selected classes
plotClass<-c('class0', 'class1')

#---------------------------------------------------------------------------------------------------
data<-read.csv2(file="data/data_banknote_authentication.txt",header=TRUE,sep=",",quote = "")

data$variance<-as.numeric(as.character(data$variance))
data$skewness<-as.numeric(as.character(data$skewness))
data$curtosis<-as.numeric(as.character(data$curtosis))
data$entropy<-as.numeric(as.character(data$entropy))

data$class[data$class == 0]<-"class0"
data$class[data$class == 1]<-"class1"


# Some Analysis
# --------------------------------------------------------------------------------------------------------------
# Variance
# find out the minimum and maximum of variance
minVar <- min(data$variance)
maxVar <- max(data$variance)

# find out minimum and maximum of variance in class0
class0<-subset(x=data, class %in% 'class0')
minVarClass0 <- min(class0$variance)
maxVarClass0 <- max(class0$variance)
meanVarClass0 <- mean(class0$variance)

# find out minimum and maximum of variance in class1
class1<-subset(x=data, class %in% 'class1')
minVarClass1 <- min(class1$variance)
maxVarClass1 <- max(class1$variance)
meanVarClass1 <- mean(class1$variance)

# how many observations are in the overlapping area of the variance.
# Area between minVarclass0 and maxVarClass1

overlapVar <- subset(x=data, variance > minVarClass0 & variance < maxVarClass1)

# plot overlapping area
# Bring the data in a form that can be plotted with parCoordPlot()
v_data <- prepareData(overlapVar, colOrdering, 'class')
parCoordPlot(v_data, useWhiteBackground, xLab, yLab)

# How many oberservations are there in the overlapping variance-area?
sizeOverLapVar <- nrow(overlapVar)
sizeOverLapVarRatio <- sizeOverLapVar / nrow(data) # 66%!!!! Much too large...
# --------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------------------
# Variance
# Estimate the Values by simply looking at the plot
minVarClass0 <- -3 #can be chosen even smaller than -1, because skewness is very high for wrong classified class0-elements for -1...
maxVarClass1 <- +1
overlapVar <- subset(x=data, variance > minVarClass0 & variance < maxVarClass1)

# plot overlapping range
v_data <- prepareData(overlapVar, colOrdering, 'class')
parCoordPlot(v_data, useWhiteBackground, xLab, yLab)
# remaining can be classified easily using skewness, and curtosis

# How many observations are in the wrong class? For Class0
wrongVar0 <- subset(x=data, variance < minVarClass0 & class %in% 'class0')
v_data <- prepareData(wrongVar0, colOrdering, 'class')
parCoordPlot(v_data, useWhiteBackground, xLab, yLab)

# How many observations are in the wrong class? For Class0
wrongVar1 <- subset(x=data, variance > maxVarClass1 & class %in% 'class1')
v_data <- prepareData(wrongVar1, colOrdering, 'class')
parCoordPlot(v_data, useWhiteBackground, xLab, yLab)


# --------------------------------------------------------------------------------------------------------------
# Skewness
# find out the minimum and maximum of Skewness
minSkew <- min(data$skewness)
maxSkew <- max(data$skewness)

# find out minimum and maximum of variance in class0
class0<-subset(x=data, class %in% 'class0')
minSkewClass0 <- min(class0$skewness)
maxSkewClass0 <- max(class0$skewness)
meanSkewClass0 <- mean(class0$skewness)

# find out minimum and maximum of variance in class1
class1<-subset(x=data, class %in% 'class1')
minSkewClass1 <- min(class1$skewness)
maxSkewClass1 <- max(class1$skewness)
meanSkewClass1 <- mean(class1$skewness)

# how many observations are in the overlapping area of the variance.
# Area between minVarclass0 and maxVarClass1

overlapSkew <- subset(x=data, skewness > minSkewClass0 & skewness < maxSkewClass1)

# plot overlapping area
# Bring the data in a form that can be plotted with parCoordPlot()
s_data <- prepareData(overlapSkew, colOrdering, 'class')
parCoordPlot(s_data, useWhiteBackground, xLab, yLab)

# How many oberservations are there in the overlapping variance-area?
sizeOverLapSkew <- nrow(overlapSkew)
sizeOverLapSkewRatio <- sizeOverLapSkew / nrow(data) # 83%!!!! Much too large...
# --------------------------------------------------------------------------------------------------------------





samp<-sample(1:nrow(data), sampleSize, replace = FALSE, prob = NULL)
samp<-sort(samp)

data_samp<-data[samp,]

#scale all columns to -1..+1
if(useDataScaling)
  data_samp<-scaleData(data_samp, excludeColScaling)

#' Sub-selection'
ordData<-subset(x=data_samp, class %in% plotClass)

# Bring the data in a form that can be plotted with parCoordPlot()
data_m<-prepareData(ordData, colOrdering, 'class')



# Plot the data
parCoordPlot(data_m, useWhiteBackground, xLab, yLab)

parCoordMatrix(ordData, 'class')

