source(file = "miniBooNeFunc.r", chdir=TRUE)



useWhiteBackground <- TRUE

xLab<-1.5
yLab<- 1.5
xText <- 1.5
yText <- 1.5



load(file="classifyPlot.Rda")
load(file="data.Rda")

#dfSE <- subset(df1, V51 == "SE")
#dfBG <- subset(df1, V51 == "BG")

# # select some rows from each class that were classified correctly
# dfSECor <- dfSE[resSE == "SE",]
# dfBGCor <- dfBG[resBG == "BG",]
# 
# numSamp <- 1000
# smplSE <- dfSECor[sample(1:nrow(dfSECor), numSamp, replace=FALSE),]
# smplBG <- dfBGCor[sample(1:nrow(dfBGCor), numSamp, replace=FALSE),]
# 
# # Only show columns that are relevant for classification
# showCols <- c(1,17,25,16,3,2,6,23, 51)
# 
# smplSE <- smplSE[,showCols]
# smplBG <- smplBG[,showCols]
#complete <- rbind(smplSE, smplBG)

# some finetuning
complete <- subset(complete, V1 < 8)
complete <- subset(complete, V1 > -998)
complete <- subset(complete, V3 < 1000)
complete <- subset(complete, V2 < 3)
complete <- subset(complete, V23 < 500)

#save(complete, file="classifyPlot.Rda")

roundLabelDigits <-  3
maxComplete <- round(apply(complete[, -ncol(complete)], MARGIN=2, max), digits = roundLabelDigits)
minComplete <- round(apply(complete[, -ncol(complete)], MARGIN=2, min), digits = roundLabelDigits)




# Scale data      
dfCs <- data.frame(apply(complete[, -ncol(complete)], MARGIN = 2, rescale), complete[, ncol(complete)])
nam <- names(dfCs)
nam[ncol(complete)] <- "V51"
names(dfCs) <- nam





generateAxisRange <- function(class, axis, rangeMax, rangeMin, tolerance, interval) {
  value <- rangeMax - tolerance
  myAxisDf <- data.frame()
  while(value >= rangeMin + tolerance) {
    tmpRow <- data.frame(class, 1, axis, value)
    myAxisDf <- rbind(myAxisDf, tmpRow)
    value <- value - interval
  }
  names(myAxisDf) <- c("V51", "ID","variable","value")
  return (myAxisDf)
}

sf<-function(x) {
  m=1.0/(maxVal-minVal)
  b=1-m*maxVal
  return (m*x+b)
}

myAxDf <- data.frame()
tolerance <- 0.02
interval <- 0.01

# splitV1 <- 4, larger, BG
# splitV1SE <- 3.2, smaller, SE
maxVal <- max(complete$V1)
minVal <- min(complete$V1)
tmp <- generateAxisRange("BG", "V1", 1.0, sf(4.0), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)
tmp <- generateAxisRange("SE", "V1", sf(3.2), 0.0, tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)


# splitV17 <- 0.7, larger, SE
# splitV17BG <- 0.7, smaller, BG
maxVal <- max(complete$V17)
minVal <- min(complete$V17)
tmp <- generateAxisRange("SE", "V17", 1.0, sf(0.7), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)
tmp <- generateAxisRange("BG", "V17", sf(0.7), 0.0, tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)


#splitV25_1 <- 0.2 , splitV25_2 <- 0.45, BG
maxVal <- max(complete$V25)
minVal <- min(complete$V25)
tmp <- generateAxisRange("BG", "V25", sf(0.45), 0.0, tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)

#splitV16 <- 1300, larger, SE
# splitV16BG <- 1000, smaller, BG
maxVal <- max(complete$V16)
minVal <- min(complete$V16)
tmp <- generateAxisRange("BG", "V16", sf(1000), 0.0, tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)
tmp <- generateAxisRange("SE", "V16", 1.0, sf(1300), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)

#splitV3_1 <- 80, splitV3_2 <- 210, BG
maxVal <- max(complete$V3)
minVal <- min(complete$V3)
tmp <- generateAxisRange("BG", "V3", sf(210), sf(80), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)


# V3 SE
#splitV3_1 <- 600 
#splitV3_2 <- 1000
tmp <- generateAxisRange("SE", "V3", sf(980), sf(600), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)


# splitV2 <- 0.6, smaller, SE
maxVal <- max(complete$V2)
minVal <- min(complete$V2)
tmp <- generateAxisRange("SE", "V2", sf(0.6), 0.0, tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)

#splitV2 <- 0.8, larger, BG
tmp <- generateAxisRange("BG", "V2", 1.0, sf(1.0), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)

# splitV6 <- 0.4, larger, SE
maxVal <- max(complete$V6)
minVal <- min(complete$V6)
tmp <- generateAxisRange("SE", "V6", 1.0, sf(0.2), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)

# splitV23 <- 160, larger, SE
maxVal <- max(complete$V23)
minVal <- min(complete$V23)
tmp <- generateAxisRange("SE", "V23", 1.0, sf(160), tolerance, interval)
myAxDf <- rbind(myAxDf,tmp)



#################################################################################################################################
# Plots
#################################################################################################################################
colVec <- c("#9999CC", "#CC7777")
alphaVec <- c(0.03,0.03)
data_m<-prepareData(dfCs, c(1:9), 'V51')
p <- parCoordPlotColP(data_m, TRUE, xLab+1, yLab+1, xText+0.5, yText+0.5, maxComplete, minComplete, colVec, alphaVec, 0.5, myAxDf)
ggsave(paste("", "classify1.pdf", sep=""), width=12.0, height=8.0);

data_m<-prepareData(dfCs[c(701, 1500),], c(1:9), 'V51')
p <- parCoordPlotColP(data_m, TRUE, xLab, yLab, xText, yText, maxComplete, minComplete, colVec, c(1,1), 1.0, myAxDf)
ggsave(paste("", "classify2.pdf", sep=""), width=12.0, height=8.0);
