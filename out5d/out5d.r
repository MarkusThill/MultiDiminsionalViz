require(ggplot2)
require(reshape2)
require(PairViz)
require(GGally)
library(scales) # for muted

#load data from csv-file
#df1<-read.csv2(file="data/out5d.txt",header=TRUE, sep="",quote = "")
#df1 <- apply(df1, MARGIN=2, as.character)
#df1 <- apply(df1, MARGIN=2, as.numeric)
#df1 <- data.frame(df1)
#save(df1,file="data.Rda")

source(file = "out5dFunc.r")



# Definitions
# ------------------------------------------------------------------------------------------------------------
useDataScaling <- FALSE
xLab<-1.5
yLab<- 1.5
xText <- 1.5
yText <- 1.5
colOrdering <- c(1:5)
useWhiteBackground <- TRUE
globalAlpha = 0.01;
pngDPI <- 100


if(! exists("df1"))
  load("data.Rda")

summary(df1)


# ==============================================================================
# PLOT 1
#===============================================================================
# Bring the data in a form that can be plotted with parCoordPlot()
# Plot the data: first with alpha = 1.0
data_m<-prepareData(df1, colOrdering, NULL)
#p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.8, xText, yText, 0.1)
#ggsave(paste("../pdf/out5d", "out5d_1.pdf", sep="/"), width = 12.0, height = 8.0);

p<-parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.8, xText, yText, 0.5)
ggsave(paste("../pdf/out5d", "out5d_1.png", sep="/"), width = 12.0, height = 8.0, dpi=pngDPI);

# ==============================================================================
# Plot 2
# ==============================================================================
p <- ggplot(data_m, aes(x=value, colour=variable)) + geom_density(size = 1.0)
p <- p + theme_bw()
p <- p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
p <- p + theme(legend.text= element_text(size = rel(1.5), angle = 0))
p <- p + theme(legend.title= element_text(size = rel(1.2), angle = 0))
#ggsave(paste("../pdf/pollen", "density.pdf",sep="/"), width=12.0, height=8.0);


# ==============================================================================
# Plot 3: Alpha Blending 0.01
# ==============================================================================
data_m <- prepareData(df1, colOrdering, NULL)
#p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText, 0.1)
#ggsave(paste("../pdf/out5d", "out5d_alpha0_01.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText,0.5)
ggsave(paste("../pdf/out5d", "out5d_alpha0_01.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);


# ==============================================================================
# Plot 4: Invert Magnetics axis WITHOUT scaling
# ==============================================================================
df <- df1
df$Mag <- -df$Mag + max(df1$Mag)
#df$Mag <- rescale(df$Mag, to=c(min(df1$Mag), max(df1$Mag)))
#df <- data.frame(apply(df, MARGIN = 2, rescale))
nam <- names(df)
nam[2] <- "Mag (inv)"
names(df) <- nam
data_m <- prepareData(df, colOrdering, NULL)
#p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText, 0.1)
#ggsave(paste("../pdf/out5d", "out5d_invMag.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText, 0.5)
ggsave(paste("../pdf/out5d", "out5d_invMag.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);

# ==============================================================================
# Plot 5: Highlight cluster in the center of the plot around 155
# ==============================================================================
magBorder1 <- 155
magBorder2 <- 170
df <- df1

cl1 <- subset(df, Mag > magBorder1 & Mag < magBorder2)
cl1 <- data.frame(cl1, rep("cluster", nrow(cl1)))
nam <- names(cl1)
nam[ncol(cl1)] <- "class"
names(cl1) <- nam

cl2 <- subset(df, Mag <= magBorder1 | Mag >= magBorder2)
cl2 <- data.frame(cl2, rep("rest", nrow(cl2)))
nam <- names(cl2)
nam[ncol(cl2)] <- "class"
names(cl2) <- nam

ll <- data.frame(rbind(cl2, cl1))
ll$Mag <- -ll$Mag + max(ll$Mag)
nam <- names(ll)
nam[2] <- "Mag (inv)"
names(ll) <- nam

# General Plot
data_m <- prepareData(ll, c(1:6), 'class')
#p <- parCoordPlotClassCol(data_m, useWhiteBackground, xLab, yLab, 0.03, xText, yText, c("#BBCCFF", "black"), c(0.001, 0.5), 0.1)
#p <- p + theme(legend.position="none")
#ggsave(paste("../pdf/out5d", "out5d_Cluster.pdf", sep="/"), width=12.0, height=8.0);

p <- parCoordPlotClassCol(data_m, useWhiteBackground, xLab, yLab, 0.03, xText, yText, c("#BBCCFF", "black"), c(0.001, 0.5), 0.5)
p <- p + theme(legend.position="none")
ggsave(paste("../pdf/out5d", "out5d_Cluster.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);

# How many values are in this cluster
nrow(cl1)

# How many are in a region of the same size but moved a little
df <- df1
clTest <- subset(df, Mag > magBorder1-20 & Mag < magBorder2-20)
nrow(clTest)

# Make random forest for cluster to see, which variables are the most important
#library(randomForest)
#rfData<-cl1
#rf<-randomForest (rfData[,-2],rfData[,2], ntree =500  ,importance=TRUE, proximity=FALSE) 
#rf$importance
#varImpPlot(rf)




# ==============================================================================
# Plot 5.1: Highlight 3 clusters in the center of the plot around 155
# ==============================================================================
magBorder1 <- 155
magBorder2 <- 170
df <- df1

cl1 <- subset(df, Mag > magBorder1 & Mag < magBorder2)
cl1 <- data.frame(cl1, rep("cluster3", nrow(cl1)))
nam <- names(cl1)
nam[ncol(cl1)] <- "class"
names(cl1) <- nam

cl1_1 <- subset(df, Mag > 253)
cl1_1 <- data.frame(cl1_1, rep("cluster1", nrow(cl1_1)))
nam <- names(cl1_1)
nam[ncol(cl1_1)] <- "class"
names(cl1_1) <- nam

cl1_2 <- subset(df, Potas > 253)
cl1_2 <- data.frame(cl1_2, rep("cluster2", nrow(cl1_2)))
nam <- names(cl1_2)
nam[ncol(cl1_2)] <- "class"
names(cl1_2) <- nam

# Rest
cl2 <- subset(df, (Mag <= magBorder1 | Mag >= magBorder2) & Mag <= 253 & Potas <= 253)
cl2 <- data.frame(cl2, rep("cluster0", nrow(cl2)))
nam <- names(cl2)
nam[ncol(cl2)] <- "class"
names(cl2) <- nam

ll <- data.frame(rbind(cl2, cl1_1, cl1_2, cl1))
ll$Mag <- -ll$Mag + max(ll$Mag)
nam <- names(ll)
nam[2] <- "Mag (inv)"
names(ll) <- nam

data_m <- prepareData(ll, c(1:6), 'class')
p <- parCoordPlotClassCol(data_m, useWhiteBackground, xLab, yLab, 0.03, xText, yText, c("#DDDDDD", "#773333" , "#333377", "black"), c(0.1, 0.2, 0.4, 0.55), 0.5)
p <- p + theme(legend.position="none")
ggsave(paste("../pdf/out5d", "out5d_Cluster1.pdf", sep="/"), width=12.0, height=8.0);


# ==============================================================================
# Plot 6: PC-Matrix has two rows. Make two plots
# ==============================================================================
df <- df1
#invert Magnetics
df$Mag <- -df$Mag + max(df1$Mag)
nam <- names(df)
nam[2] <- "Mag (inv)"
names(df) <- nam

len <- ncol(df)
H = hamilDecomp(len)
df <- data.frame(df, df[,ncol(df)])
nam <- names(df)
nam[ncol(df)] <- nam[(ncol(df)-1)] #duplicate Uraniium column
names(df) <- nam

nam <- names(df)
nam[2] <- "Mag (inv)"
names(df) <- nam

dfP<-prepareData(df, H[1,], NULL)
#p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.01, xText, yText,0.1)
#ggsave(paste("../pdf/out5d", "out5d_PCM1.pdf", sep="/"), width=12.0, height=8.0);
p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.01, xText, yText,0.5)
ggsave(paste("../pdf/out5d", "out5d_PCM1.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);

dfP<-prepareData(df, H[2,], NULL)
#p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.01, xText, yText, 0.1)
#ggsave(paste("../pdf/out5d", "out5d_PCM2.pdf", sep="/"), width=12.0, height=8.0);
p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.01, xText, yText, 0.5)
ggsave(paste("../pdf/out5d", "out5d_PCM2.png", sep="/"), width=12.0, height=8.0, dpi=pngDPI);




# # ==============================================================================
# # Scale all axis
# # ==============================================================================
# dfScale <- df1
# dfScale <- data.frame(apply(df1, MARGIN = 2, rescale))
# data_m <- prepareData(dfScale, colOrdering, NULL)
# p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText)
# 
# 
# 
# # ==============================================================================
# # Invert Magnetics axis and scale
# # ==============================================================================
# df <- df1
# df$Mag <- -df$Mag
# df <- data.frame(apply(df, MARGIN = 2, rescale))
# data_m <- prepareData(df, colOrdering, NULL)
# p <- parCoordPlot(data_m, useWhiteBackground, xLab, yLab, 0.03, xText, yText)
# 
# # Matrix
# len <- ncol(df1)
# H = hamilDecomp(len)
# df <- data.frame(df, df[,ncol(df)])
# nam <- names(df)
# nam[ncol(df)] <- nam[(ncol(df)-1)]
# names(df) <- nam
# dfP<-prepareData(df, H[1,], NULL)
# p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.03, xText, yText)
# plot(p)
# 
# dfP<-prepareData(df, H[2,], NULL)
# p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.03, xText, yText)
# plot(p)
# 
# 
# 
# 
# # ==============================================================================
# # SCLAED PC-Matrix has two rows. Make two plots
# # ==============================================================================
# len <- ncol(df1)
# H = hamilDecomp(len)
# df <- dfScale
# df <- data.frame(df, df[,ncol(df)])
# nam <- names(df)
# nam[ncol(df)] <- nam[(ncol(df)-1)]
# names(df) <- nam
# dfP<-prepareData(df, H[1,], NULL)
# p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.01, xText, yText)
# plot(p)
# 
# dfP<-prepareData(df, H[2,], NULL)
# p<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.01, xText, yText)
# plot(p)
# 
# 
# 
# # ==============================================================================
# # High Magnetics
# # ==============================================================================
# len <- ncol(df1)
# H = hamilDecomp(len)
# df <- df1
# df <- data.frame(df, df[,ncol(df)])
# nam <- names(df)
# nam[ncol(df)] <- nam[(ncol(df)-1)]
# names(df) <- nam
# 
# magBorder <- 250
# 
# cl1 <- subset(df, Mag > magBorder)
# cl1 <- data.frame(cl1, rep("high mag.", nrow(cl1)))
# nam <- names(cl1)
# nam[ncol(cl1)] <- "class"
# names(cl1) <- nam
# 
# cl2 <- subset(df, Mag < magBorder)
# cl2 <- data.frame(cl2, rep("low mag.", nrow(cl2)))
# nam <- names(cl2)
# nam[ncol(cl2)] <- "class"
# names(cl2) <- nam
# 
# ll <- data.frame(rbind(cl2, cl1))
# 
# # General Plot
# data_m <- prepareData(ll[,-6], c(1:6), 'class')
# p <- parCoordPlotClass(data_m, useWhiteBackground, xLab, yLab, 0.01, xText, yText)
# plot(p)
# 
# # First plot of matrix
# dfP<-prepareData(ll, c(H[1,], 7) , 'class')
# p<-parCoordPlotClass(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.02, xText, yText)
# plot(p)
# 
# # Second plot of matrix
# dfP<-prepareData(ll, c(H[2,], 7) , 'class')
# p<-parCoordPlotClass(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.02, xText, yText)
# plot(p)
# 
# 
# # ==============================================================================
# # Low magnetics
# # ==============================================================================
# len <- ncol(df1)
# H = hamilDecomp(len)
# df <- df1
# df <- data.frame(df, df[,ncol(df)])
# nam <- names(df)
# nam[ncol(df)] <- nam[(ncol(df)-1)]
# names(df) <- nam
# 
# magBorder <- 100
# 
# cl1 <- subset(df, Mag > magBorder)
# cl1 <- data.frame(cl1, rep("high mag.", nrow(cl1)))
# nam <- names(cl1)
# nam[ncol(cl1)] <- "class"
# names(cl1) <- nam
# # sample bigger subset
# smp <- sample(x=1:nrow(cl1), 1000, replace = FALSE, prob = NULL)
# smp<-sort(smp)
# cl1 <- cl1[smp,]
# 
# cl2 <- subset(df, Mag < magBorder)
# cl2 <- data.frame(cl2, rep("low mag.", nrow(cl2)))
# nam <- names(cl2)
# nam[ncol(cl2)] <- "class"
# names(cl2) <- nam
# 
# ll <- data.frame(rbind(cl1, cl2))
# # First plot of matrix
# dfP<-prepareData(ll, c(H[1,], 7) , 'class')
# p<-parCoordPlotClass(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.02, xText, yText)
# plot(p)
# 
# # Second plot of matrix
# dfP<-prepareData(ll, c(H[2,], 7) , 'class')
# p<-parCoordPlotClass(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.02, xText, yText)
# plot(p)
# 
# 
# # ==============================================================================
# # Low magnetics: Brush Low magnetics
# # ==============================================================================
# len <- ncol(df1)
# H = hamilDecomp(len)
# df <- df1
# df <- data.frame(df, df[,ncol(df)])
# nam <- names(df)
# nam[ncol(df)] <- nam[(ncol(df)-1)]
# names(df) <- nam
# 
# magBorderLow <- 70
# magBorderHigh <- 80
# 
# cl1 <- subset(df, Mag < magBorderLow | Mag > magBorderHigh)
# cl1 <- data.frame(cl1, rep("high mag.", nrow(cl1)))
# nam <- names(cl1)
# nam[ncol(cl1)] <- "class"
# names(cl1) <- nam
# 
# # sample bigger subset
# #smp <- sample(x=1:nrow(cl1), 1000, replace = FALSE, prob = NULL)
# #smp<-sort(smp)
# #cl1 <- cl1[smp,]
# 
# cl2 <- subset(df, Mag > magBorderLow & Mag < magBorderHigh)
# cl2 <- data.frame(cl2, rep("low mag.", nrow(cl2)))
# nam <- names(cl2)
# nam[ncol(cl2)] <- "class"
# names(cl2) <- nam
# 
# ll <- data.frame(rbind(cl1, cl2))
# 
# data_m <- prepareData(ll, c(H[2,], 7), 'class')
# p <- parCoordPlotClassCol(data_m, useWhiteBackground, xLab, yLab, 0.03, xText, yText, c("black", "red"), c(0.01, 0.05))
# 
# # First plot of matrix
# #dfP<-prepareData(ll, c(H[1,], 7) , 'class')
# #p<-parCoordPlotClass(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.02, xText, yText)
# #plot(p)
# 
# # Second plot of matrix
# #dfP<-prepareData(ll, c(H[2,], 7) , 'class')
# #p<-parCoordPlotClass(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, 0.02, xText, yText)
# #plot(p)
# 
# 
# 
# # ==============================================================================
# # Brush Magnetics, three classes
# # Do coloring after inversing Magaxis???
# # ==============================================================================
# df <- df1
# magBorderLow1 <- 77
# magBorderHigh1 <- 92
# 
# cl1 <- subset(df, Mag > magBorderLow1 & Mag < magBorderHigh1)
# cl1 <- data.frame(cl1, rep("low mag.", nrow(cl1)))
# nam <- names(cl1)
# nam[ncol(cl1)] <- "class"
# names(cl1) <- nam
# 
# magBorderHigh2 <- 240
# 
# cl2 <- subset(df, Mag > magBorderHigh2)
# cl2 <- data.frame(cl2, rep("high mag.", nrow(cl2)))
# nam <- names(cl2)
# nam[ncol(cl2)] <- "class"
# names(cl2) <- nam
# 
# # The rest
# cl3 <- subset(df, (Mag <= magBorderHigh2 & Mag >= magBorderHigh1) | Mag <= magBorderLow1)
# cl3 <- data.frame(cl3, rep("rest", nrow(cl3)))
# nam <- names(cl3)
# nam[ncol(cl3)] <- "class"
# names(cl3) <- nam
# 
# ll <- data.frame(rbind(cl3, cl2, cl1))
# 
# # sample bigger subset
# smp <- sample(x=1:nrow(ll), 8000, replace = FALSE, prob = NULL)
# smp<-sort(smp)
# ll <- ll[smp,]
# 
# data_m <- prepareData(ll, c(1:6), 'class')
# p <- parCoordPlotClassCol(data_m, useWhiteBackground, xLab, yLab, 0.03, xText, yText, c("black", gg_color_hue(2)), c(0.0001, 0.001, 0.001))
# p <- p + theme(legend.position="none")
# 
# 
# 
# #library(randomForest)
# #rfData<-df1
# #rf<-randomForest (rfData[,-2],rfData[,2], ntree =500  ,importance=TRUE) 
# #importance<-data.frame(rf$importance) #importance
# #importance[order(importance$MeanDecreaseAccuracy),]
# #load("rf.Rda")
# #rf$importance
