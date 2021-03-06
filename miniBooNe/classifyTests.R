source(file = "classifyFunc.r")

load(file="data.Rda")

######################################################################################################################
# V1, BG                                                                                 
######################################################################################################################
splitV1 <- 4#5.33955
classify(df1, 1, splitV1, split2 = NA, largerSplit1 = TRUE, "BG")

######################################################################################################################
# V1, SE (16% error)                                                                                
######################################################################################################################
df <- df1[apply(df1[, -51], MARGIN = 1, function(x) all(x > -998)), ]
maxV1 <- 17.0573
minV1 <- 0
splitV1SE <- 3.2 #0.1*(maxV1-minV1)+minV1
classify(df, 1, splitV1SE, split2 = NA, largerSplit1 = FALSE, "SE")


######################################################################################################################
# V17 SE (important for SE classification)                                                                                
######################################################################################################################
splitV17 <- 0.7#1.565552
classify(df1, 17, splitV17, split2 = NA, largerSplit1 = TRUE, "SE")

######################################################################################################################
# V17 BG                                                                                
######################################################################################################################
splitV17BG <- 0.7#0.35
classify(df1, 17, splitV17BG, split2 = NA, largerSplit1 = FALSE, "BG")


######################################################################################################################
# V16 SE                                                                                 
######################################################################################################################
splitV16 <- 1300#1800
classify(df1, 16, splitV16, split2 = NA, largerSplit1 = TRUE, "SE")

######################################################################################################################
# V16 BG (15% error)                                                                               
######################################################################################################################
splitV16BG <- 1000
classify(df1, 16, splitV16BG, split2 = NA, largerSplit1 = FALSE, "BG")

######################################################################################################################
# V3                                                                                
######################################################################################################################
splitV3_1 <- 80 #110
splitV3_2 <- 210#160
classify(df1, 3, splitV3_1, split2 = splitV3_2, largerSplit1 = TRUE, "BG")

######################################################################################################################
# V3 SE #unbrauchbar                                                                              
######################################################################################################################
splitV3_1 <- 600 #110
splitV3_2 <- 1000#160
classify(df1, 3, splitV3_1, split2 = splitV3_2, largerSplit1 = TRUE, "SE")


######################################################################################################################
# V25                                                                                
######################################################################################################################
splitV25_1 <- 0.0#0.2
splitV25_2 <- 0.45#0.45 #0.45 seems good as well
classify(df1, 25, splitV25_1, split2 = splitV25_2, largerSplit1 = TRUE, "BG")


######################################################################################################################
# V2 (11%)                                                                                
######################################################################################################################
splitV2 <- 0.6
classify(df1, 2, splitV2, split2 = NA, largerSplit1 = FALSE, "SE")

splitV2 <- 0.8
classify(df1, 2, splitV2, split2 = NA, largerSplit1 = TRUE, "BG")


######################################################################################################################
# V6  (14%)                                                                            
######################################################################################################################
splitV6 <- 0.4
classify(df1, 6, splitV6, split2 = NA, largerSplit1 = TRUE, "SE")


######################################################################################################################
# V23 SE                                                                           
######################################################################################################################
splitV23 <- 160#210
classify(df1, 23, splitV23, split2 = NA, largerSplit1 = TRUE, "SE")

######################################################################################################################
# V4 SE                                                                           
######################################################################################################################
splitV4_1 <- 0.15#210
splitV4_2 <- 0.28#210
classify(df1, 4, splitV4_1, split2 = splitV4_2, largerSplit1 = TRUE, "SE")






counter1 <-0
counter16 <- 0
counter17 <- 0
counter3 <- 0
counter25 <- 0

dfSE <- subset(df1, V51 == "SE")
#res <- apply(dfSE, MARGIN=1, mySimpleClassifier)

resSE <- rep(-1, nrow(dfSE))
for(i in 1:nrow(dfSE)) {
  resSE[i] <- mySimpleClassifier(dfSE[i,])
}

length(resSE[resSE == "SE"])
length(resSE[resSE == "BG"])
length(resSE[resSE == "NA"])

#> length(resSE[resSE == "SE"])
#[1] 22849
#> length(resSE[resSE == "BG"])
#[1] 3132
#> length(resSE[resSE == "NA"])
#[1] 10518


dfBG <- subset(df1, V51 == "BG")
#res <- apply(dfBG, MARGIN=1, mySimpleClassifier)

resBG <- rep(-1, nrow(dfBG))
for(i in 1:nrow(dfBG)) {
  resBG[i] <- mySimpleClassifier(dfBG[i,])
}
length(resBG[resBG == "BG"])
length(resBG[resBG == "SE"])
length(resBG[resBG == "NA"])

#> length(resBG[resBG == "BG"])
#[1] 68600
#> length(resBG[resBG == "SE"])
#[1] 5771
#> length(resBG[resBG == "NA"])
#[1] 19194



# #parCoordMatrix(dfMat, 'V51')

#1,4,32,3,^17,16,25,27,2,13,18
#ff <- rbind(dfSE, dfBG[1:10000,])
#library(randomForest)
#rfData<-ff
#rfSE<-randomForest (rfData[,-51],rfData[,51], ntree =500  ,importance=TRUE) 
#save(rfSE, file="rfSE.Rda")

#smp <- sample(1:nrow(df1), 10000, replace=FALSE)
#rfBG<-randomForest (df1[smp,-51],df1[smp,51], ntree = 500  ,importance=TRUE) 
#save(rfBG, file="rfBG.Rda")