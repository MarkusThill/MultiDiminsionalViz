classify <- function(df, colNr, split1, split2 = NA, largerSplit1 = TRUE, class) {
  if(is.na(split2)){
    if(largerSplit1)
      indexes <- which(df[,colNr] > split1)
    else
      indexes <- which(df[,colNr] <= split1)
  } else {
    if(largerSplit1)
      indexes <- which(df[,colNr] > split1 & df[,colNr] < split2)
    else
      indexes <- which(df[,colNr] <= split1 | df[,colNr] >= split2)
  }
  
  classifyData <- df[indexes,]
  numSE <- nrow(subset(classifyData, V51 == "SE"))
  numBG <- nrow(subset(classifyData, V51 == "BG"))
  n <- nrow(classifyData)
  if(class == "SE")
    errorRatio <- numBG / n
  else
    errorRatio <- numSE / n
  
  print(paste("numSE:", numSE));
  print(paste("numBG:", numBG));
  print(paste("total:", n))
  print(paste("errorRatio:",errorRatio));
}

mySimpleClassifier <- function(rec) {
  votesBG <- 0;
  votesSE <- 0;
  
  # First, all classifiers with err < 10%
  # +2 for classifiers < 5
  # +4 for classifiers with error < 3
  # V1, BG           
  splitV1 <- 5.33955
  if(rec[1] > splitV1) {votesBG <- votesBG + 1; counter1 <<- counter1 + 1}; #4
  
  # V17, SE                                                                            
  splitV17 <- 1.565552
  if(rec[17] > splitV17) votesSE <- votesSE + 1; #4
  
  # V17, BG                                                                                
  splitV17BG <- 0.35
  if(rec[17] <= splitV17BG) {votesBG <- votesBG + 1; counter17 <<- counter17 + 1} 
  
  # V16 SE                                                                                 
  splitV16 <- 1800
  if(rec[16] > splitV16) votesSE <- votesSE + 1;
  
  # V3, BG                                                                                
  splitV3_1 <- 110
  splitV3_2 <- 160
  if(rec[3] > splitV3_1 & rec[3] <= splitV3_2) {votesBG <- votesBG + 1; counter3 <<- counter3 + 1}
  
  # V25, BG
  splitV25_1 <- 0.2
  splitV25_2 <- 0.45
  if(rec[25] > splitV25_1 & rec[25] <= splitV25_2) {votesBG <- votesBG + 1; counter25 <<- counter25 + 1}; 
  
  # V23 SE                                                                           
  splitV23 <- 210
  if(rec[23] > splitV23) votesSE <- votesSE + 1;
  
  
  diff <- votesBG - votesSE
  classifiedAs <- "NA"
  if(diff > 1) classifiedAs <- "BG";
  if(diff < 0) classifiedAs <- "SE";
  
  if(classifiedAs != "NA")
    return (classifiedAs)
  
  
  
  # V1, BG
  splitV1 <- 4
  if(rec[1] > splitV1) {votesBG <- votesBG + 1; counter1 <<- counter1 + 1}
  
  # V1, SE
  splitV1SE <- 3.2
  if(rec[1] < splitV1SE) votesSE <- votesSE + 1;#2
  
  # V17, SE
  splitV17 <- 0.7#1.565552
  if(rec[17] > splitV17) votesSE <- votesSE + 1;#2
  
  #splitV17BG <- 0.7
  #if(rec[17] <= splitV17BG) {votesBG <- votesBG + 1; counter17 <<- counter17 + 1}
  
  #V16, SE
  splitV16 <- 1300#1800
  if(rec[16] > splitV16) votesSE <- votesSE + 1;#2
  
  #V16, BG
  splitV16BG <- 1000
  if(rec[16] <= splitV16BG) {votesBG <- votesBG + 1; counter16 <<- counter16 + 1}
  
  #V3, BG
  splitV3_1 <- 80   #110
  splitV3_2 <- 210  #160
  if(rec[3] > splitV3_1 & rec[3] <= splitV3_2) {votesBG <- votesBG + 1; counter3 <<- counter3 + 1}
  
  # V3 SE
  splitV3_1 <- 600 
  splitV3_2 <- 1000
  if(rec[3] > splitV3_1 & rec[3] <= splitV3_2) {votesSE <- votesSE + 1}
  
  #V2, SE
  splitV2 <- 0.6
  if(rec[2] <= splitV2) votesSE <- votesSE + 1;#2
  #classify(df1, 2, splitV2, split2 = NA, largerSplit1 = FALSE, "SE")
  
  splitV6 <- 0.4
  if(rec[6] >splitV6) votesSE <- votesSE + 1;#2
  #classify(df1, 6, splitV6, split2 = NA, largerSplit1 = TRUE, "SE")
  
  #V23, SE
  splitV23 <- 160#210
  if(rec[23] > splitV23) votesSE <- votesSE + 1;#2
  
  #V4, SE, large error, 50%
  splitV4_1 <- 0.15
  splitV4_2 <- 0.28
  if(rec[4] > splitV4_1 & rec[4] < splitV4_2) votesSE <- votesSE + 1;
  #classify(df1, 4, splitV4_1, split2 = splitV4_2, largerSplit1 = TRUE, "SE")
  
  diff <- votesBG - votesSE
  classifiedAs <- "NA"
  if(diff > 3) classifiedAs <- "BG";
  if(diff < 0) classifiedAs <- "SE";
  return (classifiedAs)
}