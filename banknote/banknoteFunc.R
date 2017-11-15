# Author: Markus Thill
# Date: 06.01.2013
# Initial Tests with parallel Coordinates

source(file = "multiplot.r")

scaleData<-function(df, excludedCols) {
  for(i in 1:ncol(df)) {
    if(!(i %in% excludedCols)) {
      minVal= min(df[,i],na.rm = TRUE)
      maxVal= max(df[,i],na.rm = TRUE)
      sf<-function(x) {
        m=2.0/(maxVal-minVal)
        b=1-m*maxVal
        return (m*x+b)
      }
      df[,i]<-sapply(df[,i], sf)
    }
  }
  return (df)
}

prepareData<-function(df, colOrdering, meltClass) {
  # If different Column-Ordering is desired
  df<-changeOrder(df, colOrdering)
  
  # Prepare the data to be plotted
  df$ID <- 1:nrow(df)
  data_m <- melt(df, id.vars=c(meltClass, 'ID'))
  data_m$value <- as.numeric(data_m$value)
  return (data_m)
}

parCoordPlot<-function(data_m, useWhiteBackground, xLab, yLab) {
  
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID, color = class), size=0.1, alpha=0.1)
   p<-p +geom_point(aes(x = variable, y = value, group = ID, color = class), size=1, alpha=0.1)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p
}

changeOrder<-function(df, ordering) {
  #TODO: check if length of ordering is the same as col in df
  tmp<-NULL
  if(ncol(df) == length(ordering))
    tmp<-df[ ,names(df)[ordering]] 
  return (tmp)
}

hamilDecomp<-function(n) {
  if(n %% 2 == 0)
    H<-hpaths(n)
  else{
    H<-hpaths(n-1)
    H<-cbind(rep(n,nrow(H)),H, rep(n,nrow(H)))
  }
  return (H)
}


# TestTest
parCoordMatrix<-function(df, meltClass) {
  len = ncol(df)-length(meltClass)
  H = hamilDecomp(len)
  
  # for data-preperation also the class-variable is needed
  cl<-rep((len+1):ncol(df),nrow(H))
  H<-cbind(H,cl)
  
  p<-list()
  for(i in 1:1:nrow(H)) {
    dfP<-prepareData(df, H[i,], meltClass)
    pl<-parCoordPlot(data_m=dfP, useWhiteBackground=FALSE, xLab=1.3, yLab=1.3)
    p[[length(p)+1]]<-pl
  }
  
  multiplot(plotlist=p, cols=1)
}

