# Author: Markus Thill
# Date: 06.01.2013
# Initial Tests with parallel Coordinates

source(file = "multiplot.r")

scaleData<-function(df, excludedCols, useNegScale) {
  for(i in 1:ncol(df)) {
    if(!(i %in% excludedCols)) {
      minVal= min(df[,i],na.rm = TRUE)
      maxVal= max(df[,i],na.rm = TRUE)
      if(useNegScale) {
        sf<-function(x) {
          m=2.0/(maxVal-minVal)
          b=1-m*maxVal
          return (m*x+b)
        }
      }
      else{
        sf<-function(x) {
          m=1.0/(maxVal-minVal)
          b=1-m*maxVal
          return (m*x+b)
        }
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


parCoordPlotColP<-function(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxLab, minLab, colVec, alphaVec, linesize, axisCol) {
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID, color=V51, alpha = V51), size=linesize)
  p<-p +geom_point(data = axisCol, aes(x = variable, y = value, group = ID, color=V51), cex=12, pch='|', alpha=0.8)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
  
  #Turn off for the moment'
  p <- p + annotate("text", x = 1:8, y = -0.03, label = minLab)
  p <- p + annotate("text", x = 1:8, y = 1.03, label = maxLab)
  #p <- p + scale_colour_gradientn(colours = rainbow(10, start = 0.0, end=0.2))
  
  p <- p +scale_alpha_manual(values=alphaVec, guide = 'none')
  p <-p + scale_colour_manual(values = colVec)
  p <-p +scale_alpha_manual(values=alphaVec, guide = 'none')
  p <- p + theme(legend.position="none") # turn legend off
  return (p)
}


parCoordPlotCol<-function(data_m, useWhiteBackground, xLab, yLab, xText, yText, maxLab, minLab, colVec, alphaVec, linesize) {
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID, color=V51, alpha = V51), size=linesize)
  p<-p +geom_point(aes(x = variable, y = value, group = ID, color=V51), cex=3, pch='|', alpha=0.005)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
  
  #p <- p + scale_colour_gradientn(colours = rainbow(10, start = 0.0, end=0.2))
  
  p <- p +scale_alpha_manual(values=alphaVec, guide = 'none')
  p <-p + scale_colour_manual(values = colVec)
  p <-p +scale_alpha_manual(values=alphaVec, guide = 'none')
  p <- p + theme(legend.position="none") # turn legend off
  return (p)
}

parCoordPlot<-function(data_m, useWhiteBackground, xLab, yLab, alpha, xText, yText, maxLab, minLab, lsize) {
  
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID), size=lsize, alpha = alpha)
  p<-p +geom_point(aes(x = variable, y = value, group = ID), size=1, alpha = alpha/10.0)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
  
  #p <- p + annotate("text", x = 1:12, y = -0.03, label = minLab)
  #p <- p + annotate("text", x = 1:12, y = 1.03, label = maxLab)
  
  # p<- p + scale_colour_gradientn(colours = rainbow(10, start = 0.1))
  # p <-p +scale_alpha_manual(values=c(0.02,0.02), guide = 'none')
  return (p)
}

# parCoordPlot<-function(data_m, useWhiteBackground, xLab, yLab) {
# 
#   p <- ggplot(data_m) + 
#     geom_line(aes(x = variable, y = value, group = ID, color=V51, alpha = V51), size=0.01)
#    p<-p +geom_point(aes(x = variable, y = value, group = ID, color=V51, alpha =  V51), size=1)
#   
#   
#   if(useWhiteBackground)
#     p<-p+theme_bw()
#   
#   p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
#   p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
#   
#   # p<- p + scale_colour_gradientn(colours = rainbow(10, start = 0.1))
#   p <-p +scale_alpha_manual(values=c(0.02,0.02), guide = 'none')
#   p
# }

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

remove_outliers <- function(x, perc=0.01) {
  y<-x
  fac<-2
  # don't remove, if more than perc of the observations are affected
  while(length(y) > perc*length(x)) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
    H <- fac * IQR(x, na.rm =TRUE)
    y <- c(which(x < (qnt[1] - H)), which(x > (qnt[2] + H))) 
    fac = fac * 2
  }
  return (y)
}
