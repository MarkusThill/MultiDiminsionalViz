# Author: Markus Thill
# Date: 06.01.2013
# Initial Tests with parallel Coordinates

source(file = "multiplot.r")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}


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

parCoordPlot<-function(data_m, useWhiteBackground, xLab, yLab, alpha, xText, yText, lsize=0.1) {

  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID), size=lsize, alpha = alpha)
   p<-p +geom_point(aes(x = variable, y = value, group = ID), size=1, alpha = alpha/10.0)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
  
  # p<- p + scale_colour_gradientn(colours = rainbow(10, start = 0.1))
  # p <-p +scale_alpha_manual(values=c(0.02,0.02), guide = 'none')
  return (p)
}

parCoordPlotClass<-function(data_m, useWhiteBackground, xLab, yLab, alpha, xText, yText) {
  
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID, color = class), size=0.1, alpha = alpha)
  p<-p +geom_point(aes(x = variable, y = value, group = ID, color = class), size=1, alpha = alpha/10.0)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
  
  # p<- p + scale_colour_gradientn(colours = rainbow(10, start = 0.1))
  # p <-p +scale_alpha_manual(values=c(0.02,0.02), guide = 'none')
  return (p)
}

parCoordPlotClassCol<-function(data_m, useWhiteBackground, xLab, yLab, alpha, xText, yText, colVec, alphaVec, lsize=0.1) {
  
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID, color = class), size=lsize, alpha = alpha)
  p<-p +geom_point(aes(x = variable, y = value, group = ID, color = class), size=1, alpha = alpha/10.0)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
  
  
  p <-p + scale_colour_manual(values = colVec)
  p <-p +scale_alpha_manual(values=alphaVec, guide = 'none')
  return (p)
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
  #cl<-rep((len+1):ncol(df),nrow(H))
  #H<-cbind(H,cl)
  if(len %% 2 == 1) {
    df <- data.frame(df, df[,ncol(df)])
    nam <- names(df)
    nam[ncol(df)] <- nam[(ncol(df)-1)]
    names(df) <- nam
  }
  p<-list()
  for(i in 1:nrow(H)) {
    dfP<-prepareData(df, H[i,], meltClass)
    pl<-parCoordPlot(data_m=dfP, useWhiteBackground, xLab=xLab, yLab=yLab, globalAlpha, xText, yText)
    p[[length(p)+1]]<-pl
    plot(pl)
  }
  
  ##multiplot(plotlist=p, cols=1)
  return (p)
}

remove_outliers <- function(x) {
  y<-x
  fac<-4
  # don't remove, if more than 3% of the observations are affected
  while(length(y) > 0.02*length(x)) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
    H <- fac * IQR(x, na.rm =TRUE)
    y <- c(which(x < (qnt[1] - H)), which(x > (qnt[2] + H))) 
    fac = fac * 2
  }
  return (y)
}
