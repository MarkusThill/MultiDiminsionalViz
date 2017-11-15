require(ggplot2)
require(reshape2)
require(PairViz)
require(GGally)
library(scales) # for muted

xLab <- 2.5
yLab <- 2.5

xText <- 3.0
yText <- 3.0

pointSize <- 5

plotWidth <- 9

parCoordPlot<-function(data_m, useWhiteBackground, xLab, yLab, alpha, xText, yText) {
  
  p <- ggplot(data_m) + 
    geom_line(aes(x = variable, y = value, group = ID), size=0.01, alpha =  alpha)
  # p<-p +geom_point(aes(x = variable, y = value, group = ID), size=1, alpha =  0.1)
  
  
  if(useWhiteBackground)
    p<-p+theme_bw()
  
  p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
  p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
  p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
  p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))


  # p<- p + scale_colour_gradientn(colours = rainbow(10, start = 0.1))
  #p <-p +scale_alpha_manual(values=c(0.02,0.02), guide = 'none')
  return (p)
}

dev.off()

# Circle
# -----------------------------------------------------
n <- 20
x1 <- (-n:n)/n
x2_1 <- sqrt(1 - x1^2)
x2_2 <- -sqrt(1 - x1^2)
     
df <- data.frame(c(x1, x1), c(x2_1, x2_2))
df$ID <- 1:nrow(df)
names(df) <- c("x1", "x2", "ID")
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 0.7, xText, yText)
ggsave(paste("../pdf/structures", "circle_pc.pdf",sep="/"),width=plotWidth,height=8);
#plot(p)
#dev.off()

persp3d(x1, x2_1, x2_1, theta=50, phi=25, expand=0.75, col=color, ticktype="detailed",
        xlab="dd", ylab="dd", zlab="dd", 
        axes=TRUE)


names(df) <- c("x1", "x2", "ID")
p <- ggplot(df[,-3])
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize, alpha =  0.6)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "circle_xy.pdf",sep="/"),width=plotWidth,height=8);
#dev.off()

# Cluster
# -----------------------------------------------------
n<- 300

x1_1 <- rnorm(n, mean = 0, sd = 0.5)+0.5
x1_2 <- rnorm(n, mean = 2, sd = 0.5)
x2_1 <- rnorm(n, mean = 0, sd = 0.3)
x2_2 <- rnorm(n, mean = 2, sd = 0.5)+0.5
df <- data.frame(c(x1_1, x1_2), c(x2_1, x2_2))
df$ID <- 1:nrow(df)
names(df) <- c("x1", "x2", "ID")
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 0.2, xText, yText)
ggsave(paste("../pdf/structures", "cluster_pc.pdf",sep="/"),width=plotWidth,height=8);
#dev.off()

p <- ggplot(df[,-3])
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize/2, alpha =  0.4)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "cluster_xy.pdf",sep="/"),width=plotWidth,height=8);



# Normal Distribution
# ------------------------------------------------------------------------------------------------------------------------
n<- 10000

x1 <- rnorm(n, mean = 0, sd = 1.0)
x2 <- rnorm(n, mean = 0, sd = 1.0)

df <- data.frame(x1, x2)
summary(df)


df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 0.05, xText, yText)
ggsave(paste("../pdf/structures", "rnorm_pc.pdf",sep="/"),width=plotWidth,height=8);

p <- ggplot(df[,-3])
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize/2, alpha =  0.5)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "rnorm_xy.pdf",sep="/"),width=plotWidth,height=8);

     
# Straight line (y = x)
# -----------------------------------------------------------------------------------------------------------------------------
n <- 20
x1 <- (0:n)/n
x2 <- x1
df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 1.0, xText, yText)
ggsave(paste("../pdf/structures", "lineM1_pc.pdf",sep="/"),width=plotWidth,height=8);

p <- ggplot(df[,-3])
p <- p + geom_line(aes(x = x1, y = x2), size=0.3, alpha =  0.7)
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize, alpha =  0.5)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "lineM1_xy.pdf",sep="/"),width=plotWidth,height=8);


# Straight line (y = 0)
# -----------------------------------------------------------------------------------------------------------------------------
n <- 20
m <- 0.0
x1 <- (0:n)/n
x2 <- m*x1
df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 1.0, xText, yText)
ggsave(paste("../pdf/structures", "line_pc.pdf",sep="/"),width=plotWidth,height=8);

p <- ggplot(df[,-3])
p <- p + geom_line(aes(x = x1, y = x2), size=0.3, alpha =  0.7)
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize, alpha =  0.5)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "line_xy.pdf",sep="/"),width=plotWidth,height=8);



# Straight line (y = m*x)
# -----------------------------------------------------------------------------------------------------------------------------
#n <- 20
#m <- 0.3
#x1 <- (0:n)/n
#x2 <- m*x1
#df <- data.frame(x1, x2)
#df$ID <- 1:nrow(df)
#data_m <- melt(df, id.vars=c('ID'))
#data_m$value <- as.numeric(data_m$value)
#parCoordPlot(data_m, TRUE, 1.3, 1.3, 1.0)


# Straight line (y = m*x, m->infinity)
# -----------------------------------------------------------------------------------------------------------------------------
#n <- 20
#m <- 99999999999999999
#x1 <- (0:n)/n
#x2 <- m*x1
#df <- data.frame(x1, x2)
#df$ID <- 1:nrow(df)
#data_m <- melt(df, id.vars=c('ID'))
#data_m$value <- as.numeric(data_m$value)
#parCoordPlot(data_m, TRUE, 1.3, 1.3, 1.0)


# Straight line (y = x + b)
# -----------------------------------------------------------------------------------------------------------------------------
n <- 20
b <- 0.3
x1 <- (0:n)/n
x2 <- x1 + b
df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 1.0, xText, yText)
ggsave(paste("../pdf/structures", "lineM1b_pc.pdf",sep="/"),width=plotWidth,height=8);

p <- ggplot(df[,-3])
p <- p + geom_line(aes(x = x1, y = x2), size=0.3, alpha =  0.7)
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize, alpha =  0.5)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "lineM1b_xy.pdf",sep="/"),width=plotWidth,height=8);


# Straight line (y = m*x + b)
# -----------------------------------------------------------------------------------------------------------------------------
n <- 20
m <- 0.3
b <- 0.3
x1 <- (0:n)/n
x2 <- m*x1 + b
df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 1.0, xText, yText)
ggsave(paste("../pdf/structures", "lineMxb_pc.pdf",sep="/"),width=plotWidth,height=8);

p <- ggplot(df[,-3])
p <- p + geom_line(aes(x = x1, y = x2), size=0.3, alpha =  0.7)
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize, alpha =  0.5)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "lineMxb_xy.pdf",sep="/"),width=plotWidth,height=8);


# Straight line (y = -m*x + b)
# -----------------------------------------------------------------------------------------------------------------------------
n <- 20
m <- 1
b <- 1.2
x1 <- (0:n)/n
x2 <- -m*x1 + b
df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 1.0, xText, yText)
ggsave(paste("../pdf/structures", "lineNegMxb_pc.pdf",sep="/"),width=plotWidth,height=8);

p <- ggplot(df[,-3])
p <- p + geom_line(aes(x = x1, y = x2), size=0.3, alpha =  0.7)
p <- p + geom_point(aes(x = x1, y = x2), size=pointSize, alpha =  0.5)
p<-p+theme_bw()
p<-p + theme(axis.title.y = element_text(size = rel(yLab), angle = 90))
p <- p + theme(axis.text.y = element_text(size = rel(yText), angle = 0))
p<-p+theme(axis.title.x = element_text(size = rel(xLab), angle = 0))
p <- p + theme(axis.text.x = element_text(size = rel(xText), angle = 0))
ggsave(paste("../pdf/structures", "lineNegMxb_xy.pdf",sep="/"),width=plotWidth,height=8);


# x, e^x
# -----------------------------------------------------------------------------------------------------------------------------
n <- 2000
x1 <- (-n:n)/n
x2 <- exp(x1)
df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 0.01, xText, yText)


# lognormal distribution
# -----------------------------------------------------------------------------------------------------------------------------
n <- 10000
x2 <- rlnorm(n, meanlog = 0, sdlog = 0.6)
x1 <- rnorm(n, mean = 0, sd = 0.6)
x1 <- rbeta(n, 2, 4, ncp = 0.3)
#x1 <- rescale(rlnorm(10*n, meanlog = 1, sdlog = 0.4) + 0.5*rnorm(n, mean=0, sd=1))


df <- data.frame(x1, x2)
df$ID <- 1:nrow(df)
data_m <- melt(df, id.vars=c('ID'))
data_m$value <- as.numeric(data_m$value)
p<-parCoordPlot(data_m, TRUE, xLab, yLab, 0.1, xText, yText)
ggsave(paste("../pdf/structures", "tmp.pdf",sep="/"),width=plotWidth,height=8);

#x2 <- 12*rbeta(n, 4, 200, ncp = 0.1)
df <- data.frame(x2)
p <- ggplot(df, aes(x=x2)) + geom_density(size = 1.0)
p

dev.off()