data(iris)
require(ggplot2)
require(reshape2)
iris$ID <- 1:nrow(iris)
iris_m <- melt(iris, id.vars=c('Species', 'ID'))

#png(filename="foo.png", width=10000, height=10000,  res=800)
ggplot(iris_m) + 
  geom_line(aes(x = variable, y = value, group = ID, color = Species), size=1)

#dev.off()