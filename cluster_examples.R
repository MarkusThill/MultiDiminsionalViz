data(iris)
library(plyr)
library(ggplot2)
iris_s <- scale(iris[,-5]) 
k_def <- many_kmeans(iris_s)
k_10 <- many_kmeans(iris_s, 2:10)
k_rep <- many_kmeans(iris_s, rep(4, 5))
h_def <- all_hclust(iris_s)
h_10 <- all_hclust(iris_s, 2:10)
h_5 <- all_hclust(iris_s, seq(2, 20, by = 4))

pr <- princomp(iris_s)
pr1 <- predict(pr)[, 1]
pr2 <- predict(pr)[, 2]

plot(clustergram(k_def, pr1))
plot(clustergram(k_rep, pr1))
plot(clustergram(k_rep, pr2))