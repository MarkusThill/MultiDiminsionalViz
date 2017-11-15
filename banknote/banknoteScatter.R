#' Modified version of the ggplot2 plotmatrix function that accepts additional
#' variables for aesthetic mapping.
#' 
#' example
#data(iris)
#iris.vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#ggpairs(data = iris, facet.vars = iris.vars, 
#mapping = aes(color = Species, shape = Species))

# uses data_samp

data.vars <- c("variance", "skewness", "curtosis", "entropy")
ggpairs(data = data_samp,facet.vars = data.vars
        , mapping = aes(color = class, shape = class))

ggpairs <- function (data, facet.vars = colnames(data), facet.scale = "free",
                     mapping = aes(), size = 1, alpha = 1, shape = 16, color = "black", 
                     density = FALSE, bins = 30) {
  
  suppressMessages(require(ggplot2, quietly = TRUE))
  
  facet.scale <- match.arg(facet.scale, c("free", "fixed"))
  
  if(length(mapping) > 0) {
    aes.vars <- unique(as.character(unlist(mapping)))
    aes.data <- data[, aes.vars]
    
    if(length(aes.vars) == 1) {
      aes.data <- data.frame(aes.data)
      names(aes.data) <- aes.vars
    }
  }
  
  data <- data[, facet.vars]
  
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  
  # data.frame with xy coordinates
  all <- lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol])
  })
  
  # add aes variables to all
  if(length(mapping) > 0) {
    all <- lapply(all, cbind, aes.data)
  }
  
  all <- do.call("rbind", all)
  
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  
  xy.mapping <- aes_string(x = "x", y = "y")
  class(xy.mapping) <- "uneval"
  
  p <- ggplot(all, xy.mapping) + facet_grid(xvar ~ yvar, scales = facet.scale)
  
  if(density) {
    p <- p + stat_binhex(bins = bins, geom = "hex")
  } else {
    
    geom.args <- list(mapping = mapping, stat = "identity", na.rm = TRUE)
    
    # Add manual aesthetic mappings 
    if (!"size" %in% names(mapping)) geom.args$size <- size
    if (!"alpha" %in% names(mapping)) geom.args$alpha <- alpha
    if (!"colour" %in% names(mapping)) geom.args$colour <- color
    
    # Assume geom should be text is mapping includes label
    if("label" %in% names(mapping)) {
      geom.args$geom <- "text"
    } else {
      geom.args$geom <- "point"
      if (!"shape" %in% names(mapping)) geom.args$shape <- shape
    }
    geom <- do.call("layer", geom.args)
    p <- p + geom
  }
  
  # Calculate each variable's kernel density 
  densities <- with(all, simplify = FALSE, 
                    tapply(x, yvar, stats::density, na.rm = TRUE))
  densities <- lapply(densities, function(x) data.frame(x = x$x, y = x$y))
  names(densities) <- levels(all$xvar)
  
  # Melt densities
  densities <- reshape2::melt(densities, id.vars = c("x", "y"))
  names(densities) <- c("x", "y", "xvar")
  densities$xvar <- factor(densities$xvar, levels = levels(all$xvar))
  densities$yvar <- factor(densities$xvar, levels = levels(all$xvar))
  
  # Scale each variable's density estimate to match range of the variable
  densities$scaled <- NA
  for(v in levels(all$xvar)) {
    var.dens <- densities$y[densities$xvar == v]
    
    scaled.dens <- scales::rescale(var.dens, to = range(data[, v], na.rm = TRUE))
    densities$scaled[densities$xvar == v] <- scaled.dens
  }
  
  # Add density line to plot
  p <- p + geom_line(data = densities, aes(x = x, y = scaled), color = "grey20")
  
  return(p)
}