# Beuth University of Applied Sciences, Berlin
# Data Science Master's Program
# Semester: 2
# Course: Data Visualization

#######################################
### Exercise 1: Dimension Reduction ###
#######################################

setwd("~/git-reps/visualization/E01")
par(mfrow=c(2, 3)) # plot the following plots side-by-side

#######################################
### Task 1: Mapping European Cities ###
#######################################

# See also: https://www.r-statistics.com/2016/01/multidimensional-scaling-with-r-from-mastering-data-analysis-with-r/

?eurodist
length(eurodist)
head(eurodist)
summary(eurodist)
as.matrix(eurodist)[1:5, 1:5]
dim(as.matrix(eurodist))

### i. Create mapping positions for the 21 cities, using classical MDS.

mds <- cmdscale(eurodist) # performs multi-dimensional scaling with principal components

### ii. Explore the differences between mapped and original distances (some seem weird!).
dist.ed <- as.matrix(eurodist)
dist.mds <- as.matrix(dist(mds))
dist.diff <- dist.ed - dist.mds

dist.ed[1:5, 1:5]
dist.mds[1:5, 1:5]
dist.diff[1:5, 1:5]

sort(dist.diff[rank(dist.diff) <= 10])
sort(dist.diff[rank(-dist.diff) <= 10], decreasing=TRUE)

# convert to "flat" data frame:
dim (dist.diff)
df <- data.frame(row = rep(1:21, 21), col = rep(1:21, each = 21), x = as.vector(dist.diff))

# order DESC:
dfd <- df[rev(order(df$x, na.last = FALSE)), ]
dfd[1:10, ]

# order ASC:
dfa <- df[order(df$x, na.last = FALSE), ] 
dfa[1:10, ]

### iii. Plot the mapped cities (no plot symbols, city names centered on the plot position) 
### without axis distortion (asp=1).

plot(mds, type = 'n', asp=1)
text(mds[, 1], mds[, 2], row.names(mds))

# alternative plot
# avoid overlaps in city names and suppress x and y axis labels and ticks
#library(ggplot2)
#ggplot(as.data.frame(mds), aes(V1, -V2, label = rownames(mds))) +
#       geom_text(check_overlap = TRUE) + theme_minimal() + xlab('') + ylab('') +
#       scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)

### iv. Compare the map to the actual geography, and obtain a (hand-crafted) rotation and/or reflection 
### such that mapped locations reflect usual map layout as well as possible (similar to previous task).

# Reflection:
mds[, 2] <- mds[, 2] * -1 # *-1 = reflection => exchange north and south

plot(mds, type = 'n', asp=1)
text(mds[, 1], mds[, 2], labels(eurodist), col="blue") 

# Rotation:
angle <- 30/180
rot <- rbind(c(cos(angle), -sin(angle))
          , c(sin(angle), cos(angle))) # clockwise rotation matrix

dim(mds)
dim(rot)

mds.rot <- mds %*% rot # apply rotation matrix transformation

plot(mds.rot, type = 'n', asp=1)
text(mds.rot[, 1], mds.rot[, 2], labels(eurodist), col="navyblue")
text(mds[, 1], mds[, 2], labels(eurodist), col="blue") 

### v. Create an overlay plot:

## Draw geo coordinates of the cities (see file provided in moodle) 
## into a coordinate system with lat 30 to 65, lon -15 to 30.

load("GeoEurocities.RData")

plot(geodat$lat, geodat$lon, xlim=c(30,65), ylim=c(-15,30), asp=1)
text(geodat$lat, geodat$lon, geodat$city, pos=4, cex=0.8)

## Overlay the mapping onto this map.

# For the scaling in v, rescale scaled versions of the mapping (function scale) with 
# the scale information of the geo coordinates that can e.g. be obtained from the 
# attributes “scaled:center” and “scaled:scale” of the result of applying function scale.

mds.scaled = scale(mds, center=TRUE, scale=TRUE) # Malte: "stinknormale Z-Standardisierung"
geodat.scaled = scale(geodat[,10:11], center=TRUE, scale=TRUE)

# plot scaled geodat coordinates:
plot(geodat.scaled, col="seagreen", pch=19, asp=1)
text(geodat.scaled[, 1], geodat.scaled[, 2], labels(eurodist), pos=4, cex=0.8, col="seagreen")

# plot rescaled MDS coordinates:
points(mds.scaled, col="red", pch=19)
text(mds.scaled[, 1], mds.scaled[, 2], labels(eurodist), pos=4, cex=0.8, col="red")

## Draw connecting lines between mapped points and the respective geo location for each mapping.

# The segments function can be used for drawing the connecting lines in v:
segments(mds.scaled[, 1], mds.scaled[, 2], geodat.scaled[, 1], geodat.scaled[, 2], col="grey")

par(mfrow=c(1, 1)) # reset parameter to single plot

#################################################
### Task 2: Method exploration with iris data ###
#################################################

### i. Apply classical MDS to the quantitative variables in the iris data, and interpret the axes. 
### Plot the mapping, and characterize the differences between the three species in terms of the principal axes.

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- dist(iris[,1:4]) # euclidean distances between the rows
fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
str(fit)
summary(fit)

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS", type="n")
text(x, y, labels = iris$Species, cex=.7) 

pcs <- prcomp(iris[,1:4])
par(mfrow=c(1,2))
biplot(pcs, pc.biplot = TRUE, cex=0.8, add=TRUE, xaxt="n", yaxt="n", main="iris data, covariance based", xpd=NA)
## now with correlations
pcs.scaled <- prcomp(iris[,1:4], scale.=TRUE)
biplot(pcs.scaled, pc.biplot = TRUE, cex=0.8, add=TRUE, xaxt="n", yaxt="n", main="iris data, correlation based", xpd=NA)
symbols(0,y=0,circles=1, inches=FALSE, add=TRUE, fg="red", xpd=NA)
par(mfrow=c(1,1))

### Better Solution ###
# copied from very nice tutorial:
# Principal Component Analysis in R: Example with Predictive Model & Biplot Interpretation
# https://www.youtube.com/watch?v=OowGKNgdowA

?iris
str(iris)
summary(iris)

# Scatter Plot & Correlation
library(psych)
pairs.panels(iris[,-5]
             , gap=0
             , bg=c("red","yellow","blue")[iris$species]
             , pch=21)

# Principal Component Analysis
pc <- prcomp(iris[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
pc$center
mean(iris$Sepal.Length)
pc$scale
sd(iris$Sepal.Length)
print(pc) # in this case returns standard devs and rotations (also called loadings) of PCs
# each PC is a normalized linear combination (each vector multiplied by a scalar) of the original variables
# rotations/loadings are the coefficients of the linear combination of the continous variables
# see also: https://www.youtube.com/watch?v=k7RM-ot2NWY&t=11s (Linear combinations, span, and basis vectors | Essence of linear algebra, chapter 2)
summary(pc) # here we can see how much of the variance is covered by each PC

# Orthogonality of PCs
pairs.panels(pc$x
             , gap=0
             , bg=c("red","yellow","blue")[iris$species]
             , pch=21)
# correlation coefficients always zero, b/c PCs are orthogonal to each other

# Bi-Plot
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = iris$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68) # this % of data (variability) will be captured by ellipses
# add more layers to g
g <- g + scale_color_discrete(name = "")
g <- g + theme(legend.direction = "horizontal",
               legend.position = "top")
print(g)

### ii. Apply a SOM to the iris data, and inspect its performance graphically.

# Procedure for SOM creation (lecture notes):
# https://lms.beuth-hochschule.de/pluginfile.php/707289/mod_resource/content/4/DimRedScript.html#41_procedure_for_som_creation
# see also the following video (especially at min 6:46):
# https://www.youtube.com/watch?v=GdZckTLNqsY&t=14s

library(kohonen)
library(RColorBrewer)

old <- cur <- Inf
dat <- scale(iris[,-5])
## iterative improvement of SOM
for (i in 1:100){
  erg <- som(dat, grid=somgrid(6,6,"hexagonal"), rlen=1000)
  cur <- sum(erg$distances)
  if (cur<old){
    erg2 <- erg
    old <- cur
  }
}
plot.somgrid <- function (x, xlim, ylim, ...) 
{
  if (missing(xlim)) 
    xlim <- c(0, max(x$pts[, 1]) + min(x$pts[, 1]))
  if (missing(ylim)) 
    ylim <- c(max(x$pts[, 2]) + min(x$pts[, 2]), 0)
  plot(xlim, ylim, axes = FALSE, type = "n", xlab = "", 
       ylab = "", asp=1, ...)
}
assignInNamespace("plot.somgrid",
                  plot.somgrid, "kohonen")

som2pts <- function(x){
  stopifnot("kohonen" %in% class(x))
  x$grid$pts[x$unit.classif,]
}

som_out <- som2pts(erg2)

mm <- cbind(rep(c(1,2),each=2), rep(3:4, each=2))
widths <- c(3,2); heights <- rep(1,6)
layout(mm, widths=widths, heights=heights)
pal <- function(n) brewer.pal(n, "Set3")
plot(erg2, shape="straight", palette.name=pal)
plot(erg2, shape="straight", type="mapping", col=iris[,5], pch=19)
legend("left", title="Species", levels(iris[,5]), fill=unique(iris[,5]), horiz=FALSE, cex=0.8)
#plot(erg2, shape="straight", type="mapping", cex=0)
#text(som_out + runif(47)*0.5-0.25, labels=substr(iris[,5],1,2), cex=0.8)
plot(erg2, shape="straight", type="dist.neighbours")
plot(erg2, shape="straight", type="quality")

# see also: Second case study: the iris dataset
# https://cran.r-project.org/web/packages/SOMbrero/vignettes/doc-numericSOM.html


