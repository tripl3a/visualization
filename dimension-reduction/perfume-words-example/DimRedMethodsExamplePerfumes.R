## obtain words
hks51 <- "#006a73"
require(SensoMineR)
data("perfume")
source("extractWordsPerfumesSensominer.R")
## cW is a list with elements 
##      data (0/1 for each word / perfume / panelist combination,
##            a row for each word)
##      and sumdata (total count for each word / perfume combination) 
## prepare data to not contain duplicates (because otherwise most methods don't work)

## overall data (only 12 columns per word, one for each perfume)
## remove duplicates
  tothilfdata <- unique(cW$sumdata)
  dim(tothilfdata)  ## looses 24 duplicates

## classical MDS
Y <- cmdscale(dist(tothilfdata))
plot(Y, axes=F, cex=0.1, main="Word map using Euclidean distance, classical MDS",
     xlab="",ylab="")
text(Y, rownames(tothilfdata), col=hks51, 
     cex=sqrt(log10(rowSums(tothilfdata))+0.5), xpd=NA)

## classical MDS with canberra distance
Y <- cmdscale(dist(tothilfdata, method="canberra"))
plot.default(Y, axes=F, cex=0.1, main="Word map using Canberra distance, classical MDS, Canberra distance",
     xlab="",ylab="")
text(Y, rownames(tothilfdata), col=hks51, 
     cex=sqrt(log10(rowSums(tothilfdata))+0.5), xpd=NA)

### Rtsne
require(Rtsne)
totrtsne_out <- Rtsne(tothilfdata, perplexity=15, 
              verbose=TRUE, max_iter=20000)
plot(totrtsne_out$Y, axes=F, cex=0.1, main="Word map using Euclidean distance, Rtsne",
     xlab="",ylab="")
text(totrtsne_out$Y, rownames(tothilfdata), col=hks51, 
     cex=sqrt(log10(rowSums(tothilfdata))+0.5), xpd=NA)

## SOM visualization
require(kohonen)
xdim <- 10
ydim <- 6
cur <- old <- Inf
system.time({for (i in 1:100){
hilf <- som(tothilfdata, 
                               somgrid(xdim=xdim, ydim=ydim, topo="hexagonal"), rlen=1000)
  cur <- sum(hilf$distances)
  if (cur<old){
    kohonen_out <- hilf
    old <- cur
  }
  print(old)
}})
sum(kohonen_out$distances)

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
mm <- rbind(c(1,2), c(1,3))
## relative heights of the three layout rows
heights <- c(1,1)
## relative widths of the two layout columns
widths <- c(3,2)
pal <- function(n) c("green1", "blue1", "orange1", "blue2", "green2", "cyan", "orange2", "blue3","orange3","green3")
layout(mm, widths=widths, heights=heights)
plot(kohonen_out, type="mapping", labels=rownames(tothilfdata), cex=sqrt(log10(rowSums(tothilfdata))+0.5), 
     shape="straight", 
     main="Word map, SOM, random position within cell", 
     col=hks51)
plot(kohonen_out, shape="straight", type="dist.neighbours")
plot(kohonen_out, shape="straight", type="quality")

## 3D visualization instead of 2D
options(viewer=NULL)
require(threejs)
Y3 <- cmdscale(dist(tothilfdata, method="canberra"),k=3)
scatterplot3js(Y3, pch=rownames(tothilfdata), 
               num.ticks = c(0, 0, 0), 
               size=unname(log10(rowSums(tothilfdata))/10)+0.1, 
               grid=FALSE, col=hks51)

totrtsne_out3 <- Rtsne(tothilfdata, dim=3, perplexity=15, verbose=TRUE, max_iter=5000)
scatterplot3js(totrtsne_out3$Y, pch=rownames(tothilfdata), 
               num.ticks = c(0, 0, 0), size=unname(log10(rowSums(tothilfdata))/10)+0.1, 
               grid=FALSE, col=hks51)

