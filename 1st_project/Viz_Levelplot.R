

## Example data
x <- seq(1,8, length.out=8)
y <- seq(1,8, length.out=8)
data <- expand.grid(X=x, Y=y)
data$Z <- runif(64, 0, 100)

# Levelplot with ggplot2
library(ggplot2)
ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + theme_bw()

# To change the color of the gradation :
ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + 
  theme_bw() + 
  scale_fill_gradientn(colours = brewer.pal(5, "OrRd"), 
                       values=c(0,0.01,0.02,0.1,0.49,1)) # Delta E classes

setwd("~/git-reps/visualization/1st_project/")

# LabMeasurements‐Color‐Card.csv
#   contains the measurements for 42*13 color cards (n=546)
#     13 of the large sheets have been sampled from production
#   column headers:
#     Row, Column (target position on large sheet of color cards)
#       row numbers from 1 to 7
#       column numbers from 1 to 6      
#     L11, a11, b11, …, L88, a88, b88 Lab‐values for color spots (1,1) to (8,8)
#       The first digit reflects the card’s row, the second the card’s column.
measures <- read.csv2("LabMeasurements-Color-Card.csv")
dim(measures)
str(measures)
table(measures[,1:2])

master <- read.csv2("MasterColorCard.csv")
dim(master)
head(master)

# RESHAPE DATA (pivot measures)
data = data.frame("row"=c(),"col"=c(),"L"=c(),"a"=c(),"b"=c(), "L.master"=c(),"a.master"=c(),"b.master"=c())
for(row in 1:8){
  for(col in 1:8){
    # get master-colour
    index = which(master$Crow==row & master$Ccol==col)
    data_master = master[index,c("L","a","b")]
    
    # reshape measures-dataframe and calculate difference to master-colour
    data = rbind(data,
                 rbind(
                   cbind(row,
                         col,
                         measures[[paste("L",row,col,sep="",collapse="")]],
                         measures[[paste("a",row,col,sep="",collapse="")]],
                         measures[[paste("b",row,col,sep="",collapse="")]],
                         data_master$L, 
                         data_master$a, 
                         data_master$b
                  )
                 )
                )
  }
}
colnames(data) = c("row","col","L","a","b","L.master","a.master","b.master")
#data$value = as.numeric(as.vector(data$value))
data$deltaE = sqrt((data$L-data$L.master)^2 + (data$a-data$a.master)^2 + (data$b-data$b.master)^2)

#------------------------------------------------

idx = matrix(0,34944,3)
i = 1
for (pos in 1:64){
  for (target.col in 1:6){
    for (target.row in 1:7){
      for (sheet.no in 1:13){
        idx[i,1]<-sheet.no
        idx[i,2]<-target.row
        idx[i,3]<-target.col
        i=i+1
      }
    }
  }
}

table(idx[,1])
table(idx[,2],idx[,3])
table(idx[,1],idx[,2])
table(idx[,1],idx[,3])

idx = data.frame(idx)
colnames(idx) = c("sheet.no","target.row","target.col")
data = cbind(idx,data)
data[1:5,]

table(data[,1],data[,4])
table(data[,1],data[,5])
table(data[,4],data[,5])

