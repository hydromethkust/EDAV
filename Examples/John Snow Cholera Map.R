## Recreate the John Snow plot

library(HistData)
data(Snow.deaths)
data(Snow.pumps) 
data(Snow.streets)
data(Snow.polygons)

options(family='mono')
# First to plot the "deaths" points
plot(Snow.deaths[,c("x","y")], col='red', pch=15, cex=0.6, 
       xlab="", ylab="", xlim=c(3,20), ylim=c(3,20),
       main="Snow's Cholera Map of London",family='mono')

# Second to plot and label the pumps
points(Snow.pumps[,c("x","y")], col='green', pch=17, cex=1.5)
text(Snow.pumps[,c("x","y")], labels=Snow.pumps$label, pos=1, cex=0.8,family='mono')

# Third to draw the streets network
slist = split(Snow.streets[,c("x","y")],
              as.factor(Snow.streets[,"street"]))
invisible(lapply(slist, lines, col='grey50'))

# add a scale for the map
scale = matrix(c(0,0, 4,0, NA, NA), 
               nrow=3, ncol=2, byrow=TRUE)
colnames(scale)= c("x","y")

# tick marks
scale = rbind(scale, expand.grid(y=c(-.1, .1, NA), x=0:4)[,2:1])
lines(3.5+scale[,1], 20+scale[,2])
# value and axis labels
stext = matrix(c(0,0, 2,0, 4,0, 4, 0.1), nrow=4, ncol=2, byrow=TRUE)
text(3.5+stext[,1], 20+stext[,2], 
     labels=c("0", "2", "4", "100 m."), 
     pos=c(1,1,1,4), cex=0.8,family='mono')

## draw the Thiessen polygon which include 
## all cholera deaths nearer to a given pump than to any other
## This helps you to identify which pump was the main source
starts = which(Snow.polygons$start==0)
for(i in 1:length(starts)) {
  this = starts[i]:(starts[i]+1)
  lines(Snow.polygons[this,2:3], col="blue", lwd=2, lty=2)
}

## Optional: Add a density contour for the deaths
library(KernSmooth)
kde2d = bkde2D(Snow.deaths[,2:3], bandwidth=c(0.5,0.5))
contour(x=kde2d$x1, y=kde2d$x2,z=kde2d$fhat, add=TRUE)
