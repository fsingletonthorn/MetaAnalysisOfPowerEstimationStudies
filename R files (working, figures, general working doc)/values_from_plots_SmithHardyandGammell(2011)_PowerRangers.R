##FIND THE CHANGE POINTS IN GRAPH
library(jpeg)
library(grid)
imgMed <- readJPEG("C:/Users/fsingletonthorn/Documents/PhD/Systematic Reviews/History of Power Estimation Studies/R files (working, figures, general working doc)/SmithEtAlPowerRangersImage.JPG")
imgSml <- readJPEG("C:/Users/fsingletonthorn/Documents/PhD/Systematic Reviews/History of Power Estimation Studies/R files (working, figures, general working doc)/SmithEtAlPowerRangersImageSml.JPG")

  
plot.new()
grid::grid.raster(imgMed)

# or for small:
# plot.new()
# grid::grid.raster(imgSml)

calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)
#click two points on the x axis then two points on the y axis
data <- locator(type='p',pch=1,col='red',lwd=1.2,cex=1.2)
#click each cross over point

calibrate = function(calpoints,data,x1,x2,y1,y2)
{
  x  <- calpoints$x[c(1,2)]
  y  <- calpoints$y[c(3,4)]
  cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
  cy <- lm(formula = c(y1,y2) ~ c(y))$coeff
  data$x <- data$x*cx[2]+cx[1]
  data$y <- data$y*cy[2]+cy[1]
  return(as.data.frame(data))
}

calibrate(calpoints, data, x1=1996, x2 = 2009, y1 = 0, y2 = 1)

# Output used for medium: (lowest value, 1st quartile, median, 3rd quartile, max) for 1996 2003 and 2009
#   year     power estiamte
#1  1996.000 0.07544141
#2  1996.040 0.15409310
#3  1996.000 0.22953451
#4  1996.040 0.53611557
#5  1996.040 0.99678973
#6  2002.441 0.07223114
#7  2002.441 0.13643660
#8  2002.480 0.25200642
#9  2002.480 0.51845907
#10 2002.401 1.00000000
#11 2008.960 0.05136437
#12 2009.000 0.14446228
#13 2008.960 0.23916533
#14 2008.960 0.47030498
#15 2008.960 0.99678973#


# Output used for small: (lowest value, 1st quartile, median, 3rd quartile, max) for 1996 2003 and 2009
#   year     power estiamte
#1  1995.961 0.05039370
#2  1996.000 0.06456693
#3  1995.961 0.07244094
#4  1996.000 0.10078740
#5  1996.000 1.00000000
#6  2002.442 0.05039370
#7  2002.442 0.06299213
#8  2002.403 0.07244094
#9  2002.403 0.11496063
#10 2002.442 0.82677165
#11 2008.961 0.05039370
#12 2009.000 0.06456693
#13 2008.961 0.07244094
#14 2008.961 0.10078740
#15 2009.000 0.94173228