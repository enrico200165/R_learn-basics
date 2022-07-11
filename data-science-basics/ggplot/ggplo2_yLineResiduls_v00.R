# load necessary packages/install if needed
library(ggplot2); 
library(UsingR); 
library(scales)
# function to plot the histograms

myplotdf <- function(df, titolo ){
  print(titolo)
  #myplot(df$xpar,df$ypar,df$mxPar, FALSE, df$xpoints, df$ypoints, titolo)
}


myplot <- function(xpar,ypar,mxPar, myPar, xpoints, ypoints, titolo ){
  # calculate the mean squares

  grp = rep(1:ceiling(length(xpar)/2),2)
  grp <- grp[1:length(xpar)]

  data <- data.frame(x = xpar,y = ypar, grp)
  print(cbind(ypar,grp))
  g <- ggplot(data)

  if (!missing(mxPar) && mxPar == TRUE) {
    mx <- mean(xpar)
    g <- g + geom_vline(aes(xintercept = mx), lwd=2, col = 2)
  } else {
    mx = "-"
  }
  if (!missing(myPar) && myPar == TRUE) {
    my <- mean(ypar)
    g <- g + geom_hline(aes(yintercept = my), lwd=2, col = 4)
  } else {
    my = "-"
  }
  
  g <- g + geom_line(aes(x = x, y= y, group = grp))

  if (!missing(xpoints) & !missing(ypoints) 
      & !is.null(xpoints) &  !is.null(xpoints)) {
    datapoints <- data.frame(xp = xpoints,yp = ypoints) 
    g <- g + geom_point(data = datapoints, aes(x = xpoints,y = ypoints,lwd=3))
  }
  g <- g + ggtitle(titolo)

  g
}


x1 <- c(rep(c(1,5),5))
y1 <- c(rep(1:5,each=2))


#  retdf <- data.frame(xsupp,mxvect,ysupp,xpoints = x,ypoints = y)


my4displ <- function(y) {
  
  x <- 1:length(y)
  xsupp <- c(x,x)
  # print(xsupp)

  my <- mean(y)
  myvect <- rep(my,length(y))
  ploty <- c(y,myvect)
  print(ploty)
  
  retdf <- data.frame(plotx = xsupp,ploty = ploty,
                      xpoints = x, ypoints = y)
}




y <- c(15:5)
data <- my4displ(y)



titolo <- paste("SD2",round(var(y),2),"SD",round(sd(y),2))
print(myplot(data$plotx,data$ploty, mxPar = F,myPar = T, data$xpoints, data$ypoints, titolo))
