# load necessary packages/install if needed
library(ggplot2); 
library(UsingR); 
library(scales)
# function to plot the histograms

myplotdf <- function(df, titolo ){
  print(titolo)
  myplot(df$xpar,df$ypar,df$mxPar, FALSE, df$xpoints, df$ypoints, titolo)
}

myplot <- function(xpar,ypar,mxPar, myPar, xpoints, ypoints, titolo ){
  # calculate the mean squares

  grp = rep(1:ceiling(length(xpar)/2),2)
  grp <- grp[1:length(xpar)]

  data <- data.frame(x = xpar,y = ypar, grp)

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

  if (!missing(xpoints) & !missing(ypoints)) {
    datapoints <- data.frame(xp = xpoints,yp = ypoints) 
    g <- g + geom_point(data = datapoints, aes(x = xpoints,y = ypoints,lwd=3))
  }
  g <- g + ggtitle(titolo)

  g
}


x1 <- c(rep(c(1,5),5))
y1 <- c(rep(1:5,each=2))


mx4displ <- function(x) {

  y <- 1:length(x)
  ysupp <- c(y,y)

  mx <- mean(x)
  mxvect <- rep(mx,length(x))

  xsupp <- c(x,mxvect)
  retdf <- data.frame(xsupp,mxvect,ysupp,xpoints = x,ypoints = y)
}


x <- c(5:-5)
data <- mx4displ(x)



titolo <- paste("SD2",round(var(x),2),"SD",round(sd(x),2))
print(myplot(data$xsupp,data$ysupp, mxPar = T,myPar = F, data$xpoints, data$ypoints, titolo))
