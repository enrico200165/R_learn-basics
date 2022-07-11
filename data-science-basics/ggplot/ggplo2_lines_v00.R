# load necessary packages/install if needed
library(ggplot2); 
library(UsingR); 
library(scales)
# function to plot the histograms

myplot <- function(xpar,ypar,mxPar){
  # calculate the mean squares

  grp = rep(1:ceiling(length(xpar)/2),each=2)
  grp <- grp[1:length(xpar)]
  # print(grp)
  data <- data.frame(xpar,ypar, grp)
    # plot histogram
  g <- ggplot(data)

  if (!missing(mxPar) && mxPar == TRUE) {
    mx <- mean(xpar)
    g <- g + geom_vline(aes(xintercept = mx), lwd=2, col = 2)
  } else {
    mx = "-"
  }
  
  g <- g + geom_line(aes(x = xpar, y= ypar, group = grp))
  g <- g + ggtitle(paste("mu = ", mx, ", MSE = ",999, sep = ""))

  g
}


x1 <- c(rep(c(1,5),5))
y1 <- c(rep(1:5,each=2))


mx4displ <- function(x) {

  ysupp <- 1:length(x)
  ysupp <- c(ysupp,ysupp)

  mx <- mean(x)
  mxvect <- rep(mx,length(x))
  x <- c(x,mxvect)
  retdf <- data.frame(x,mxvect,ysupp)
}


x <- rep(c(-2,2),3)
data <- mx4displ(x)
print(data)




# manipulate allows the user to change the variable mu to see how the mean squares changes
#   library(manipulate); manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))]
# plot the correct graph
print(myplot(data$x,data$ysupp, mx = FALSE))