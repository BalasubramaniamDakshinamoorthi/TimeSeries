plotts.wge <-function(x, yLabel = "", gg = 0, title = "Realization", xLabel = "Time") 
{
#
#
#     x is a vector of length n containing the time series realization which is to be plotted 
#        as x(1), x(2), ..., x(n) where n=length(x)
#
cex.labs <- c(.9,.8,.9)
#
numrows <- 1
numcols <- 1
par(mfrow=c(numrows,numcols),mar=c(3.8,2.5,1,1))
#
n=length(x)
t=1:n
#
#Plot Data
#
#

if(gg == 0) # Regular tswge
{
  if(class(x) != "ts")
  {
    if(n <=200) {plot(t,x,type='o',cex=0.5,pch=16,cex.lab=.75,cex.axis=.75,lwd=.75,xlab=xLabel,ylab=yLabel, col = "blue", main = title)}
    else if(n > 200) {plot(t,x,type='l',cex=0.5,pch=16,cex.lab=.75,cex.axis=.75,lwd=.75,xlab=xLabel,ylab=yLabel, col = "blue", main = title)}
  }
  if(class(x) == "ts")
  {
    if(n <=200) {plot(x,type='o',cex=0.5,pch=16,cex.lab=.75,cex.axis=.75,lwd=.75,xlab=xLabel,ylab=yLabel, col = "blue", main = title)}
   else if(n > 200) {plot(x,type='l',cex=0.5,pch=16,cex.lab=.75,cex.axis=.75,lwd=.75,xlab=xLabel,ylab=yLabel, col = "blue", main = title)}
  }
}
else if(gg == 1) # GGPLOT Version
{
 
    if(class(x) != "ts")
    {

  df = data.frame(x = t, y = x)
  df %>% ggplot(aes(x = x, y = y)) + geom_point() + geom_line() + ggtitle(title) + xlab(xLabel) + ylab(yLabel)
      } 
  else if(class(x) == "ts")
  {
    df = data.frame(x = zoo::as.yearmon(time(x)), y = x)
    df %>% ggplot(aes(x = x, y = y)) + geom_point() + geom_line() + ggtitle(title) + xlab(xLabel) + ylab(yLabel)
  } 
  
}

#if(n <=200) {plot(t,x,type='o',xaxt='n',yaxt='n',cex=0.5,pch=16,cex.lab=.75,cex.axis=.75,lwd=.75,xlab='Time',ylab=yLabel, col = "blue", main = "Realization")}
#if(n > 200) {plot(t,x,type='l',xaxt='n',yaxt='n',cex=0.5,pch=16,cex.lab=.75,cex.axis=.75,lwd=.75,xlab='Time',ylab=yLabel, col = "blue", main = "Realization")}
#axis(side=1,cex.axis=.9,mgp=c(3,0.15,0),tcl=-.3);
#axis(side=2,las=1,cex.axis=.9,mgp=c(3,.4,0),tcl=-.3)
#mtext(side=c(1,2,1),cex=cex.labs,text=c('Time','','Realization'),line=c(1,1.1,2.1))
#                 }
#     
}
