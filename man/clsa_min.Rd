\name{clsa_min}
\alias{clsa_min}
\title{Continuous line segment algorithm to compute sliding window minimums}
\description{Fast min/max sliding window algorithm on unevenly spaced data}
\usage{
clsa_min(x,f,window)
\method{clsa_min}{default}(x, y, window=1)
}
\arguments{
\item{x}{ the locations of the points. }
\item{f}{ the signals of the corresponding points in \code{x}. }
\item{window}{ length of the sliding window. }
}
\value{
A vector of the same length as \code{f}, with the sliding window minimums in each element.
}
\author{Ty Stanford}
\examples{
set.seed(12345)
x<-sort(runif(100))
f<-rchisq(100,10)
this_win<-0.1
mins_f <- clsa_min(x,f,this_win)
maxs_f <- clsa_max(x,f,this_win)
# this is a morphological opening
mo_f<-clsa_max(x,mins_f,this_win)
plot(x,f,type="l",bty="n")
lines(x,mins_f,col="navy")
lines(x,maxs_f,col="firebrick4")
lines(x,mo_f,col="forestgreen")
}
\keyword{erode}

