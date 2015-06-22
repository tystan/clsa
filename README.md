# clsa
Fast min/max sliding window algorithm on unevenly spaced data. More explanation to come.

# Installation
To install in R, run:
```R
library(devtools) # see http://cran.r-project.org/web/packages/devtools/README.html
devtools::install_github('tystan/clsa')
### see help file to run example
?clsa_min
### Example usage:
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
```
