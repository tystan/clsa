# clsa
Fast min/max sliding window algorithm on unevenly spaced data. Impemented for use in `R` (`R` wrapper functions call `C` code for speed).

Details are available in the paper:
[Informed baseline subtraction of proteomic mass spectrometry data aided by a novel sliding window algorithm](https://proteomesci.biomedcentral.com/track/pdf/10.1186/s12953-016-0107-8/). Specifically, Figure 7 contains the details of the CLSA algorithm.

# Installation
To install and load in R, run:
```R
library(devtools) # see http://cran.r-project.org/web/packages/devtools/README.html
devtools::install_github('tystan/clsa')
library(clsa)
### see help file to run example
?clsa_min
```

## Example usage
```R
set.seed(12345)
n <- 100 # number of points
x <- sort(runif(n)) # location of points
f <- rchisq(n,10) # signal at points
this_win <- 0.1 # the size of the window passed over the points
# rolling/moving minimum (erosion in morphology)
mins_f <- clsa_min(x,f,this_win)
# rolling/moving maximum (dilation in morphology)
maxs_f <- clsa_max(x,f,this_win)
# this is a morphological opening
mo_f <- clsa_max(x,mins_f,this_win)
plot(x,f,type="l",bty="n")
lines(x,mins_f,col="navy")
lines(x,maxs_f,col="firebrick4")
lines(x,mo_f,col="forestgreen")
```

![](https://github.com/tystan/clsa/blob/master/example.png)
