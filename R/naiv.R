########################################################################################
# in the cmd shell - to compile
# cd ~/Documents/clsa/src
# R CMD SHLIB naivimp.c
########################################################################################

# cdir<-"~/Documents/clsa/src"
# setwd(cdir)
# dyn.load(paste0("naivimp", .Platform$dynlib.ext))

########################################################################################
############################# wrapper function for C-code ##############################
########################################################################################


.rolling_min_naiv<-function(x,f,k)
{
	n<-length(x)
	r_min<-numeric(n)
	r_min<-.C("rolling_min_naiv_"
		, x, f, as.integer(n), as.numeric(k/2), r_min)[[5]]
	return(r_min)
}


########################################################################################
################################### visible functions ##################################
########################################################################################



naiv_min<-function(x,f,window) return(.rolling_min_naiv(x,f,window))
naiv_max<-function(x,f,window) return(-.rolling_min_naiv(x,-f,window))





