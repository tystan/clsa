########################################################################################
# in the cmd shell - to compile
# cd ~/Documents/clsa/src
# R CMD SHLIB clsaimp.c
########################################################################################

# cdir<-"~/Documents/clsa/src"
# setwd(cdir)
# dyn.load(paste0("clsaimp", .Platform$dynlib.ext))


########################################################################################
############################# wrapper functions for C-code #############################
########################################################################################


.get_rmin<-function(x,f,n,k,m)
{
	return(
		.C("get_rmin_"
			, numeric(n) # r_min
			, as.numeric(x)
			, as.numeric(f)
			, as.integer(n)
			, as.numeric(k)
			, as.integer(m)
		)[[1]]
	)
}


########################################################################################
######################################## main() ########################################
########################################################################################

.erode_cts<-function(x,f,k,verbose=FALSE){
	t1<-proc.time()[3]
	nx<-length(x)
	x.span<-x[nx]-x[1]
	isAppend<-FALSE
	
	if(k>=x.span){
		cat("Warning: structuring element spans the entire input set \n")
		cat("The input f vector has been output \n")
		return(f)
	}else{
		m<-ceiling(x.span/k)
		mk<-m*k
		# make span(X) == m*k artificially
		if(!((x[1]+mk) == x[nx])){
			x<-c(x,x[1]+mk)
			f<-c(f,1e+100) # +Inf not allowed for c-code, nasty workaround
			isAppend<-TRUE
			nx<-nx+1
		}
		#### workhorse function ####
		r_min<-.get_rmin(x,f,nx,k,m)
		############################
		if(isAppend) r_min<-r_min[-nx]
		if(verbose) cat("Completed rolling min/max (cts scale) in"
			,sprintf("%.2f",proc.time()[3]-t1),"seconds \n")
		return(r_min)
	}
}




########################################################################################
################################### visible functions ##################################
########################################################################################

clsa_min<-function(x,f,window) return(.erode_cts(x,f,window))
clsa_max<-function(x,f,window) return(-.erode_cts(x,-f,window))






