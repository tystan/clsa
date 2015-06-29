########################################################################################
# in the cmd shell - to compile
# cd ~/Documents/clsa/src
# R CMD SHLIB clsaimp.c
########################################################################################

cdir<-"~/Documents/clsa/src"
setwd(cdir)
dyn.load(paste0("clsaimp", .Platform$dynlib.ext))


########################################################################################
############################# wrapper functions for C-code #############################
########################################################################################

.create_theta<-function(x,k,m,n)
{
	theta<-integer(n+2)
	return(
		.C("create_theta_"
			, x, k, as.integer(n), as.integer(m), theta)[[5]]
	)
}
.create_index<-function(x,k0,n)
{
	i_l<-i_r<-integer(n)
	i_l_r<-.C("create_index_"
		, x, as.integer(n), k0, i_l, i_r)[4:5]
	names(i_l_r)<-c("left","right")
	return(i_l_r)
}
.create_g<-function(theta,f,n)
{
	g<-numeric(n)
	return(
		.C("create_g_"
			, as.integer(theta), f, as.integer(n), g)[[4]]
	)
}
.create_h<-function(theta,f,n)
{
	h<-numeric(n)
	return(
		.C("create_h_"
			, as.integer(theta), f, as.integer(n), h)[[4]]
	)
}
.case_1_2_3<-function(r_min,theta,i_l,i_r,g,h,n)
{
	return(
		.C("case_1_2_3_"
			, theta, i_l, i_r, g, h, as.integer(n), r_min)[[7]]
	)
}

.rolling_min_naiv<-function(x,f,k)
{
	n<-length(x)
	r_min<-numeric(n)
	r_min<-.C("rolling_min_naiv_"
		, x, f, as.integer(n), as.numeric(k/2), r_min)[[5]]
	return(r_min)
}

########################################################################################
###################################### workhorse ######################################
########################################################################################

.erode_cts<-function(x,f,k,verbose=FALSE){
	t1<-proc.time()[3]
	nx<-length(x)
	x.span<-x[nx]-x[1]
	isAppend<-FALSE
	k0<-k/2
	
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
# 			x.span<-x[nx]-x[1]
		}
		# create theta vector
		theta<-.create_theta(x,k,m,nx)
		# get the i_{(-k_0)} and i_{(+k_0)} indexes
		i_l_r<-.create_index(x,k0,nx)
		# create g and h vectors 
		g<-.create_g(theta,f,nx)
		h<-.create_h(theta,f,nx)
		# r calculation has 3 possibilities
		# case 1: $\theta_{i_{(-k_0)}-1} = \theta_{i_{(+k_0)}}$
		# case 2: $\theta_{i_{(-k_0)}} = \theta_{i_{(+k_0)}+1}$
		# case 3: "otherwise" {most will be this}
		r_min<-.case_1_2_3(rep(0,nx)
				,theta,i_l_r$left,i_l_r$right,g,h,nx)
################################### R implementation ###################################
# 		i_l<-i_l_r$left+1
# 		i_r<-i_l_r$right+1
# 		# r calculation has 3 possibilities
# 		# case 3: "otherwise" {most will be this}
# 		r_min<-pmin(h[i_l],g[i_r])
# 		# case 2: $\theta_{i_{(-k_0)}} = \theta_{i_{(+k_0)}+1}$
# 		which.low<-which(theta[i_l]==theta[i_r+1])
# 		r_min[which.low]<-h[i_l[which.low]]
# 		# case 1: $\theta_{i_{(-k_0)}-1} = \theta_{i_{(+k_0)}}$
# 		which.hi<-which(theta[i_l+1]==theta[i_r+2])
# 		r_min[which.hi]<-g[i_r[which.hi]]
########################################################################################
		# remove artificially added points if required
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

naiv_min<-function(x,f,window) return(.rolling_min_naiv(x,f,window))
naiv_max<-function(x,f,window) return(-.rolling_min_naiv(x,-f,window))






