### shouldn't do loops in R!!
.create_gh_R<-function(theta, f)
{
	nx<-length(f)
	p<-theta[1]
	q<-theta[nx+2]
	g<-h<-rep(0,nx)
	i<-1
	j<-nx
	while(i<=nx){
		this.p<-theta[i+1]
		this.q<-theta[j+1]
		if(p==this.p){
			g[i]<-min(g[i-1],f[i])
		}else{
			g[i]<-f[i]
		}
		if(q==this.q){
			h[j]<-min(h[j+1],f[j])
		}else{
			h[j]<-f[j]
		}
		p<-this.p
		q<-this.q
		i<-i+1
		j<-j-1
	}
	return(list(g=g,h=h))
}

.erode_cts_R<-function(x,f,k,verbose=FALSE){
	t1<-proc.time()[3]
	nx<-length(x)
	x.span<-x[nx]-x[1]
	isAppend<-FALSE
	r_min<-rep(0,nx)
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
			f<-c(f,+Inf)
			isAppend<-TRUE
			nx<-nx+1
			x.span<-x[nx]-x[1]
		}
		# create theta vector
		theta<-c(0,findInterval(x,seq(x[1],x[1]+(m-1)*k,by=k)),m+1)
		# create g and h vectors using slow .create_gh_R() function
		g_and_h<-.create_gh_R(theta, f)
		g<-g_and_h$g
		h<-g_and_h$h
		# get the i_{(-k_0)} and i_{(+k_0)} indexes
		i_l<-nx-rev(findInterval(rev(-x),rev(-(x+k0))))+1
		i_r<-findInterval(x+k0,x)
		# r calculation has 3 possibilities
		# case 3: "otherwise" {most will be this}
		r_min<-pmin(h[i_l],g[i_r])
		# case 2: $\theta_{i_{(-k_0)}} = \theta_{i_{(+k_0)}+1}$
		which.low<-which(theta[i_l]==theta[i_r+1])
		r_min[which.low]<-h[i_l[which.low]]
		# case 1: $\theta_{i_{(-k_0)}-1} = \theta_{i_{(+k_0)}}$
		which.hi<-which(theta[i_l+1]==theta[i_r+2])
		r_min[which.hi]<-g[i_r[which.hi]]
		# remove artificially added points if required
		if(isAppend) r_min<-r_min[-nx]
		if(verbose) cat("Completed rolling min/max (cts scale) in"
			,sprintf("%.2f",proc.time()[3]-t1),"seconds \n")
		return(r_min)
	}
}

clsa_min_R<-function(x,f,window) return(.erode_cts_R(x,f,window))
clsa_max_R<-function(x,f,window) return(-.erode_cts_R(x,-f,window))

