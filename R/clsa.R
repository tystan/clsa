.erode_cts<-function(x,f,se.span){
	out.vector<-NULL
	nx<-length(x)
	x.span<-x[nx]-x[1]
	k<-se.span
	t1<-proc.time()[3]
	isAppend<-FALSE
	
	if(k>=x.span){
		cat("Warning: structuring element spans the entire input set \n")
		cat("The input f vector has been output \n")
		return(f)
	}else{
		m<-ceiling(x.span/k)
		mk<-m*k
		if(!((x[1]+mk) == x[nx])){
			x<-c(x,x[1]+mk)
			f<-c(f,+Inf)
			isAppend<-TRUE
			nx<-nx+1
			x.span<-x[nx]-x[1]
		}
		k.blocks<-c(0,findInterval(x,seq(x[1],x[1]+(m-1)*k,by=k)),m+1)
		p<-k.blocks[1]
		q<-k.blocks[nx+2]
		g<-rep(0,nx)
		h<-rep(0,nx)
		r<-rep(0,nx)
		i<-1
		j<-nx
		cat("set up time =",sprintf("%.2f",proc.time()[3]-t1),"seconds \n")
		t2<-proc.time()[3]
		while(i<=nx){
			this.p<-k.blocks[i+1]
			this.q<-k.blocks[j+1]
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
		cat("creating g and h ="
			,sprintf("%.2f",proc.time()[3]-t2),"seconds \n")
		t3<-proc.time()[3]
		k.left<-k/2
		low.bound.index<-nx-rev(findInterval(rev(-x),rev(-(x+k.left))))+1
		upp.bound.index<-findInterval(x+k.left,x)
		cat("creating lower and upper limits ="
			,sprintf("%.2f",proc.time()[3]-t3),"seconds \n")
		t4<-proc.time()[3]
		out.vector<-pmin(h[low.bound.index],g[upp.bound.index])
		which.low<-which(k.blocks[low.bound.index]==k.blocks[upp.bound.index+1])
		out.vector[which.low]<-h[low.bound.index[which.low]]
		which.hi<-which(k.blocks[low.bound.index+1]==k.blocks[upp.bound.index+2])
		out.vector[which.hi]<-g[upp.bound.index[which.hi]]
		cat("if else statements =",sprintf("%.2f",proc.time()[3]-t4),"seconds \n")
		if(isAppend) out.vector<-out.vector[-nx]
		cat("Completed morphological erosion/dilation (cts scale) in"
			,sprintf("%.2f",proc.time()[3]-t1),"seconds \n")
		return(out.vector)
	}
}

clsa_min<-function(x,f,window) return(.erode_cts(x,f,window))
clsa_max<-function(x,f,window) return(-.erode_cts(x,-f,window))

