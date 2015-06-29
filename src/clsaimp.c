#include <R.h> 
#include <Rmath.h>
#include <stdlib.h>

void create_theta_(double *x, double *k, int *n, int *m, int *theta)
{
	double k_=(*k);
	int n_=(*n);
	int m_=(*m);
	int i=1;
	double x_0=x[0];
	int curr_m=1;
	
	theta[0]=0;
	theta[n_+1]=m_+1;
	
	while(i <= n_){
		if ( x[i-1] < (x_0 + (double) curr_m * k_) ) {
			theta[i]=curr_m;
			i++;
		} else {
			curr_m++;
		}
	}
}

double min_d(double x1, double x2){
	return (x1<x2 ? x1 : x2);
}

void create_g_(int *theta, double *f, int *n, double *g)
{
	int n_=(*n);
	int i;
	
	for (i=0; i<n_; i++){
		if(theta[i]<theta[i+1]) g[i]=f[i];
		else g[i]=min_d(f[i],g[i-1]);
	}
}


void create_h_(int *theta, double *f, int *n, double *h)
{
	int n_=(*n);
	int i;
	
	for (i=n_; i>0; i--){
		if(theta[i]<theta[i+1]) h[i-1]=f[i-1];
		else h[i-1]=min_d(f[i-1],h[i]);
	}
}


void create_index_(double *x, int *n, double *k0, int *i_l, int *i_r)
{
	int l_pos = 0;
	int r_pos = 0;
	int i;
	int n_=(*n);
	double k0_=(*k0);
	double this_x;

	for (i=0; i<n_; i++){
		this_x=x[i];
		while (x[l_pos] < (this_x - k0_)) {
			l_pos++;
		}
		while (((this_x + k0_) >= x[r_pos]) & (r_pos < n_)) {
			r_pos++;
		}
		i_l[i]=l_pos;
		i_r[i]=r_pos-1;
	}
}

// theta is 2 longer than g,h
void case_1_2_3_(int *theta, int *i_l, int *i_r, double *g, double *h, int *n, double *r_min)
{
	int n_=(*n);
	
	for (int i=0; i<n_; i++){
		if ( theta[i_l[i]+1]==theta[i_r[i]+2] ) { 		// theta need +1 in index
			r_min[i]=g[i_r[i]]; 						// case #1
		} else if ( theta[i_l[i]]==theta[i_r[i]+1] ) { 	// theta need +1 in index
			r_min[i]=h[i_l[i]]; 						// case #2
		} else {  
			r_min[i]=min_d(g[i_r[i]],h[i_l[i]]); 		// case #3
		}
	}
}


void rolling_min_naiv_(double *x, double *f, int *n, double *k0, double *r_min)
{
	int l_pos = 0;
	int r_pos = 0;
	int i;
	int n_=(*n);
	double k0_=(*k0);
	double this_x;

	for (i=0; i<n_; i++){
		this_x=x[i];
		while (x[l_pos] < (this_x - k0_)) {
			l_pos++;
		}
		while (((this_x + k0_) >= x[r_pos]) & (r_pos < n_)) {
			r_pos++;
		}
		if ( (r_pos-1) == l_pos) {
			r_min[i]=f[l_pos];
		}
		else if ( (r_pos - 1 - l_pos) == 1) {
			r_min[i]=min_d(f[l_pos],f[r_pos-1]);
		}
		else {
			r_min[i]=f[l_pos];
			for (int j=l_pos+1; j<r_pos; j++) r_min[i]=min_d(r_min[i],f[j]);
		}
	}
}










