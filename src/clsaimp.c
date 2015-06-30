#include <R.h> 
#include <Rmath.h>
#include <stdlib.h>


double min_d(double x1, double x2){
	return (x1<x2 ? x1 : x2);
}

void create_theta_(int *theta, double *x, int *n, double *k, int *m)
{

	int n_=(*n);
	double k_=(*k);
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


void create_index_(int *i_l, int *i_r, double *x, int *n, double k0)
{
	int n_=(*n);
	int l_pos = 0;
	int r_pos = 0;
	int i;
	double this_x;

	for (i=0; i<n_; i++){
		this_x=x[i];
		while (x[l_pos] < (this_x - k0)) {
			l_pos++;
		}
		while (((this_x + k0) >= x[r_pos]) & (r_pos < n_)) {
			r_pos++;
		}
		i_l[i]=l_pos;
		i_r[i]=r_pos-1;
	}
}

void create_g_(double *g, int *theta, double *f, int *n)
{
	int n_=(*n);
	
	for (int i=0; i<n_; i++){
		if(theta[i]<theta[i+1]) g[i]=f[i];
		else g[i]=min_d(f[i],g[i-1]);
	}
}


void create_h_(double *h, int *theta, double *f, int *n)
{
	int n_=(*n);
	
	for (int i=n_; i>0; i--){
		if(theta[i]<theta[i+1]) h[i-1]=f[i-1];
		else h[i-1]=min_d(f[i-1],h[i]);
	}
}


// theta is 2 longer than g,h
void case_1_2_3_(double *r_min, int *theta, int *i_l, int *i_r, double *g, double *h, int *n)
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

// ::: fun C facts :::
// *xxx is the value stored at the location xxx in memory
// &xxx is the address of a variable xxx
// *(&xxx)==xxx i.e. the LHS is the value of (the address of xxx) == xxx
// also xxx[5] is shorthand for *(xxx + 5)
// i.e. the value in address xxx plus five (the sixth elent of the array starting at xxx)
void get_rmin_(double *r_min, double *x, double *f, int *n, double *k, int *m)
{

	double k0=(*k)/2;

	int theta[*n+2];
	int i_l[*n];
	int i_r[*n];
	double g[*n];
	double h[*n];
	
	create_theta_(theta, x, n, k, m);
	create_index_(i_l, i_r, x, n, k0);
	create_g_(g, theta, f, n);
	create_h_(h, theta, f, n);
	case_1_2_3_(r_min, theta, i_l, i_r, g, h, n);
	
}




