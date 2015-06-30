#include <R.h> 
#include <Rmath.h>
#include <stdlib.h>

// function prototypes
double min_d(double x1, double x2);

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

