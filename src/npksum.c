#include <R.h>
#include <Rmath.h>
#include <float.h>

#ifndef MY_OMP_H_
#define MY_OMP_H_

#ifdef _OPENMP
#include <omp.h>
#if _OPENMP >= 201307
#define OMP_VER_4
#endif
#endif

// Insert SIMD pragma if supported
#ifdef OMP_VER_4
#define SAFE_SIMD _Pragma("omp simd")
#define SAFE_FOR_SIMD _Pragma("omp for simd")
#else
#define SAFE_SIMD
#define SAFE_FOR_SIMD
#endif

#endif


double pnormstd(double x)
{
  return 0.5 * erfc(-x * M_SQRT1_2);
}

double dnormstd(double x)
{
  // double one_over_sqrt_two_pi = 0.39894228040143267794;
  // return one_over_sqrt_two_pi *exp(-0.5*pow(x,2.0));
  return 0.5 * M_2_SQRTPI * M_SQRT1_2 * exp(- x * x * 0.5);
}

void pnormstdC(double *x)
{
  *x = 0.5 * erfc(-*x * M_SQRT1_2);
}

void dnormstdC(double *x)
{
  *x = 0.5 * M_2_SQRTPI * M_SQRT1_2 * exp(- *x * *x * 0.5);
}


void npksumXnew(
    int *Nthreds,
    double *xdat, double *xeval, double *bw,
    int *xtype, int *nlevels, int *n, int *neval, int *q,
    double *tym2){

  double tym1, Kix, difftym, tym;

  // http://jakascorner.com/blog/2016/06/omp-for-reduction.html about `reduction`
  #pragma omp parallel for num_threads(*Nthreds) private(tym1, Kix, difftym, tym) //reduction(*: Kix) reduction(+: tym1)
  for(int j = 0; j < *neval; j++){
    // Rprintf("j = %2i \n", j );
    tym1 = 0;
    for (int i = 0; i < *n; i++){
      Kix = 1.0; // will be Y[i] for npksumYX
      for (int k = 0; k < *q; k++){
        // cat("j = ",j,"k = ",k,"\n")
        difftym = xeval[(k-0) * *neval + j] - xdat[(k-0) * *n + i];
        if ( xtype[k] == 3 ) { // "ordered"
          if( difftym == 0.0 ) {
            tym = 1.0;
          } else {
            tym = pow(bw[k], fabs(difftym));
          }
        } // end if (xtype[k] == 3)
        if (xtype[k] == 2 ) { // "factor"
          if( difftym == 0.0 ) {
            tym = 1.0 - bw[k];
          } else {
            tym = bw[k] / nlevels[k];
          }
        } // end if (xtype[k] == 2)
        if ( xtype[k] == 1) {
          tym = dnormstd( difftym / bw[k]) / bw[k];
        } // end if (xtype[k] == 1)
        // if ( j == 1 & i == 1){
        //   Rprintf("j = %2i, k = %2i,(k-0) * *neval + j = %2i, (k-0) * *n + i = %2i, difftym = %4.8e, tym = %4.8e \n", j, k, (k-0) * *neval + j, (k-0) * *n + i, difftym, tym );
        // }
        Kix *= tym; // product [kernel] of the row 'i' which has q elements of the matrix 'n x q'
      } // end for (k in 1:q)
      // tym2[j] += Kix;
      tym1 += Kix; // sum of products of all 'n' rows
    } //end for (i in 1:myn)
    tym2[j] = tym1;
  }

}

void npksumX(
    int *Nthreds,
    double *xdat, double *bw,
    int *xtype, int *nlevels, int *n, int *q,
    double *tym2){

  double tym1, Kix, difftym, tym;

  #pragma omp parallel for num_threads(*Nthreds) private(tym1, Kix, difftym, tym)
  for(int j = 0; j < *n; j++){
    tym1 = 0;
    for (int i = 0; i < *n; i++){
      Kix = 1.0;
      for (int k = 0; k < *q; k++){
        difftym = xdat[(k-0) * *n + j] - xdat[(k-0) * *n + i];
        if ( xtype[k] == 3 ) { // "ordered"
          if( difftym == 0.0 ) {
            tym = 1.0;
          } else {
            tym = pow(bw[k], fabs(difftym));
          }
        } // end if (xtype[k] == 3)
        if (xtype[k] == 2 ) { // "factor"
          if( difftym == 0.0 ) {
            tym = 1.0 - bw[k];
          } else {
            tym = bw[k] / nlevels[k];
          }
        } // end if (xtype[k] == 2)
        if ( xtype[k] == 1) {
          tym = dnormstd( difftym / bw[k]) / bw[k];
        } // end if (xtype[k] == 1)
        Kix *= tym;
      } // end for (k in 1:q)
      tym1 += Kix;
    } //end for (i in 1:myn)
    tym2[j] = tym1;
  }
}

void npksumYXnew(
    int *Nthreds,
    double *ydat, double *xdat, double *xeval, double *bw,
    int *xtype, int *nlevels, int *n, int *neval, int *q,
    double *tym2){

  double tym1, Kix, difftym, tym;

  #pragma omp parallel for num_threads(*Nthreds) private(tym1, Kix, difftym, tym)
  for(int j = 0; j < *neval; j++){
    tym1 = 0;
    for (int i = 0; i < *n; i++){
      Kix = ydat[i]; // will be Y[i] for npksumYX
      for (int k = 0; k < *q; k++){
        // cat("j = ",j,"k = ",k,"\n")
        difftym = xeval[(k-0) * *neval + j] - xdat[(k-0) * *n + i];
        if ( xtype[k] == 3 ) { // "ordered"
          if( difftym == 0 ) {
            tym = 1.0;
          } else {
            tym = pow(bw[k], fabs(difftym));
          }
        } // end if (xtype[k] == 3)
        if (xtype[k] == 2 ) { // "factor"
          if( difftym == 0 ) {
            tym = 1.0 - bw[k];
          } else {
            tym = bw[k] / nlevels[k];
          }
        } // end if (xtype[k] == 2)
        if ( xtype[k] == 1) {
          tym = dnormstd( difftym / bw[k]) / bw[k];
        } // end if (xtype[k] == 1)
        Kix *= tym;
        // if ( j == 5 & i == 0){
        //   Rprintf("i = %2i, k = %2i, difftym = %4.8e, tym = %4.8e, tym = %4.8e \n", i, k, difftym, tym, Kix );
        // }
      } // end for (k in 1:q)
      tym1 += Kix;
    } //end for (i in 1:myn)
    tym2[j] = tym1;
  }
}

void npksumYX(
    int *Nthreds,
    double *ydat, double *xdat, double *bw,
    int *xtype, int *nlevels, int *n, int *q,
    double *tym2){

  double tym1, Kix, difftym, tym;

  #pragma omp parallel for num_threads(*Nthreds) private(tym1, Kix, difftym, tym)
  for(int j = 0; j < *n; j++){
    tym1 = 0;
    for (int i = 0; i < *n; i++){
      Kix = ydat[i];
      for (int k = 0; k < *q; k++){
        difftym = xdat[(k-0) * *n + j] - xdat[(k-0) * *n + i];
        if ( xtype[k] == 3 ) { // "ordered"
          if( difftym == 0.0 ) {
            tym = 1.0;
          } else {
            tym = pow(bw[k], fabs(difftym));
          }
        } // end if (xtype[k] == 3)
        if (xtype[k] == 2 ) { // "factor"
          if( difftym == 0.0 ) {
            tym = 1.0 - bw[k];
          } else {
            tym = bw[k] / nlevels[k];
          }
        } // end if (xtype[k] == 2)
        if ( xtype[k] == 1) {
          tym = dnormstd( difftym / bw[k]) / bw[k];
        } // end if (xtype[k] == 1)
        Kix *= tym;
      } // end for (k in 1:q)
      tym1 += Kix;
    } //end for (i in 1:myn)
    tym2[j] = tym1;
  }
}

void npksumYXloo(
    int *Nthreds,
    double *ydat, double *xdat, double *bw,
    int *xtype, int *nlevels, int *n, int *q,
    double *tym2){

  double tym1, Kix, difftym, tym;

#pragma omp parallel for num_threads(*Nthreds) private(tym1, Kix, difftym, tym)
  for(int j = 0; j < *n; j++){
    tym1 = 0;
    for (int i = 0; i < *n; i++){
      if (i != j){
        Kix = ydat[i];
        for (int k = 0; k < *q; k++){
          difftym = xdat[(k-0) * *n + j] - xdat[(k-0) * *n + i];
          if ( xtype[k] == 3 ) { // "ordered"
            if( difftym == 0.0 ) {
              tym = 1.0;
            } else {
              tym = pow(bw[k], fabs(difftym));
            }
          } // end if (xtype[k] == 3)
          if (xtype[k] == 2 ) { // "factor"
            if( difftym == 0.0 ) {
              tym = 1.0 - bw[k];
            } else {
              tym = bw[k] / nlevels[k];
            }
          } // end if (xtype[k] == 2)
          if ( xtype[k] == 1) {
            tym = dnormstd( difftym / bw[k]) / bw[k];
          } // end if (xtype[k] == 1)
          Kix *= tym;
        } // end for (k in 1:q)
        tym1 += Kix;
      } // end if (i != j)
    } //end for (i in 1:myn)
    tym2[j] = tym1;
  }
}
