
// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-



// [[Rcpp::depends(RcppEigen)]]

//neue Version wie in SBS2

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>
#define _USE_MATH_DEFINES


/////////////////////////////////////////////
// FILE NAME HAS TO BE packageName_types.h !!
//https://stackoverflow.com/questions/42884946/compilation-error-using-rcpp-with-typedef
/////////////////////////////////////////////


//#include <iostream>
//#include <Eigen/Dense>


typedef double Double; //data type for double - long double slows down by 5 times

typedef long int Int; //data type for integers (long int because simulations can be larger than 32.000 - otherwise the calculations go wrong compared to R

/// @brief Dynamic vector of Double variables (from the previous typedef).
typedef Eigen::Matrix< Double, Eigen::Dynamic, 1              > DoubleVec;

/// @brief Dynamic matrix of Double variables (from the previous typedef).
typedef Eigen::Matrix< Double, Eigen::Dynamic, Eigen::Dynamic > DoubleMat;

typedef Eigen::Matrix< int, Eigen::Dynamic, 1              > intVec;




