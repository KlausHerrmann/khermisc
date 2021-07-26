// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>

//# include <cstdlib>
//# include <iostream>
//# include <iomanip>
#define _USE_MATH_DEFINES
//# include <cmath>
//# include <ctime>

#include "khermisc_types.h"

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
DoubleVec rowMaxCpp(const DoubleMat & x){
	DoubleVec maxVal = x.rowwise().maxCoeff();
	return(maxVal);
}

