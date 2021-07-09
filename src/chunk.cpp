// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>

//# include <cstdlib>
//# include <iostream>
//# include <iomanip>
#define _USE_MATH_DEFINES
//# include <cmath>
//# include <ctime>

#include <limits> //for quiet nan
#include "khermisc_types.h"

//' @export
// [[Rcpp::export]]
DoubleMat chunkCpp(const DoubleVec & x, const Int & chunkSize, const Int & overlap){

	Int effectiveSize = chunkSize - overlap;

	size_t n = x.size();
	Int m;
	if ( (n % effectiveSize ) == 0) {
		m = n / effectiveSize;
	} else {
		m = n / effectiveSize + 1;
		
	}

	DoubleMat chunks = DoubleMat::Constant(m, chunkSize, std::numeric_limits<float>::quiet_NaN());
	
	Int highestIndex = m*chunkSize - (m-1)*overlap; //do a table to see that upperIndex takes this value in the last iteration

	DoubleVec vec;
	if (highestIndex > n) {
		Int tmpLength = highestIndex-n;
		DoubleVec tmp = DoubleVec::Constant(tmpLength, std::numeric_limits<float>::quiet_NaN());
		vec.resize(highestIndex);
		vec << x, tmp;
	} else {
		vec << x;
	}

	Int i = 1;
	for (Int k = 0; k < m; ++k) {
		//Int upperIndex = i + chunkSize - 1;
		chunks.row(k) = vec.segment(i-1,chunkSize);
		i = i + effectiveSize;
	}

	return(chunks);
}





