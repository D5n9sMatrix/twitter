#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/

// plogr_demo <- Rcpp::cppFunction(depends = "plogr", '
                                  // C++ code begin
#include <plogr.h>
                                  
RObject plogr_demo() {
plog::init_r(plog::info);
LOG_INFO << "shown";
LOG_DEBUG << "not shown";
plog::init_r("DEBUG");
LOG_DEBUG << "shown now";
return R_NilValue;
}