// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// auc_partial_cpp
double auc_partial_cpp(NumericVector time, NumericVector dv, NumericVector range);
RcppExport SEXP _mrgmisc_auc_partial_cpp(SEXP timeSEXP, SEXP dvSEXP, SEXP rangeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dv(dvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type range(rangeSEXP);
    rcpp_result_gen = Rcpp::wrap(auc_partial_cpp(time, dv, range));
    return rcpp_result_gen;
END_RCPP
}
// min_through
NumericVector min_through(NumericVector x);
RcppExport SEXP _mrgmisc_min_through(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(min_through(x));
    return rcpp_result_gen;
END_RCPP
}
// max_through
NumericVector max_through(NumericVector x);
RcppExport SEXP _mrgmisc_max_through(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(max_through(x));
    return rcpp_result_gen;
END_RCPP
}
// padLeft
std::vector< std::string > padLeft(std::vector< std::string > strings, int numChars, std::string paddingChar);
RcppExport SEXP _mrgmisc_padLeft(SEXP stringsSEXP, SEXP numCharsSEXP, SEXP paddingCharSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::string > >::type strings(stringsSEXP);
    Rcpp::traits::input_parameter< int >::type numChars(numCharsSEXP);
    Rcpp::traits::input_parameter< std::string >::type paddingChar(paddingCharSEXP);
    rcpp_result_gen = Rcpp::wrap(padLeft(strings, numChars, paddingChar));
    return rcpp_result_gen;
END_RCPP
}
// set_bins_cpp
IntegerVector set_bins_cpp(NumericVector x, NumericVector left, NumericVector right);
RcppExport SEXP _mrgmisc_set_bins_cpp(SEXP xSEXP, SEXP leftSEXP, SEXP rightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type left(leftSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type right(rightSEXP);
    rcpp_result_gen = Rcpp::wrap(set_bins_cpp(x, left, right));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mrgmisc_auc_partial_cpp", (DL_FUNC) &_mrgmisc_auc_partial_cpp, 3},
    {"_mrgmisc_min_through", (DL_FUNC) &_mrgmisc_min_through, 1},
    {"_mrgmisc_max_through", (DL_FUNC) &_mrgmisc_max_through, 1},
    {"_mrgmisc_padLeft", (DL_FUNC) &_mrgmisc_padLeft, 3},
    {"_mrgmisc_set_bins_cpp", (DL_FUNC) &_mrgmisc_set_bins_cpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_mrgmisc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
