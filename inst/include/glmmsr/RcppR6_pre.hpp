// Generated by RcppR6 (0.2.4): do not edit by hand
#ifndef _GLMMSR_RCPPR6_PRE_HPP_
#define _GLMMSR_RCPPR6_PRE_HPP_

#include <RcppCommon.h>


namespace glmmsr {
namespace RcppR6 {
template <typename T> class RcppR6;
}
}



namespace Rcpp {
template <typename T> SEXP wrap(const glmmsr::RcppR6::RcppR6<T>&);
namespace traits {
template <typename T> class Exporter<glmmsr::RcppR6::RcppR6<T> >;
}

template <> SEXP wrap(const std::vector<MixedContinuousBelief>&);
template <> std::vector<MixedContinuousBelief> as(SEXP);
template <> SEXP wrap(const Parameters&);
template <> Parameters as(SEXP);
template <> SEXP wrap(const ClusterGraph&);
template <> ClusterGraph as(SEXP);
}

#endif
