#ifndef GUARD_PARAMETERS_H
#define GUARD_PARAMETERS_H

#include <string>

#include <RcppEigen.h>

#include "QuadratureRule.h"

#include "Basis.h"

#include "Family.h"

class Parameters
{
 public:
  Parameters();

  // GLMMBelief:
  Eigen::VectorXd GLMMTheta;
  Eigen::VectorXd GLMMBeta;
  Family GLMMFamily;

  // SparseBelief:
  Basis sparseBasis;

  // integration:
  QuadratureRule quadratureRule;
  
};

#endif // GUARD_PARAMETERS_H
