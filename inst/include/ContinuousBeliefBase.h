#ifndef GUARD_CONTINUOUSBELIEFBASE_H
#define GUARD_CONTINUOUSBELIEFBASE_H

#include <vector>

#include <RcppEigen.h>

#include "BeliefBase.h"
#include "Parameters.h"

/// The base class for all ContinuousBeliefs
class ContinuousBeliefBase : public BeliefBase
{
  friend class ContinuousBelief;
 public:
  virtual ~ContinuousBeliefBase();
  ContinuousBeliefBase();
  ContinuousBeliefBase(const std::vector<int>&);
  
  virtual double evaluate(const Eigen::VectorXd&,
			  const Parameters&) = 0;
  virtual Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&,
					     const Parameters&) = 0;
  virtual Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&,
						   const Parameters&) = 0;

};

#endif // GUARD_CONTINUOUSBELIEFBASE_H
