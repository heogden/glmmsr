#include "SparseBelief.h"

using Eigen::VectorXd;
using Eigen::MatrixXd;

SparseBelief::SparseBelief(const std::vector<int>& items,
			   Function f, 
			   VectorFunction fDerivative,
			   MatrixFunction fSecondDerivative,
			   const MultiNormal& normal,
			   const Basis& basis):
  ContinuousBeliefBase(items),
  fStored_(f, fDerivative, fSecondDerivative, normal, basis)
{
}



double SparseBelief::evaluate(const VectorXd& x, 
			      const Parameters& parameters)
{
  return fStored_.evaluate(x, parameters.sparseBasis);
}

VectorXd SparseBelief::evaluateDerivative(const VectorXd& x,
					  const Parameters& parameters)
{
  return fStored_.evaluateDerivative(x, parameters.sparseBasis);
}
					  
MatrixXd SparseBelief::evaluateSecondDerivative(const VectorXd& x,
						const Parameters& parameters)
{
  return fStored_.evaluateSecondDerivative(x, parameters.sparseBasis);
}
