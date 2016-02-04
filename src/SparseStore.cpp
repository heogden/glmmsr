#include "SparseStore.h"

SparseStore::SparseStore(Function f,  
			 VectorFunction fDerivative,
			 MatrixFunction fSecondDerivative, 
			 const Eigen::VectorXd& location,
			 const Eigen::MatrixXd& precision, 
			 const Basis& basis):
  modifier_(location.size()), 
  fPlusModifierStored_(location.size(), basis)
{
  MultiNormal normal(location, precision);
  initializeFromNormal(normal, 
		       f, fDerivative, fSecondDerivative,
		       basis);
}

SparseStore::SparseStore(Function f,  
			 VectorFunction fDerivative,
			 MatrixFunction fSecondDerivative, 
			 const MultiNormal& normal,
			 const Basis& basis):
  modifier_(normal.dimension()),
  fPlusModifierStored_(normal.dimension(), basis)
{
  initializeFromNormal(normal, 
		       f, fDerivative, fSecondDerivative,
		       basis);
}


double SparseStore::evaluate(const Eigen::VectorXd& x, const Basis& basis) const
{
  double ret = fPlusModifierStored_.interpolate(x, basis);
  ret -= modifier_(x);
  return ret;
}

Eigen::VectorXd SparseStore::evaluateDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  Eigen::VectorXd ret = fPlusModifierStored_.interpolateDerivative(x, basis);
  ret -= modifier_.derivative(x);
  return ret;
}

Eigen::MatrixXd SparseStore::evaluateSecondDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  Eigen::MatrixXd ret = fPlusModifierStored_.interpolateSecondDerivative(x, basis);
  ret -= modifier_.secondDerivative(x);
  return ret;
}

void SparseStore::initializeFromNormal(const MultiNormal& normal,
				       Function f,
				       VectorFunction fDerivative,
				       MatrixFunction fSecondDerivative,
				       const Basis& basis)
{
  auto location = normal.getMean();
  Eigen::VectorXd derivativeModifier
    = fDerivative(location) 
      - normal.evaluateDerivative(location);
  Eigen::MatrixXd secondDerivativeModifier
    = fSecondDerivative(location)
      - normal.evaluateSecondDerivative(location);
  modifier_.initializeFromDerivatives(location, 
				      derivativeModifier, 
				      secondDerivativeModifier);

  auto fModified = [f, this](const Eigen::VectorXd& x) {
    return f(x) + modifier_(x); 
  };
  fPlusModifierStored_.initializeFromNormal(fModified, 
					    normal, basis);
}
