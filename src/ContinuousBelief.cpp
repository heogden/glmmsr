#include "GLMMBelief.h"

#include "ContinuousBelief.h"

ContinuousBelief::ContinuousBelief(): continuousBelief_(new NormalBelief)
{
}

ContinuousBelief::ContinuousBelief(const std::vector<int>& items):
  continuousBelief_(new NormalBelief(items))
{
}


ContinuousBelief::ContinuousBelief(const std::vector<int>& items,
				   const Eigen::MatrixXd& X,
				   const Eigen::MatrixXd& Zt,
				   const Eigen::SparseMatrix<double>& Lambdat,
				   const Eigen::VectorXi& Lind,
				   const Eigen::ArrayXd& response,
				   const Eigen::ArrayXd& weights):
  continuousBelief_(new GLMMBelief(items, X, Zt, Lambdat, Lind, response, 
			weights))
{

}

ContinuousBelief::ContinuousBelief(const std::vector<int>& items,
				   const Eigen::VectorXd& mean,
				   const Eigen::MatrixXd& precision):
  continuousBelief_(new NormalBelief(items, mean, precision))
{
}

ContinuousBelief::ContinuousBelief(const std::vector<int>& items,
				   const MultiNormal& normal,
				   double logNormalizingConstant):
  continuousBelief_(new NormalBelief(items, normal, logNormalizingConstant))
{
}

ContinuousBelief::ContinuousBelief(const std::vector<int>& items, 
				   SparseBelief::Function f,
				   SparseBelief::VectorFunction fDerivative,
				   SparseBelief::MatrixFunction fSecondDerivative,
				   const MultiNormal& normal, 
				   const Basis& basis):
  continuousBelief_(new SparseBelief(items, f, fDerivative, fSecondDerivative,normal, basis))
{
}


ContinuousBelief::ContinuousBelief(const NormalBelief& belief):
  continuousBelief_(new NormalBelief(belief.getItems(),
				     belief.getNormal(),
				     belief.getLogNormalizingConstant()))
{
}


std::vector<int> ContinuousBelief::getItems() const
{
  return continuousBelief_->getItems();
}

double ContinuousBelief::evaluate(const Eigen::VectorXd& x,
				  const Parameters& parameters)
{
  return continuousBelief_->evaluate(x, parameters);
}

Eigen::VectorXd ContinuousBelief::evaluateDerivative
(const Eigen::VectorXd& x,
 const Parameters& parameters)
{
  return continuousBelief_->evaluateDerivative(x, parameters);
}

Eigen::MatrixXd ContinuousBelief::evaluateSecondDerivative
(const Eigen::VectorXd& x,
 const Parameters& parameters)
{
  return continuousBelief_->evaluateSecondDerivative(x, parameters);
}


