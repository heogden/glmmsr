#include <numeric>

#include "matrixHelper.h"

#include "itemsHelper.h"

#include "NormalBelief.h"

using Eigen::VectorXd;
using Eigen::MatrixXd;

NormalBelief::NormalBelief():
  logNormalizingConstant_(0)
{
}

NormalBelief::NormalBelief(const std::vector<int>& items):
  ContinuousBeliefBase(items),
  normal_(Eigen::VectorXd::Zero(items.size()),
	  Eigen::MatrixXd::Zero(items.size(), items.size())),
  logNormalizingConstant_(0)
{
}

NormalBelief::NormalBelief(const std::vector<int>& items,
			   const MultiNormal& normal,
			   double logNormalizingConstant):
  ContinuousBeliefBase(items),
  normal_(normal), 
  logNormalizingConstant_(logNormalizingConstant)
{
}

NormalBelief::NormalBelief(const std::vector<int>& items,
			   const Eigen::VectorXd& mean,
			   const Eigen::MatrixXd& precision):
  ContinuousBeliefBase(items),
  normal_(mean, precision),
  logNormalizingConstant_(0)
{
}

NormalBelief::NormalBelief(const std::vector<int>& items,
			   const Eigen::VectorXd& location,
			   const Eigen::VectorXd& derivative,
			   const Eigen::MatrixXd& secondDerivative):
  ContinuousBeliefBase(items),
  normal_(location, derivative, secondDerivative),
  logNormalizingConstant_(0)
{
}

void NormalBelief::setItems(const std::vector<int>& items)
{
  items_ = items;
  normal_.setMean(Eigen::VectorXd::Zero(items.size()));
  normal_.setPrecision(Eigen::MatrixXd::Zero(items.size(), items.size()));
}


void NormalBelief::operator*=(const NormalBelief& other)
{

  auto relativeItems = findRelativeItems(other.getItems(), items_);

  normal_.multiplySubset(other.normal_, relativeItems);

  logNormalizingConstant_ += other.logNormalizingConstant_;

}

void NormalBelief::operator/=(const NormalBelief& other)
{
  auto relativeItems = findRelativeItems(other.getItems(), items_);
  normal_.divideSubset(other.normal_, relativeItems);
  logNormalizingConstant_ -= other.logNormalizingConstant_;
}

NormalBelief NormalBelief::margin(const std::vector<int>& items) const
{
  auto relativeItems = findRelativeItems(items, items_);
  return NormalBelief(items,
		      normal_.computeMarginal(relativeItems),
		      logNormalizingConstant_);
}



double NormalBelief::getLogNormalizingConstant() const
{
  return logNormalizingConstant_;
}

MultiNormal NormalBelief::getNormal() const
{
  return normal_;
}

double NormalBelief::evaluate(const VectorXd& x, const Parameters& parameters)
{
  return logNormalizingConstant_ + normal_.evaluate(x);
}

VectorXd NormalBelief::evaluateDerivative(const VectorXd& x, const Parameters& parameters)
{
  return normal_.evaluateDerivative(x);
}

MatrixXd NormalBelief::evaluateSecondDerivative(const VectorXd& x, const Parameters& parameters)
{
  return normal_.evaluateSecondDerivative(x);
}


void NormalBelief::initializeGivenItems()
{
  normal_.setMean(Eigen::VectorXd::Zero(items_.size()));
  normal_.setPrecision(Eigen::MatrixXd::Zero(items_.size(), items_.size()));
  logNormalizingConstant_ = 0;
}
