#include <math.h>

#include "subsetEigen.h"

#include "MultiNormal.h"

MultiNormal::MultiNormal(): mean_(), precision_(0, 0), variance_(0, 0),
			    normalizer_(0)
{
}

MultiNormal::MultiNormal(int dimension):
  mean_(dimension), precision_(dimension, dimension),
  variance_(dimension, dimension), normalizer_(0)
{
}

MultiNormal::MultiNormal(const Eigen::VectorXd& mean,
			 const Eigen::MatrixXd& precision):
  mean_(mean), precision_(precision), variance_(precision), normalizer_(0)
{
  setPrecision(precision);
}

MultiNormal::MultiNormal(const Eigen::VectorXd& location,
			 const Eigen::VectorXd& derivative,
			 const Eigen::MatrixXd& secondDerivative):
  mean_(location.size()), precision_(location.size(), location.size()),
  variance_(location.size(), location.size()), normalizer_(0)
{
  initializeFromDerivatives(location, derivative, secondDerivative);
}


void MultiNormal::initializeFromDerivatives(const Eigen::VectorXd& location,
					    const Eigen::VectorXd& derivative,
					    const Eigen::MatrixXd& secondDerivative)
{
  setPrecision(-secondDerivative);
  setMean(location + precision_.colPivHouseholderQr().solve(derivative));
}


void MultiNormal::setMean(const Eigen::VectorXd& mean)
{
  mean_.resize(mean.size());
  mean_ = mean;
}

void MultiNormal::setPrecision(const Eigen::MatrixXd& precision)
{
  precision_.resize(precision.rows(), precision.cols());
  precision_ = precision;
  variance_.resize(precision.rows(), precision.cols());
  variance_ = precision.inverse();
  initializeNormalizer();
}

void MultiNormal::setVariance(const Eigen::MatrixXd& variance)
{
  variance_.resize(variance.rows(), variance.cols());
  variance_ = variance;
  precision_.resize(variance.rows(), variance.cols());
  precision_ = variance.inverse();
  
  initializeNormalizer();
}

void MultiNormal::multiplySubset(const MultiNormal& normal,
				 const std::vector<int>& subset)
{
  multiplySubsetInternal(normal, subset, false);
}

void MultiNormal::divideSubset(const MultiNormal& normal,
			       const std::vector<int>& subset)
{
  multiplySubsetInternal(normal, subset, true);
}

bool MultiNormal::isProper() const
{
  return ((normalizer_ != 0) && (std::isfinite(normalizer_)));
}

double MultiNormal::evaluate(const Eigen::VectorXd& x) const
{
  Eigen::VectorXd xDiff = x - mean_;
  double ret = normalizer_;
  ret += -0.5 * xDiff.transpose() * precision_ * xDiff;
  return ret;
}

Eigen::VectorXd MultiNormal::evaluateDerivative(const Eigen::VectorXd& x) const
{
  Eigen::VectorXd xDiff = x - mean_;
  return -xDiff.transpose() * precision_;
}

Eigen::MatrixXd MultiNormal::evaluateSecondDerivative(const Eigen::VectorXd& x) const
{
  return -precision_;
}

Eigen::VectorXd MultiNormal::getMean() const
{
  return mean_;
}

Eigen::MatrixXd MultiNormal::getPrecision() const
{
  return precision_;
}

Eigen::MatrixXd MultiNormal::getVariance() const
{
  return variance_;
}

int MultiNormal::dimension() const
{
  return mean_.size();
}

MultiNormal MultiNormal::computeMarginal(const std::vector<int>& items) const
{
  auto varianceMarginal = getMatrixSubset(variance_, items);
  auto meanMarginal = getVectorSubset(mean_, items);
  MultiNormal marginal(items.size());
  marginal.setMean(meanMarginal);
  marginal.setVariance(varianceMarginal);
  return marginal;
}

MultiNormal MultiNormal::integrate(int item) const
{
  auto varianceMarginal = getMatrixWithout(item, variance_);
  auto meanMarginal = getVectorWithout(item, mean_);
  MultiNormal marginal(meanMarginal.size());
  marginal.setMean(meanMarginal);
  marginal.setVariance(varianceMarginal);
  return marginal;
}

void MultiNormal::initializeNormalizer()
{
  static const double logSqrtTwoPi = 0.91893853320467266954;
  normalizer_ = -dimension() * logSqrtTwoPi + 0.5 * log(precision_.determinant());
}

void MultiNormal::multiplySubsetInternal
(const MultiNormal& normal,
 const std::vector<int>& subset,
 bool divide)
{
  auto precisionSubset = getMatrixSubset(precision_, subset);
  auto normalPrecision = normal.precision_;
  if(divide)
    normalPrecision *= -1;

  Eigen::MatrixXd newPrecisionSubset 
    = precisionSubset + normalPrecision;

  auto meanSubset = getVectorSubset(mean_, subset);
  meanSubset = newPrecisionSubset.inverse() * (precisionSubset * meanSubset + normalPrecision * normal.mean_) ;

  setMatrixSubset(precision_, newPrecisionSubset, subset);
  setVectorSubset(mean_, meanSubset, subset);

  variance_ = precision_.inverse();
  initializeNormalizer();
  
}
