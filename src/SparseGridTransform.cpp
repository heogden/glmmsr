#include "SparseGridTransform.h"

SparseGridTransform::SparseGridTransform(int d, const Basis& basis): fTransformStored_(d, basis), shift_(d), scale_(d, d), inverseScale_(d, d)
{
}

SparseGridTransform::SparseGridTransform(std::function<double(const Eigen::VectorXd&)> f, const Eigen::VectorXd& shift, const Eigen::MatrixXd& scale, const Basis& basis):
  fTransformStored_(shift.size(), basis), shift_(shift), scale_(scale), inverseScale_(shift.size(), shift.size())
{
  initializeTransformedFunction(f, basis);
  inverseScale_ = scale_.inverse();
}

double SparseGridTransform::interpolate(const Eigen::VectorXd& x, const Basis& basis) const
{
  Eigen::VectorXd z = inverseScale_ * (x - shift_);
  return fTransformStored_.interpolate(z, basis) - 0.5 * z.squaredNorm();
}

 Eigen::VectorXd SparseGridTransform::interpolateDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  Eigen::VectorXd z = inverseScale_ * (x - shift_);
  return inverseScale_.transpose() * (fTransformStored_.interpolateDerivative(z, basis) - z);
}

 Eigen::MatrixXd SparseGridTransform::interpolateSecondDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
 Eigen::VectorXd z = inverseScale_ * (x - shift_);
 Eigen::MatrixXd unscaledReturn = fTransformStored_.interpolateSecondDerivative(z, basis) - Eigen::MatrixXd::Identity(z.size(), z.size());
 return inverseScale_.transpose() * unscaledReturn * inverseScale_;
}


void SparseGridTransform::initializeTransformedFunction(std::function<double(const Eigen::VectorXd&)> f, const Basis& basis)
{
  auto fTransform = [&f, this](const Eigen::VectorXd& z) {
    return f(shift_ + scale_ * z) + 0.5 * z.squaredNorm();
  };
  fTransformStored_.initialize(fTransform, basis);
}

void SparseGridTransform::initializeFromNormal
(std::function<double(const Eigen::VectorXd&)> f, 
const MultiNormal& normal, const Basis& basis)
{
  if(normal.dimension() > 0) {
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigenPrecision
      (normal.getPrecision());
    scale_ = eigenPrecision.operatorInverseSqrt();
    inverseScale_ = eigenPrecision.operatorSqrt();
    shift_ = normal.getMean();
  }

  initializeTransformedFunction(f, basis);
}
