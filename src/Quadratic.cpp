#include "Quadratic.h"

Quadratic::Quadratic(int d): b_(d), A_(d, d)
{
}

double Quadratic::operator()(const Eigen::VectorXd& x) const
{
  return (x - b_).transpose() * A_ * (x - b_);
}

Eigen::VectorXd Quadratic::derivative(const Eigen::VectorXd& x) const
{
  return (x - b_).transpose() * (A_ +  A_.transpose());
}

Eigen::MatrixXd Quadratic::secondDerivative(const Eigen::VectorXd& x) const
{
  return A_ + A_.transpose();
}


void Quadratic::initializeFromDerivatives(const Eigen::VectorXd& location, const Eigen::VectorXd& derivativeAtLocation, 
		     const Eigen::MatrixXd& secondDerivativeAtLocation)
{
  if(location.size() > 0) {
    // there are multiple choices of A  which would give the
    // correct derivatives. We choose the symmetric one.
    A_ = -secondDerivativeAtLocation / 2;
    if(A_.norm() > 0)
       b_ = location + A_.colPivHouseholderQr().solve(derivativeAtLocation) / 2;
    else
      b_ = location;
  }
}
