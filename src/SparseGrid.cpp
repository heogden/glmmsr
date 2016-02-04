#include "counting.h"

#include "SparseGrid.h"


SparseGrid::SparseGrid(int d, const Basis& basis):
  k_(basis.getLevelMax()), d_(d), coefficients_(findNumGridPoints(d, k_))
{
}

SparseGrid::SparseGrid(std::function<double(const Eigen::VectorXd&)> f, 
		       int d, int k): k_(k), d_(d), coefficients_(findNumGridPoints(d, k))
{
  Basis basis(k);
  initialize(f, basis);
}

SparseGrid::SparseGrid(std::function<double(const Eigen::VectorXd&)> f, 
		       int d, const Basis& basis):
  k_(basis.getLevelMax()), d_(d), coefficients_(findNumGridPoints(d_, k_))
{
  initialize(f, basis);
}

void SparseGrid::initialize(std::function<double(const Eigen::VectorXd&)> f, const Basis& basis)
{
  initializeStorage(f, basis);
  hierarchize(basis);
}

double SparseGrid::interpolate(const Eigen::VectorXd& x, const Basis& basis) const
{
  double ret = 0;
  Point point(d_);
  for(auto i = 0; i < size(); ++i) {
    ret += coefficients_[i] * point.computeBasis(x, basis);
    if(i < size() - 1)
      point.advance();
  }
  return std::min(ret, bound_);
}

Eigen::VectorXd SparseGrid::interpolateDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  Eigen::VectorXd ret;
  if(interpolate(x, basis) <  bound_) {
      ret = Eigen::VectorXd::Zero(d_);
      Point point(d_);
      for(auto i = 0; i < size(); ++i) {
	ret += coefficients_[i] * point.computeBasisDerivative(x, basis);
	if(i < size() - 1)
	  point.advance();
      }
  } else {
    ret = Eigen::VectorXd::Zero(d_);
  }
  return ret;
}

Eigen::MatrixXd SparseGrid::interpolateSecondDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  Eigen::MatrixXd ret;
  if(interpolate(x, basis) <  bound_) {
    ret = Eigen::MatrixXd::Zero(d_, d_);
    Point point(d_);
    for(auto i = 0; i < size(); ++i) {
      ret += coefficients_[i] * point.computeBasisSecondDerivative(x, basis);
      if(i < size() - 1)
	point.advance();
    }
  } else {
    ret = Eigen::MatrixXd::Zero(d_, d_);
  }
  return ret;
}


int SparseGrid::size() const
{
  return coefficients_.size();
}

void SparseGrid::initializeStorage(std::function<double(const Eigen::VectorXd&)> f, const Basis& basis)
{
  Point point(d_);
  double maximumStoredValue;
  for(auto i = 0; i < size(); ++i) {
    Eigen::VectorXd gridPoint = point.getGridPoint(basis);
    coefficients_.at(i) = f(gridPoint);
    double value = coefficients_.at(i);
    if((i ==0) || (value > maximumStoredValue))
      maximumStoredValue = value;
    if(i < size() - 1)
      point.advance();
  }
  bound_ = maximumStoredValue;
}

void SparseGrid::hierarchize(const Basis& basis)
{
  for(auto t = 0; t < d_; ++t) {
    Point point(d_);
    for(auto i = 0; i < size(); ++i) {
      double knot = point.getGridPointInDirection(t, basis);
      Point parent = point.firstParent(t);
      while(parent.getLevel(t) < point.getLevel(t)) {
	double contribution = coefficients_.at(parent.getIndex());
	contribution *= basis.evaluate(knot, parent.getLevel(t), parent.getPosition(t));
	coefficients_.at(i) -= contribution;
	parent.advanceInDirection(t);
      }
      point.advance();
    }
  }
}
