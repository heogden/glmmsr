#ifndef GUARD_QUADRATIC_H
#define GUARD_QUADRATIC_H

#include <RcppEigen.h>

class Quadratic
{
  friend class SparseStore;
 public:
  Quadratic(int);
  double operator()(const Eigen::VectorXd&) const;
  Eigen::VectorXd derivative(const Eigen::VectorXd&) const;
  Eigen::MatrixXd secondDerivative(const Eigen::VectorXd&) const;
 private:
  Eigen::VectorXd b_;
  Eigen::MatrixXd A_;
  void initializeFromDerivatives(const Eigen::VectorXd&, const Eigen::VectorXd&, const Eigen::MatrixXd&);
};

#endif // GUARD_QUADRATIC_H
