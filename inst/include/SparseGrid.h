#ifndef GUARD_SPARSEGRID_H
#define GUARD_SPARSEGRID_H

#include <functional>
#include <vector>

#include <RcppEigen.h>

#include "Basis.h"
#include "Point.h"

class SparseGrid
{
 public:
  SparseGrid(int, const Basis&);
  SparseGrid(std::function<double(const Eigen::VectorXd&)>, int, const Basis&);
  SparseGrid(std::function<double(const Eigen::VectorXd&)>, int, int);

  void initialize(std::function<double(const Eigen::VectorXd&)>, const Basis&);

  double interpolate(const Eigen::VectorXd&, const Basis&) const;
  Eigen::VectorXd interpolateDerivative(const Eigen::VectorXd&, const Basis&) const;
  Eigen::MatrixXd interpolateSecondDerivative(const Eigen::VectorXd&, const Basis&) const;

  int size() const;

 private:
  int k_;
  int d_;
  std::vector<double> coefficients_;
  double bound_;

  void initializeStorage(std::function<double(const Eigen::VectorXd&)>, const Basis&);
  void hierarchize(const Basis&);
};



#endif // GUARD_SPARSEGRID_H
