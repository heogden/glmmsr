#ifndef GUARD_SPARSEGRIDTRANSFORM_H
#define GUARD_SPARSEGRIDTRANSFORM_H

#include "MultiNormal.h"

#include "SparseGrid.h"

class SparseGridTransform
{
  friend class SparseStore;
 public:
  SparseGridTransform(int, const Basis&);
  SparseGridTransform(std::function<double(const Eigen::VectorXd&)>, const Eigen::VectorXd&, const Eigen::MatrixXd&, const Basis&);

  double interpolate(const Eigen::VectorXd&, const Basis&) const;
  Eigen::VectorXd interpolateDerivative(const Eigen::VectorXd&, const Basis&) const;
  Eigen::MatrixXd interpolateSecondDerivative(const Eigen::VectorXd&, const Basis&) const;

 private:
  SparseGrid fTransformStored_;
  Eigen::VectorXd shift_;
  Eigen::MatrixXd scale_;
  Eigen::MatrixXd inverseScale_;

  void initializeTransformedFunction(std::function<double(const Eigen::VectorXd&)>, const Basis& basis);
  void initializeFromNormal(std::function<double(const Eigen::VectorXd&)>, const MultiNormal&, const Basis& basis);
};

#endif // GUARD_SPARSEGRIDTRANSFORM_H
