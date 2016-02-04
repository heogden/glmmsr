#ifndef GUARD_SPARSESTORE_H
#define GUARD_SPARSESTORE_H

#include "Quadratic.h"
#include "SparseGridTransform.h"

class SparseStore
{
  typedef std::function<double(const Eigen::VectorXd&)> Function;
  typedef std::function<Eigen::VectorXd(const Eigen::VectorXd&)> VectorFunction;
  typedef std::function<Eigen::MatrixXd(const Eigen::VectorXd&)> MatrixFunction;

 public:
  SparseStore(Function, VectorFunction, MatrixFunction, 
	      const Eigen::VectorXd&, const Eigen::MatrixXd&, 
	      const Basis&);
  SparseStore(Function, VectorFunction, MatrixFunction, 
	      const MultiNormal&, const Basis&);

  double evaluate(const Eigen::VectorXd&, const Basis&) const;
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&, const Basis&) const;
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&, const Basis&) const;

 private:
  Quadratic modifier_;
  SparseGridTransform fPlusModifierStored_;

  void initializeFromNormal(const MultiNormal&, Function,
			    VectorFunction, MatrixFunction,
			    const Basis&);
};

#endif // GUARD_SPARSESTORE_H
