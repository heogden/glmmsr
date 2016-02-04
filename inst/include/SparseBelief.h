#ifndef GUARD_SPARSEBELIEF_H
#define GUARD_SPARSEBELIEF_H

#include "MultiNormal.h"

#include "Basis.h"
#include "SparseStore.h"

#include "ContinuousBeliefBase.h"

// Belief for function stored with a sparse grid
class SparseBelief : public ContinuousBeliefBase
{
 public:
  typedef std::function<double(const Eigen::VectorXd&)> Function;
  typedef std::function<Eigen::VectorXd(const Eigen::VectorXd&)>
    VectorFunction;
  typedef std::function<Eigen::MatrixXd(const Eigen::VectorXd&)> 
    MatrixFunction;

  SparseBelief(const std::vector<int>&,
	       Function, VectorFunction, MatrixFunction,
	       const MultiNormal&, const Basis&);

  double evaluate(const Eigen::VectorXd&, const Parameters&);
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&,
				     const Parameters&);
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&,
					   const Parameters&);

 private:
  SparseStore fStored_;
};

#endif // GUARD_SPARSEBELIEF_H
