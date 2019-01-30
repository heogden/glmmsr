#ifndef GUARD_CONTINUOUSBELIEF_H
#define GUARD_CONTINUOUSBELIEF_H

#include <vector>
#include <memory>

#include <RcppEigen.h>

#include "Basis.h"
#include "MultiNormal.h"

#include "ContinuousBeliefBase.h"
#include "NormalBelief.h"
#include "Parameters.h"
#include "SparseBelief.h"

class ContinuousBelief
{
 public:
  ContinuousBelief();
  ContinuousBelief(const std::vector<int>&);
  ContinuousBelief(const std::vector<int>&, const Eigen::MatrixXd&,
		   const Eigen::MatrixXd&,
		   const Eigen::SparseMatrix<double>&,
		   const Eigen::VectorXi&, const Eigen::ArrayXd&,
		   const Eigen::ArrayXd&);
  ContinuousBelief(const std::vector<int>&, const Eigen::VectorXd&,
		   const Eigen::MatrixXd&);
  ContinuousBelief(const std::vector<int>&, const MultiNormal&, double);
  ContinuousBelief(const std::vector<int>&, SparseBelief::Function,
		   SparseBelief::VectorFunction, 
		   SparseBelief::MatrixFunction,
		   const MultiNormal&, const Basis&);
  ContinuousBelief(const NormalBelief&);

  std::vector<int> getItems() const;
  
  double evaluate(const Eigen::VectorXd&, const Parameters&);
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&,
				     const Parameters&);
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&,
					   const Parameters&);
	
 private:
  std::shared_ptr<ContinuousBeliefBase> continuousBelief_;
};


#endif // GUARD_CONTINUOUSBELIEF_H
