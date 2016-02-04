#ifndef GUARD_NORMALBELIEF_H
#define GUARD_NORMALBELIEF_H

#include "MultiNormal.h"

#include "ContinuousBeliefBase.h"

class MixedContinousBelief;

class NormalBelief : public ContinuousBeliefBase
{
public:

  NormalBelief();
  NormalBelief(const std::vector<int>&);
  NormalBelief(const std::vector<int>&, const MultiNormal&, 
	       double);
  NormalBelief(const std::vector<int>&, const Eigen::VectorXd&,
	       const Eigen::MatrixXd&);
  NormalBelief(const std::vector<int>&, const Eigen::VectorXd&,
	       const Eigen::VectorXd&, const Eigen::MatrixXd&);

  void setItems(const std::vector<int>&);

  void operator*=(const NormalBelief&);
  void operator/=(const NormalBelief&);
  
  NormalBelief margin(const std::vector<int>&) const;

  double getLogNormalizingConstant() const;
  MultiNormal getNormal() const;
  
  double evaluate(const Eigen::VectorXd&, const Parameters&);
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&,
				     const Parameters&);
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&,
					   const Parameters&);

 private:
  MultiNormal normal_;
  double logNormalizingConstant_;

  void initializeGivenItems();
};

#endif // GUARD_NORMALBELIEF_H
