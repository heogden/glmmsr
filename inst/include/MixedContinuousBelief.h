#ifndef GUARD_MIXEDCONTINUOUSBELIEF_H
#define GUARD_MIXEDCONTINUOUSBELIEF_H

#include "ContinuousBelief.h"
#include "NormalBelief.h"

class MixedContinuousBelief : public BeliefBase
{
 public:

  MixedContinuousBelief();
  MixedContinuousBelief(const std::vector<int>&);
  MixedContinuousBelief(const ContinuousBelief&);

  void setItems(const std::vector<int>&);

  void setNormalApprox(const Eigen::VectorXd&, const Eigen::MatrixXd&);
  void setNormalApprox(const NormalBelief&);

  void fixNormalApprox();
  
  void operator*=(const MixedContinuousBelief&);
  void operator/=(const MixedContinuousBelief&);
  
  MixedContinuousBelief margin(const std::vector<int>&,
			       bool,
			       const Parameters&);

  double computeLogNormalizingConstant(const Parameters&);
  
  double evaluate(const Eigen::VectorXd&, const Parameters&);
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&,
				     const Parameters&);
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&,
					   const Parameters&);

  bool isProper(const Parameters& parameters);
    
 private:
  std::vector<ContinuousBelief> beliefs_;
  std::vector<std::vector<int>> relativeItems_;
  std::vector<bool> isInverted_;
  NormalBelief normalApprox_;
  bool isNormalFixed_;

  MixedContinuousBelief integrate(int, const Parameters&);
  
  void recomputeRelativeItems();

};


#endif //GUARD_MIXEDCONTINUOUSBELIEF_H
