#ifndef GUARD_FAMILYGIVENMEAN_H
#define GUARD_FAMILYGIVENMEAN_H

#include <memory>
#include <string>

#include <RcppEigen.h>

#include "FamilyBase.h"

class FamilyGivenMean
{
 public:
  FamilyGivenMean();
  FamilyGivenMean(const std::string&);

  void initialize(const std::string&);

  void checkMean(Eigen::ArrayXd&) const;
  
  double evaluate(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateSecondDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateThirdDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateFourthDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;

  
  std::string getName() const;

 private:
  std::shared_ptr<FamilyBase> familyGivenMean_;
};

#endif // GUARD_FAMILYGIVENMEAN_H
