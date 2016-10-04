#ifndef GUARD_BINOMIAL_H
#define GUARD_BINOMIAL_H

#include "FamilyBase.h"

class Binomial : public FamilyBase
{
 public:
  Binomial();

  void checkMean(Eigen::ArrayXd&) const;
  
  double evaluate(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateSecondDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateThirdDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateFourthDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;

  std::string getName() const;
};

#endif // GUARD_BINOMIAL_H
