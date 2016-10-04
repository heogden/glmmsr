#ifndef GUARD_FAMILYBASE_H
#define GUARD_FAMILYBASE_H

#include <string>

#include <RcppEigen.h>

class FamilyBase
{
  friend class FamilyGivenMean;
 public:
  virtual ~FamilyBase();
  FamilyBase();

  virtual void checkMean(Eigen::ArrayXd&) const = 0;
  
  virtual double evaluate(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd evaluateDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd evaluateSecondDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd evaluateThirdDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd evaluateFourthDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const = 0;

  virtual std::string getName() const = 0;
};

#endif // GUARD_FAMILYBASE_H
