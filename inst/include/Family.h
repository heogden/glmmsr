#ifndef GUARD_FAMILY_H
#define GUARD_FAMILY_H

#include <string>

#include <RcppEigen.h>

#include "FamilyGivenMean.h"
#include "Link.h"

class Family
{
 public:
  Family();
  Family(const FamilyGivenMean&, const Link&);
  Family(const std::string&, const std::string&);

  void setFamilyGivenMean(const FamilyGivenMean&);
  void setLink(const Link&);

  FamilyGivenMean getFamilyGivenMean() const;
  std::string getFamilyName() const;
  Link getLink() const;
  std::string getLinkName() const;

  double evaluate(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateSecondDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateThirdDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;
  Eigen::ArrayXd evaluateFourthDerivative(const Eigen::ArrayXd&, const Eigen::ArrayXd&, const Eigen::ArrayXd&) const;


 private:
  Link link_;
  FamilyGivenMean familyGivenMean_;
};

#endif // GUARD_FAMILY_H
