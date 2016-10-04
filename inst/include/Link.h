#ifndef GUARD_LINK_H
#define GUARD_LINK_H

#include <memory>
#include <string>

#include <RcppEigen.h>

#include "LinkBase.h"

class Link
{
 public:
  Link();
  Link(const std::string&);

  void initialize(const std::string&);

  Eigen::ArrayXd computeMean(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanSecondDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanThirdDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanFourthDerivative(const Eigen::ArrayXd&) const;
  
  std::string getName() const;

 private:
  std::shared_ptr<LinkBase> link_;
};

#endif // GUARD_LINK_H
