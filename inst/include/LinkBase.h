#ifndef GUARD_LINKBASE_H
#define GUARD_LINKBASE_H

#include <string>

#include <RcppEigen.h>

class LinkBase
{
  friend class Link;
 public:
  virtual ~LinkBase();
  LinkBase();

  virtual Eigen::ArrayXd computeMean(const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd computeMeanDerivative(const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd computeMeanSecondDerivative(const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd computeMeanThirdDerivative(const Eigen::ArrayXd&) const = 0;
  virtual Eigen::ArrayXd computeMeanFourthDerivative(const Eigen::ArrayXd&) const = 0;
  
  virtual std::string getName() const = 0;
};

#endif // GUARD_LINKBASE_H
