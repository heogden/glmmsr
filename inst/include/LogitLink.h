#ifndef GUARD_LOGITLINK_H
#define GUARD_LOGITLINK_H

#include "LinkBase.h"

class LogitLink : public LinkBase
{
 public:
  LogitLink();

  Eigen::ArrayXd computeMean(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanSecondDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanThirdDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanFourthDerivative(const Eigen::ArrayXd&) const;
  
  std::string getName() const;
};

#endif // GUARD_LOGITLINK_H
