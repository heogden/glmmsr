#ifndef GUARD_PROBITLINK_H
#define GUARD_PROBITLINK_H

#include <boost/math/distributions/normal.hpp>

#include "LinkBase.h"

class ProbitLink : public LinkBase
{
 public:
  ProbitLink();

  Eigen::ArrayXd computeMean(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanSecondDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanThirdDerivative(const Eigen::ArrayXd&) const;
  Eigen::ArrayXd computeMeanFourthDerivative(const Eigen::ArrayXd&) const;

  std::string getName() const;

 private:
  boost::math::normal standardNormal_;

};

#endif // GUARD_PROBITLINK_H
