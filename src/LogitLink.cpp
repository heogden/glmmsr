#include "LogitLink.h"

LogitLink::LogitLink()
{
}

Eigen::ArrayXd LogitLink::computeMean(const Eigen::ArrayXd& linearPredictor) const
{
  return exp(linearPredictor) / (1 + exp(linearPredictor)); 
}

Eigen::ArrayXd LogitLink::computeMeanDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  return exp(linearPredictor) / pow(1. + exp(linearPredictor), 2);
}

Eigen::ArrayXd LogitLink::computeMeanSecondDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  return exp(linearPredictor) * (1 - exp(linearPredictor)) / pow(1. + exp(linearPredictor), 3);
}

Eigen::ArrayXd LogitLink::computeMeanThirdDerivative(const Eigen::ArrayXd& linearPredictor) const
{
 
  auto numerator = exp(linearPredictor) *
    (-4 * exp(linearPredictor) + exp(2 * linearPredictor) + 1.);
  auto denominator = pow(1. + exp(linearPredictor), 4);
  return numerator / denominator;
}

Eigen::ArrayXd LogitLink::computeMeanFourthDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  auto numerator = - exp(linearPredictor) *
    (11 * exp(linearPredictor) - 11 * exp(2*linearPredictor)
     + exp(3 * linearPredictor) - 1.);
  auto denominator = pow(1. + exp(linearPredictor), 5);
  return numerator / denominator;
}

std::string LogitLink::getName() const
{
  return "logit";
}
