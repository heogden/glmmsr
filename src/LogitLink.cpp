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

std::string LogitLink::getName() const
{
  return "logit";
}
