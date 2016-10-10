#include "ProbitLink.h"

ProbitLink::ProbitLink()
{
}

Eigen::ArrayXd ProbitLink::computeMean(const Eigen::ArrayXd& linearPredictor) const
{
  Eigen::ArrayXd mean(linearPredictor.size());
  for(auto i = 0; i < mean.size(); ++i)
    mean(i) = cdf(standardNormal_, linearPredictor(i));
  return mean;
}

Eigen::ArrayXd ProbitLink::computeMeanDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  Eigen::ArrayXd derivative(linearPredictor.size());
  for(auto i = 0; i < derivative.size(); ++i)
    derivative(i) = pdf(standardNormal_, linearPredictor(i));
  return derivative;
}

Eigen::ArrayXd ProbitLink::computeMeanSecondDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  Eigen::ArrayXd secondDerivative(linearPredictor.size());
  for(auto i = 0; i < secondDerivative.size(); ++i)
    secondDerivative(i) = -linearPredictor(i) * pdf(standardNormal_, linearPredictor(i));
  return secondDerivative;
}

Eigen::ArrayXd ProbitLink::computeMeanThirdDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  Eigen::ArrayXd thirdDerivative(linearPredictor.size());
  for(auto i = 0; i < thirdDerivative.size(); ++i)
    thirdDerivative(i) = (pow(linearPredictor(i), 2) - 1.) * pdf(standardNormal_, linearPredictor(i));
  return thirdDerivative;
}


Eigen::ArrayXd ProbitLink::computeMeanFourthDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  Eigen::ArrayXd fourthDerivative(linearPredictor.size());
  for(auto i = 0; i < fourthDerivative.size(); ++i)
    fourthDerivative(i) = (-pow(linearPredictor(i), 3) +  3. * linearPredictor(i)) * pdf(standardNormal_, linearPredictor(i));
  return fourthDerivative;
}


std::string ProbitLink::getName() const
{
  return "probit";
}
