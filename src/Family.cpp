#include "Family.h"

Family::Family()
{
}

Family::Family(const FamilyGivenMean& familyGivenMean, const Link& link):
  link_(link), familyGivenMean_(familyGivenMean)
{
}

Family::Family(const std::string& familyName, 
	       const std::string& linkName):
  link_(linkName), familyGivenMean_(familyName)
{
}

void Family::setFamilyGivenMean
(const FamilyGivenMean& familyGivenMean)
{
  familyGivenMean_ = familyGivenMean;
}

void Family::setLink(const Link& link)
{
  link_ = link;
}

FamilyGivenMean Family::getFamilyGivenMean() const
{
  return familyGivenMean_;
}

std::string Family::getFamilyName() const
{
  return familyGivenMean_.getName();
}

Link Family::getLink() const
{
  return link_;
}

std::string Family::getLinkName() const
{
  return link_.getName();
}

double Family::evaluate(const Eigen::ArrayXd& linearPredictor,
			const Eigen::ArrayXd& response,
			const Eigen::ArrayXd& weights) const
{
  auto mean = link_.computeMean(linearPredictor);
  familyGivenMean_.checkMean(mean);
  return familyGivenMean_.evaluate(mean, response, weights);
}


Eigen::ArrayXd Family::evaluateDerivative(const Eigen::ArrayXd& linearPredictor,
					  const Eigen::ArrayXd& response, 
					  const Eigen::ArrayXd& weights) const
{
  auto mean = link_.computeMean(linearPredictor);
  familyGivenMean_.checkMean(mean);
  auto meanDerivative = link_.computeMeanDerivative(linearPredictor);
  return meanDerivative * familyGivenMean_.evaluateDerivative(mean, response, weights);
}


Eigen::ArrayXd Family::evaluateSecondDerivative(const Eigen::ArrayXd& linearPredictor,
						const Eigen::ArrayXd& response, 
						const Eigen::ArrayXd& weights) const
{
  auto mean = link_.computeMean(linearPredictor);
  familyGivenMean_.checkMean(mean);
  auto meanDerivative = link_.computeMeanDerivative(linearPredictor);
  auto meanSecondDerivative = link_.computeMeanSecondDerivative(linearPredictor);

  auto familyDerivative = familyGivenMean_.evaluateDerivative(mean, response, weights);
  auto familySecondDerivative = familyGivenMean_.evaluateSecondDerivative(mean, response, weights);
  return pow(meanDerivative, 2) * familySecondDerivative  + meanSecondDerivative * familyDerivative;
}
