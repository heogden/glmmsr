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

Eigen::ArrayXd Family::evaluateThirdDerivative(const Eigen::ArrayXd& linearPredictor,
					       const Eigen::ArrayXd& response, 
					       const Eigen::ArrayXd& weights) const
{
  auto mean = link_.computeMean(linearPredictor);
  familyGivenMean_.checkMean(mean);
  auto meanD1 = link_.computeMeanDerivative(linearPredictor);
  auto meanD2 = link_.computeMeanSecondDerivative(linearPredictor);
  auto meanD3 = link_.computeMeanThirdDerivative(linearPredictor);

  auto familyD1 = familyGivenMean_.evaluateDerivative(mean, response, weights);
  auto familyD2 = familyGivenMean_.evaluateSecondDerivative(mean, response, weights);
  auto familyD3 = familyGivenMean_.evaluateThirdDerivative(mean, response, weights);

  return pow(meanD1, 3) * familyD3  + 3. * meanD1 * meanD2 * familyD2 + meanD3 * familyD1;
}

Eigen::ArrayXd Family::evaluateFourthDerivative(const Eigen::ArrayXd& linearPredictor,
						const Eigen::ArrayXd& response, 
						const Eigen::ArrayXd& weights) const
{
  auto mean = link_.computeMean(linearPredictor);
  familyGivenMean_.checkMean(mean);
  auto meanD1 = link_.computeMeanDerivative(linearPredictor);
  auto meanD2 = link_.computeMeanSecondDerivative(linearPredictor);
  auto meanD3 = link_.computeMeanThirdDerivative(linearPredictor);
  auto meanD4 = link_.computeMeanFourthDerivative(linearPredictor);

  auto familyD1 = familyGivenMean_.evaluateDerivative(mean, response, weights);
  auto familyD2 = familyGivenMean_.evaluateSecondDerivative(mean, response, weights);
  auto familyD3 = familyGivenMean_.evaluateThirdDerivative(mean, response, weights);
  auto familyD4 = familyGivenMean_.evaluateFourthDerivative(mean, response, weights);

  auto t4 = pow(meanD1, 4) * familyD4;
  auto t3 = 6 * pow(meanD1, 2) * meanD2 * familyD3;
  auto t2 = (4 * meanD1 * meanD3 + 3 * pow(meanD2, 2)) * familyD2;
  auto t1 = meanD4 * familyD1;
  
  return t4 + t3 + t2 + t1;
}
