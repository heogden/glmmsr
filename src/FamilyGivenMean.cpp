#include <stdexcept>

#include "Binomial.h"
#include "ContinuousBeliefBase.h"

#include "FamilyGivenMean.h"

FamilyGivenMean::FamilyGivenMean()
{
}

FamilyGivenMean::FamilyGivenMean(const std::string& type)
{
  initialize(type);
}

void FamilyGivenMean::initialize(const std::string& type)
{
  if(type == "binomial")
    familyGivenMean_ = std::shared_ptr<FamilyBase>(new Binomial);
  else
    throw std::domain_error("constructing a family of unknown type");
}

void FamilyGivenMean::checkMean(Eigen::ArrayXd& mean) const
{
  familyGivenMean_->checkMean(mean);
}

double FamilyGivenMean::evaluate(const Eigen::ArrayXd& mean, 
				 const Eigen::ArrayXd& response, 
				 const Eigen::ArrayXd& weights) const
{
  return familyGivenMean_->evaluate(mean, response, weights);
}

Eigen::ArrayXd FamilyGivenMean::evaluateDerivative(const Eigen::ArrayXd& mean, 
						    const Eigen::ArrayXd& response,
						    const Eigen::ArrayXd& weights) const
{
    return familyGivenMean_->evaluateDerivative(mean, response, weights);

}

Eigen::ArrayXd FamilyGivenMean::evaluateSecondDerivative(const Eigen::ArrayXd& mean,
							  const Eigen::ArrayXd& response, 
							  const Eigen::ArrayXd& weights) const
{
  return familyGivenMean_->evaluateSecondDerivative(mean, response, weights);
}

Eigen::ArrayXd FamilyGivenMean::evaluateThirdDerivative(const Eigen::ArrayXd& mean,
							const Eigen::ArrayXd& response, 
							const Eigen::ArrayXd& weights) const
{
  return familyGivenMean_->evaluateThirdDerivative(mean, response, weights);
}

Eigen::ArrayXd FamilyGivenMean::evaluateFourthDerivative(const Eigen::ArrayXd& mean,
							 const Eigen::ArrayXd& response, 
							 const Eigen::ArrayXd& weights) const
{
  return familyGivenMean_->evaluateFourthDerivative(mean, response, weights);
}

std::string FamilyGivenMean::getName() const
{
  return familyGivenMean_->getName();
}
