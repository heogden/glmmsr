#include "Binomial.h"

Binomial::Binomial()
{
}

void Binomial::checkMean(Eigen::ArrayXd& mean) const
{
  const double epsilon = 1e-8;
  for(auto i = 0; i < mean.size(); ++i) {
    if(mean(i) < epsilon)
      mean(i) = epsilon;
    if(mean(i) > 1 - epsilon)
      mean(i) = 1 - epsilon;
  }
}

double Binomial::evaluate(const Eigen::ArrayXd& mean, 
			  const Eigen::ArrayXd& response, 
			  const Eigen::ArrayXd& weights) const
{

  return ( weights * (response * log(mean) + (1 - response) * log(1 - mean)) ).sum();
}

Eigen::ArrayXd Binomial::evaluateDerivative(const Eigen::ArrayXd& mean, 
					     const Eigen::ArrayXd& response,
					     const Eigen::ArrayXd& weights) const
{
  return weights * (response  / mean) - (1 - response) / (1 - mean);
}

Eigen::ArrayXd Binomial::evaluateSecondDerivative(const Eigen::ArrayXd& mean,
						   const Eigen::ArrayXd& response, 
						   const Eigen::ArrayXd& weights) const
{
  return -weights * ( response * pow(mean, -2) + (1 - response) * pow(1 - mean, -2) );
}

std::string Binomial::getName() const
{
  return "binomial";
}
