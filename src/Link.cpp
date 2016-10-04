#include "LogitLink.h"
#include "ProbitLink.h"

#include "Link.h"

Link::Link()
{
}

Link::Link(const std::string& type)
{
  initialize(type);
}

void Link::initialize(const std::string& type)
{
  if(type == "logit")
    link_ = std::shared_ptr<LinkBase>(new LogitLink);
  else if(type == "probit")
    link_ = std::shared_ptr<LinkBase>(new ProbitLink);
  else
    throw std::domain_error("constructing a family of unknown type");
}

Eigen::ArrayXd Link::computeMean(const Eigen::ArrayXd& linearPredictor) const
{
  return link_->computeMean(linearPredictor);
}

Eigen::ArrayXd Link::computeMeanDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  return link_->computeMeanDerivative(linearPredictor);
}

Eigen::ArrayXd Link::computeMeanSecondDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  return link_->computeMeanSecondDerivative(linearPredictor);
}

Eigen::ArrayXd Link::computeMeanThirdDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  return link_->computeMeanThirdDerivative(linearPredictor);
}

Eigen::ArrayXd Link::computeMeanFourthDerivative(const Eigen::ArrayXd& linearPredictor) const
{
  return link_->computeMeanFourthDerivative(linearPredictor);
}


std::string Link::getName() const
{
  return link_->getName();
}
