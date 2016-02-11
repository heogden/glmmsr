#include <math.h>

#include "IntegratedFunction.h"
#include "subsetEigen.h"

#include "itemsHelper.h"
#include "matrixHelper.h"

#include "MixedContinuousBelief.h"

MixedContinuousBelief::MixedContinuousBelief():
  beliefs_(0), isInverted_(0), isNormalFixed_(false)
{
}

MixedContinuousBelief::MixedContinuousBelief(const std::vector<int>& items):
  BeliefBase(items), beliefs_(0), isInverted_(0), normalApprox_(items),
  isNormalFixed_(false)
{
}

MixedContinuousBelief::MixedContinuousBelief(const ContinuousBelief&
					     belief):
  BeliefBase(belief.getItems()), beliefs_(0), isInverted_(0),
  normalApprox_(belief.getItems()), isNormalFixed_(false)
{
    beliefs_.push_back(belief);
    isInverted_.push_back(false);
    recomputeRelativeItems();
}

void MixedContinuousBelief::setItems(const std::vector<int>& items)
{
  items_ = items;
  normalApprox_.setItems(items);
  recomputeRelativeItems();

}

void MixedContinuousBelief::setNormalApprox
(const Eigen::VectorXd& mean, const Eigen::MatrixXd& precision)
{
  if(!isNormalFixed_)
    normalApprox_ = NormalBelief(items_, mean, precision);
}

void MixedContinuousBelief::setNormalApprox
(const NormalBelief& normalApprox)
{
  if(!isNormalFixed_)
    normalApprox_ = normalApprox;
}

void MixedContinuousBelief::fixNormalApprox()
{
  isNormalFixed_ = true;
}

MixedContinuousBelief MixedContinuousBelief::integrate
(int item, const Parameters& parameters)
{
  auto normalApprox = normalApprox_.getNormal();
  
  int relativeItem = findRelativeItem(item, items_);
  auto eval = [this, &parameters](const Eigen::VectorXd& x) {
    return evaluate(x, parameters);
  };
  auto deriv = [this, &parameters](const Eigen::VectorXd& x) {
    return evaluateDerivative(x, parameters);
  };
  auto secondDeriv = [this, &parameters](const Eigen::VectorXd& x) {
    return evaluateSecondDerivative(x, parameters);
  };
  IntegratedFunction integratedBelief(relativeItem, eval, 
				      deriv, secondDeriv, 
				      normalApprox);

  auto integral = [&integratedBelief, &parameters]
    (const Eigen::VectorXd& x) {
    return integratedBelief.evaluate
    (x, parameters.quadratureRule);
  };
  auto integralDeriv = [&integratedBelief, &parameters]
    (const Eigen::VectorXd& x) {
    return integratedBelief.evaluateDerivative
    (x, parameters.quadratureRule);
  };
  auto integralSecondDeriv = [&integratedBelief, &parameters]
    (const Eigen::VectorXd& x) {
    return integratedBelief.evaluateSecondDerivative
    (x, parameters.quadratureRule);
  };
  auto remainingItems = items_;
  remainingItems.erase(remainingItems.begin() + relativeItem);

  // auto normalMargin = normalBelief.margin(remainingItems);
  auto normalMargin = normalApprox.integrate(relativeItem);

  ContinuousBelief belief(remainingItems,
			  integral, integralDeriv, 
			  integralSecondDeriv, normalMargin,
			  parameters.sparseBasis);

  MixedContinuousBelief marginalBelief(belief);
  marginalBelief.setNormalApprox(normalApprox_.margin(remainingItems));
  return marginalBelief;
}

MixedContinuousBelief MixedContinuousBelief::margin
(const std::vector<int>& items,
 bool normalApproxOnly,
 const Parameters& parameters)
{
  MixedContinuousBelief marginalBelief(items);
  if(normalApproxOnly) {
    auto normalMargin = normalApprox_.margin(items);
    marginalBelief.setNormalApprox(normalMargin);
  }
  else {
    marginalBelief = *this;
    auto itemsIntegrate = findItemsDifference(items_, items);
    std::vector<int> integratedItems;
    auto remainingItems = items_;
    for(auto i = itemsIntegrate.begin(); i != itemsIntegrate.end(); ++i) {
      marginalBelief = marginalBelief.integrate(*i, parameters);
      integratedItems.push_back(*i);
      remainingItems = findItemsDifference(items_, integratedItems);
    }
  }

  return marginalBelief;
}

double MixedContinuousBelief::computeLogNormalizingConstant
(const Parameters& parameters)
{
  std::vector<int> itemsEmpty;
  auto beliefEmpty = margin(itemsEmpty, false, parameters);
  Eigen::VectorXd xEmpty = Eigen::VectorXd::Zero(0);
  return beliefEmpty.evaluate(xEmpty, parameters);
}

void MixedContinuousBelief::operator*=(const MixedContinuousBelief& other)
{
  if(!isNormalFixed_)
    normalApprox_ *= other.normalApprox_;

  items_ = normalApprox_.getItems();
  
  for(auto i = other.beliefs_.size() - other.beliefs_.size(); 
      i != other.beliefs_.size(); ++i) {
    beliefs_.push_back(other.beliefs_[i]);
    isInverted_.push_back(other.isInverted_[i]);
  }

  recomputeRelativeItems();
}


void MixedContinuousBelief::operator/=(const MixedContinuousBelief& other)
{
  if(!isNormalFixed_)
    normalApprox_ /= other.normalApprox_;
  
  items_ = normalApprox_.getItems();
  
  for(auto i = other.beliefs_.size() - other.beliefs_.size(); 
      i != other.beliefs_.size(); ++i) {
    beliefs_.push_back(other.beliefs_[i]);
    isInverted_.push_back(!other.isInverted_[i]);
  }
  recomputeRelativeItems();
}


double MixedContinuousBelief::evaluate(const Eigen::VectorXd& x,
				       const Parameters& parameters)
{
  double ret = 0;
  for(auto i = beliefs_.begin(); i != beliefs_.end(); ++i) {
    int index = i - beliefs_.begin();
    auto relativeItems = relativeItems_.at(index);
    double beliefValue = i->evaluate(findSubset(x, relativeItems),
				     parameters);
    if(isInverted_.at(index))
      ret -= beliefValue;
    else
      ret += beliefValue;
  }
  return ret;
}

Eigen::VectorXd MixedContinuousBelief::evaluateDerivative
(const Eigen::VectorXd& x,
 const Parameters& parameters)
{
  Eigen::VectorXd ret = Eigen::VectorXd::Zero(x.size());
  for(auto i = beliefs_.begin(); i != beliefs_.end(); ++i) {
    auto relativeItems = findRelativeItems(i->getItems(), items_);
    auto xSubset = findSubset(x, relativeItems);
    auto derivativeSubset =  i->evaluateDerivative(xSubset, 
						   parameters);
    if(isInverted_.at(i - beliefs_.begin()))
      addVectorSubset(ret, relativeItems, -derivativeSubset);
    else
      addVectorSubset(ret, relativeItems, derivativeSubset);
  }
  return ret;
}

Eigen::MatrixXd MixedContinuousBelief::evaluateSecondDerivative
(const Eigen::VectorXd& x,
 const Parameters& parameters)
{
  Eigen::MatrixXd ret = Eigen::MatrixXd::Zero(x.size(), x.size());
  for(auto i = beliefs_.begin(); i != beliefs_.end(); ++i) {
    auto relativeItems = findRelativeItems(i->getItems(), items_);
    auto xSubset = findSubset(x, relativeItems);
    auto secondDerivativeSubset =  
      i->evaluateSecondDerivative(xSubset, parameters);
    if(isInverted_.at(i - beliefs_.begin()))
      addMatrixSubset(ret, relativeItems, -secondDerivativeSubset);
    else
      addMatrixSubset(ret, relativeItems, secondDerivativeSubset);
  }
  return ret;
}

bool MixedContinuousBelief::isProper(const Parameters& parameters)
{
  bool proper = true;
  auto normalMean = normalApprox_.getNormal().getMean();
  if(beliefs_.size() > 0) {
    auto evalAtMean = evaluate(normalMean, parameters);
    if(!std::isnormal(evalAtMean)) {
      proper = false;
    }
  }
  auto evalNormalAtMean = normalApprox_.evaluate(normalMean,
						 parameters);
  if(!std::isnormal(evalNormalAtMean)) {
    proper = false;
  }
  
  return proper;
}

void MixedContinuousBelief::recomputeRelativeItems()
{
  relativeItems_.clear();
  for(auto belief : beliefs_)
    relativeItems_.push_back(findRelativeItems(belief.getItems(),
					       items_));
}
