#include "itemsHelper.h"
#include "BeliefBase.h"

BeliefBase::BeliefBase(): numItems_(0), items_(0)
{
}

BeliefBase::BeliefBase(const std::vector<int>& items):
  numItems_(items.size()), items_(items)
{
}


std::vector<int> BeliefBase::getItems() const
{
  return items_;
}

int BeliefBase::width() const
{
  return items_.size();
}


bool BeliefBase::contains(const BeliefBase& other) const
{
  return isSubset(other.items_, items_);
}


bool isSubset(const BeliefBase& A, const BeliefBase& B)
{
  return B.contains(A);
}
