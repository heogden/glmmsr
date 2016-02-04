#include <stdexcept>

#include "itemsHelper.h"

int findRelativeItem(int item, const std::vector<int>& items)
{
  auto relativeItem =  std::find(items.begin(), items.end(), item);
  if(relativeItem != items.end())
    return relativeItem - items.begin();
  else
     std::domain_error("item not contained in items");
  return 0;
}

std::vector<int> findRelativeItems(const std::vector<int>& subset,
				   const std::vector<int>& items)
{
  std::vector<int> relativeItems;
  for(auto i = subset.begin(); i != subset.end(); ++i) {
    relativeItems.push_back(findRelativeItem(*i, items));
  }
  return relativeItems;
}

std::vector<int> findItemsDifference(const std::vector<int>& items,
				     const std::vector<int>& subset)
{
  std::vector<int> difference;
  for(auto i = items.begin(); i != items.end(); ++i) {
    if(std::find(subset.begin(), subset.end(), *i) == subset.end())
      difference.push_back(*i);
  }
  return difference;
}

std::vector<int> findItemsUnion(const std::vector<int>& items1,
				const std::vector<int>& items2)
{
  std::vector<int> itemsUnion;
  auto first = items1;
  auto second = items2;
  std::sort(first.begin(), first.end());
  std::sort(second.begin(), second.end());
  std::set_union(first.begin(), first.end(),
		 second.begin(), second.end(),
		 back_inserter(itemsUnion));
                                              
  return itemsUnion;
}
