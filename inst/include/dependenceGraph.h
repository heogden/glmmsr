#ifndef GUARD_DEPENDENCEGRAPH_H
#define GUARD_DEPENDENCEGRAPH_H

#include "Graph.h"
#include "itemsHelper.h"

template <typename T>
Graph makeDependenceGraph(const std::vector<T>&);

// ----------------------------------------------------
// implementations:

template <typename T>
Graph makeDependenceGraph(const std::vector<T>& beliefs)
{
  std::vector<int> items;
  
  for(auto& belief : beliefs)
    items = findItemsUnion(items, belief.getItems());

  Graph dependenceGraph(items);
  
  for(auto& belief : beliefs) {
    dependenceGraph.connect(belief.getItems());  
  }
  return dependenceGraph;
}

#endif // GUARD_DEPENDENCEGRAPH_H
