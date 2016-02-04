#ifndef GUARD_GRAPH_H
#define GUARD_GRAPH_H

#include "adjacency_list_alt.h"

#include <vector>


class Graph
{
  friend std::vector<std::vector<int>> findCliques(const Graph&);
 public:
  Graph(const std::vector<int>&);

  void addEdge(int, int);
  void removeNode(int);

  void connect(const std::vector<int>&);

  int numVertices() const;
  int numEdges() const;
  std::vector<int> getItems() const;
  
  std::vector<int> findNeighbours(int) const;
    	  
 private:
  typedef boost::adjacency_list<boost::setS, boost::vecS, boost::undirectedS> InternalGraph;
  
  std::vector<int> items_;
  std::map<int, int> positions_;

  int numVertices_;
  std::vector<bool> isActive_;
  
  InternalGraph  graph_;

  void initializePositions();
  
};

bool isComplete(const Graph&);
std::vector<std::vector<int>> findCliques(const Graph&);

#endif // GUARD_GRAPH_H
