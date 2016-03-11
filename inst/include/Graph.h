#ifndef GUARD_GRAPH_H
#define GUARD_GRAPH_H

#include "adjacency_list_alt.h"

#include <vector>

struct vertexProperties
{
  int item;
  bool active;
};

class Graph
{
 public:
  Graph(const std::vector<int>&);

  void addNode(int);
  void removeNode(int);

  void addEdge(int, int);

  void connect(const std::vector<int>&);

  int numVertices() const;
  int numEdges() const;

  std::vector<int> getItems() const;
  
  int findDegree(int) const;
  std::vector<int> findNeighbours(int) const;
    	  
 private:
  typedef boost::adjacency_list<
    boost::vecS, 
    boost::vecS, 
    boost::undirectedS,
    vertexProperties,
    boost::no_property
    > InternalGraph;

  typedef boost::graph_traits<InternalGraph>::vertex_descriptor 
    Vertex;
  typedef boost::graph_traits<InternalGraph>::vertex_iterator
    VertexIter;
  typedef boost::graph_traits<InternalGraph>::adjacency_iterator
    AdjIter;

  std::map<int, Vertex> positions_;

  int numVertices_;
  int numEdges_;
  
  InternalGraph  graph_;

};

bool isComplete(const Graph&);

#endif // GUARD_GRAPH_H
