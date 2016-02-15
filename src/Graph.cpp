#include "Graph.h"

Graph::Graph(const std::vector<int>& items): items_(items),
					     numVertices_(items.size()),
					     isActive_(items.size(), true),
					     graph_(items.size())
{
  initializePositions();
}

void Graph::addEdge(int i, int j)
{
  int ri = positions_.at(i);
  int rj = positions_.at(j);
  boost::add_edge(ri, rj, graph_);
}

void Graph::removeNode(int i)
{
  int ri = positions_.at(i);

  boost::graph_traits<InternalGraph>::adjacency_iterator wi, wi_end;
  // remove all edges incident to i
  for(boost::tie(wi, wi_end) = adjacent_vertices(ri, graph_);
      wi != wi_end; ++wi)
    boost::remove_edge(ri, *wi, graph_);

  isActive_.at(ri) = false;
  --numVertices_;
}

void Graph::connect(const std::vector<int>& items)
{
   for (auto i = items.begin(); i != items.end(); ++i) {
    auto j = i;
    j++;
    for(; j != items.end(); ++j)
      addEdge(*i, *j);
  }
}

int Graph::numVertices() const
{
  return numVertices_;
}

int Graph::numEdges() const
{
  return boost::num_edges(graph_);
}

std::vector<int> Graph::getItems() const
{
  std::vector<int> items;
  for(auto i = items_.size() - items_.size(); i != items_.size(); ++i) {
    if(isActive_.at(i))
      items.push_back(items_.at(i));
  }
  return items;
}

std::vector<int> Graph::findNeighbours(int i) const
{
  boost::graph_traits<InternalGraph>::adjacency_iterator wi, wi_end;
  std::vector<int> neighbours;
  int ri = positions_.at(i);
  
  for(boost::tie(wi, wi_end) = adjacent_vertices(ri, graph_);
      wi != wi_end; ++wi)
      neighbours.push_back(items_.at(*wi));

  return neighbours;
}


void Graph::initializePositions()
{
  for(unsigned int i = 0; i != items_.size(); ++i)
    positions_[items_.at(i)] = i;
}


bool isComplete(const Graph& graph)
{
  int possibleEdges = graph.numVertices() * (graph.numVertices() - 1) / 2;
  return (graph.numEdges() == possibleEdges);
}



