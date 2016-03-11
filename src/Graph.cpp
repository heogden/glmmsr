#include "Graph.h"


Graph::Graph(const std::vector<int>& items): numVertices_(0),
					     numEdges_(0)
{
  for(const auto& item : items) {
    addNode(item);
  }
}

void Graph::addNode(int item)
{
  auto v = boost::add_vertex(graph_);
  graph_[v].item = item;
  graph_[v].active = true;
  positions_[item] = v;
  numVertices_++;
}

void Graph::removeNode(int i)
{
  auto ri = positions_.at(i);
  if(graph_[ri].active) {
    auto di = findDegree(i);
    graph_[ri].active = false;
    numVertices_--;
    numEdges_ -= di;
  }
}

void Graph::addEdge(int i, int j)
{
  auto ri = positions_.at(i);
  auto rj = positions_.at(j);

  // make sure the vertices are active
  if(!graph_[ri].active) {
    graph_[ri].active = true;
    numVertices_++;
  }
  if(!graph_[rj].active) {
    graph_[rj].active = true;
    numVertices_++;
  }

  bool edgeExists = boost::edge(ri, rj, graph_).second;
  if(!edgeExists) {
    boost::add_edge(ri, rj, graph_);
    numEdges_++;
  }
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
  return numEdges_;
}

std::vector<int> Graph::getItems() const
{
  std::vector<int> items;
  // iterate through all vertices, only record active
  VertexIter wi, wi_end;
  for(boost::tie(wi, wi_end) = vertices(graph_);
      wi != wi_end; ++wi) {
    if(graph_[*wi].active)
      items.push_back(graph_[*wi].item);
  }

  return items;
}

int Graph::findDegree(int i) const
{
  int degree = 0;
  AdjIter wi, wi_end;
  auto ri = positions_.at(i);

  for(boost::tie(wi, wi_end) = adjacent_vertices(ri, graph_);
      wi != wi_end; ++wi) {
    if(graph_[*wi].active) {
      degree++;
    }
  }
  return degree;
}

std::vector<int> Graph::findNeighbours(int i) const
{
  AdjIter wi, wi_end;
  std::vector<int> neighbours;
  auto ri = positions_.at(i);

  for(boost::tie(wi, wi_end) = adjacent_vertices(ri, graph_);
      wi != wi_end; ++wi) {
    if(graph_[*wi].active) {
      neighbours.push_back(graph_[*wi].item);
    }
  }

  return neighbours;
}

bool isComplete(const Graph& graph)
{
  int possibleEdges = graph.numVertices() * (graph.numVertices() - 1) / 2;
  return (graph.numEdges() == possibleEdges);
}



