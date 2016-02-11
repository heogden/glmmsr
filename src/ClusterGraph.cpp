#include <stdexcept>

#include "subsetEigen.h"
#include "ClusterGraph.h"
#include "dependenceGraph.h"

ClusterGraphEdge::ClusterGraphEdge()
{
}

ClusterGraphEdge::ClusterGraphEdge
(int firstClusterId, int secondClusterId,
 const std::vector<int>& items):
firstClusterId_(firstClusterId), secondClusterId_(secondClusterId),
  belief_(items)
{
}

ClusterGraph::ClusterGraph(): isCalibrated_(false)
{
}

ClusterGraph::ClusterGraph
(const std::vector<MixedContinuousBelief>& beliefs)
{
  auto dependenceGraph = makeDependenceGraph(beliefs);
  initialize(dependenceGraph);
  populate(beliefs);
}


void ClusterGraph::initialize (const Graph& dependenceGraph)
{
  std::vector<int> emptyIds;
  auto dependenceGraphCopy = dependenceGraph;
  initializeInternal(dependenceGraphCopy, emptyIds);
}
 

void ClusterGraph::populate
(const std::vector<MixedContinuousBelief>& beliefs)
{
  auto copyBeliefs = beliefs;
  populateInternal(true, copyBeliefs);
}

void ClusterGraph::setNormalApprox
  (const Eigen::VectorXd& mean,
   const Eigen::SparseMatrix<double>& precision)
{
  for(auto& cluster : clusters_) {
    auto clusterItems = cluster.getItems();
    auto meanCluster = getVectorSubset(mean, clusterItems);
    auto precisionCluster =  getSparseMatrixSubset(precision, clusterItems);
    cluster.setNormalApprox(meanCluster, precisionCluster);
  }
  for(auto& edge : edges_) {
    auto edgeItems = edge.belief_.getItems();
    auto meanEdge = getVectorSubset(mean, edgeItems);
    auto precisionEdge =  getSparseMatrixSubset(precision, edgeItems);
    edge.belief_.setNormalApprox(meanEdge, precisionEdge);
  }

  calibrateInternal(true, true);
  calibrateInternal(false, true);
  for(auto& cluster : clusters_) {
    cluster.fixNormalApprox();
  }
  for(auto& edge : edges_) {
    edge.belief_.fixNormalApprox();
  }
}

void ClusterGraph::setParameters(const Parameters& parameters)
{
  parameters_ = parameters;
}

MixedContinuousBelief ClusterGraph::margin(const std::vector<int>& items)
{
  if(!isCalibrated_)
    calibrate();

  MixedContinuousBelief marginalBelief(items);

  int clusterId = 0;
  for(auto& cluster : clusters_) {
    if(isSubset(items, cluster.getItems())) {
      marginalBelief = cluster.margin(items, false, parameters_);
      break;
    }
    ++clusterId;
    if(clusterId == nClusters())
      throw std::domain_error("items not fully contained in any cluster");
  }
  return marginalBelief;
}

double ClusterGraph::computeLogNormalizingConstant()
{
  auto finalBelief = clusters_.at(nClusters() - 1);
  return finalBelief.computeLogNormalizingConstant(parameters_);
}

void ClusterGraph::calibrateForward()
{
  calibrateInternal(true, false);
}

void ClusterGraph::calibrateBackward()
{
  calibrateInternal(false, false);
}

void ClusterGraph::calibrate()
{
  calibrateForward();
  calibrateBackward();
  isCalibrated_ = true;
}

void ClusterGraph::calibrateInternal(bool forward, bool normalApproxOnly)
{
  if(forward) {
    for(auto e = edges_.begin(); e != edges_.end(); ++e)
      passMessage(*e, true, normalApproxOnly);
  } else {
    for(auto e = edges_.rbegin(); e != edges_.rend(); ++e)
      passMessage(*e, false, normalApproxOnly);
  }  
}


void ClusterGraph::passMessage(Edge& edge,
			       bool forward,
			       bool normalApproxOnly)
{
  int i, j;
  if(forward) {
    i = edge.firstClusterId_;
    j = edge.secondClusterId_;
  } else {
    i = edge.secondClusterId_;
    j = edge.firstClusterId_;
  }

  auto oldEdgeBelief = edge.belief_;
  
  projectClusterOntoEdge(clusters_.at(i), edge, normalApproxOnly);

  clusters_.at(j) *= edge.belief_;
  clusters_.at(j) /= oldEdgeBelief;

  if(!clusters_.at(j).isProper(parameters_)) {
    throw std::runtime_error("improper cluster belief after passing message");
  }
}


MixedContinuousBelief ClusterGraph::getCluster(int i) const
{
  return clusters_.at(i);
}

std::vector<MixedContinuousBelief> ClusterGraph::getClusters() const
{
  return clusters_;
}


int ClusterGraph::nClusters() const
{
  return clusters_.size();
}


int ClusterGraph::nEdges() const
{
  return edges_.size();
}

int ClusterGraph::width() const
{
  int width = 0;
  for(auto& cluster : clusters_)
    width = std::max(width, cluster.width());
  return width;
}

int ClusterGraph::nMessages() const
{
  return edges_.size();
}

int ClusterGraph::complexity() const
{
  int complexity = 0;
  /* for(auto& cluster : clusters_) */
  /*   complexity += cluster.complexity(); */
  return complexity;
}

bool ClusterGraph::isCalibrated() const
{
  return isCalibrated_;
}

std::vector<MixedContinuousBelief> ClusterGraph::extractBeliefs() const
{
  auto beliefs = clusters_;
  for(auto& edge : edges_) {
    beliefs.at(edge.secondClusterId_) /= edge.belief_;
  }
  return beliefs;
}

void ClusterGraph::initializeInternal(Graph& dependenceGraph,
				      const std::vector<int>& hangingEdgeIds)
{
  if(isComplete(dependenceGraph)) {
    for(auto edgeId : hangingEdgeIds)
      edges_.at(edgeId).secondClusterId_ = clusters_.size();
    clusters_.push_back(dependenceGraph.getItems());
  }
  else {
    auto remainingItems = dependenceGraph.getItems();

    std::vector<int> candidates;
    candidates = remainingItems;
      
    // choose an item to eliminate
    int minSeparatorSize = remainingItems.size();
    int itemToEliminate = candidates.at(0);
    
    for(auto candidate : candidates) {
      auto candidateSeparator = dependenceGraph.findNeighbours(candidate);
      int separatorSize = candidateSeparator.size();

      if(separatorSize < minSeparatorSize) {
	itemToEliminate = candidate;
	minSeparatorSize = separatorSize;
      }
    }

    auto separator = dependenceGraph.findNeighbours(itemToEliminate);
    std::vector<int> itemsToEliminate = {itemToEliminate};
    auto cluster = findItemsUnion(itemsToEliminate, separator);

    auto newClusterId = clusters_.size();
    clusters_.push_back(cluster);

    std::vector<int> newHangingEdgeIds;
    auto newEdgeId = edges_.size();
    edges_.push_back(Edge(newClusterId, -1, separator));

    for(auto edgeId : hangingEdgeIds) {
      if(isSubset(edges_.at(edgeId).belief_.getItems(), cluster)) {
	edges_.at(edgeId).secondClusterId_ = newClusterId;
      } else {
	newHangingEdgeIds.push_back(edgeId);
      }
    }
    newHangingEdgeIds.push_back(newEdgeId);

    dependenceGraph.connect(separator);
    dependenceGraph.removeNode(itemToEliminate);

    initializeInternal(dependenceGraph, newHangingEdgeIds);			 
  }
}


void ClusterGraph::populateInternal
(bool multiply, std::vector<MixedContinuousBelief>& beliefs)
{
  populateClusters(multiply, beliefs);
}


void ClusterGraph::populateClusters
(bool multiply, std::vector<MixedContinuousBelief>& beliefs)
{
  for(auto& cluster : clusters_)
    populateBelief(cluster, multiply, beliefs);
  
  
  isCalibrated_ = false;
}


void ClusterGraph::projectClusterOntoEdge(MixedContinuousBelief& cluster,
					  Edge& edge,
					  bool normalApproxOnly)
{
  edge.belief_ = cluster.margin(edge.belief_.getItems(),
				normalApproxOnly,
			        parameters_);
}


void populateBelief(MixedContinuousBelief& cluster,
		    bool multiply,
		    std::vector<MixedContinuousBelief>& beliefs)
{

  std::vector<MixedContinuousBelief> containedBeliefs;

  auto isContained = [&cluster](MixedContinuousBelief& belief) {
    return isSubset(belief, cluster);
  };
  
  std::copy_if(beliefs.begin(), beliefs.end(),
	       back_inserter(containedBeliefs), isContained);
  auto newEnd = std::remove_if(beliefs.begin(), beliefs.end(), isContained);
  beliefs.resize(newEnd - beliefs.begin());

  
  for(const auto& belief : containedBeliefs) {
    if(multiply)
      cluster *= belief;
    else
      cluster /= belief;
  }

}

double computeLogNormalizingConstant
(const ClusterGraph& initialBeliefs,
 const Eigen::VectorXd& mean,
 const Eigen::SparseMatrix<double>& precision,
 const Parameters& parameters)
{
  auto beliefs = initialBeliefs;
  beliefs.setParameters(parameters);
  beliefs.setNormalApprox(mean, precision);
  beliefs.calibrateForward();
  return beliefs.computeLogNormalizingConstant();
}
