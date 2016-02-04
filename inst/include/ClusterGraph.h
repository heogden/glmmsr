#ifndef GUARD_CLUSTERGRAPH_H
#define GUARD_CLUSTERGRAPH_H

#include <vector>

#include "MixedContinuousBelief.h"
#include "Parameters.h"
#include "Graph.h"

class ClusterGraphEdge;

class ClusterGraph : public BeliefBase
{
  typedef ClusterGraphEdge Edge;
  
 public:
  // Constructors:
  ClusterGraph();
  ClusterGraph(const std::vector<MixedContinuousBelief>&);

  // Initializing structure of cluster graph:
  void initialize(const Graph&);

  // Populating cluster graph with beliefs:
  void populate(const std::vector<MixedContinuousBelief>&);

  void setNormalApprox(const Eigen::VectorXd&,
		       const Eigen::SparseMatrix<double>&);

  void setParameters(const Parameters&);
  
  // find marginal distribution
  MixedContinuousBelief margin(const std::vector<int>&);
  double computeLogNormalizingConstant();

  // Message passing operations:
  void calibrateForward();
  void calibrateBackward();
  void calibrate();
  
  // Reading off information about cluster graph:
  MixedContinuousBelief getCluster(int) const;
  std::vector<MixedContinuousBelief> getClusters() const;

  int nClusters() const;
  int nEdges() const;

  int width() const;
  int nMessages() const;
  int complexity() const;
  
  bool isCalibrated() const;

  std::vector<MixedContinuousBelief> extractBeliefs() const;

 private:
  std::vector<MixedContinuousBelief> clusters_;
  std::vector<Edge> edges_;

  bool isCalibrated_;
  Parameters parameters_;

  void initializeInternal(Graph&, const std::vector<int>&);
  
  void populateInternal(bool, std::vector<MixedContinuousBelief>&);
  void populateClusters(bool, std::vector<MixedContinuousBelief>&);

  void calibrateInternal(bool, bool);
  void passMessage(Edge&, bool, bool);

  void projectClusterOntoEdge(MixedContinuousBelief&, Edge&, bool);

  
};

class ClusterGraphEdge
{
  friend class ClusterGraph;
 public:
  ClusterGraphEdge();
  ClusterGraphEdge(int, int, const std::vector<int>&);  

 private:
  int firstClusterId_;
  int secondClusterId_;
  MixedContinuousBelief belief_;
};


template <typename T>
bool isSubset(std::vector<T>, std::vector<T>);

void populateBelief(MixedContinuousBelief&, bool,
		    std::vector<MixedContinuousBelief>&);

void projectMargin(MixedContinuousBelief&, MixedContinuousBelief&);

double computeLogNormalizingConstant(const ClusterGraph&,
				     const Eigen::VectorXd&,
				     const Eigen::SparseMatrix<double>&,
				     const Parameters&);

#endif // GUARD_CLUSTERGRAPH_H
