#include "QuadratureRule.h"

QuadratureRule::QuadratureRule(): nodes_(), weights_()
{
}

QuadratureRule::QuadratureRule(int n): nodes_(n), weights_(n)
{
  initialize(n);
}

Eigen::ArrayXd QuadratureRule::getNodes() const
{
  return nodes_;
}

Eigen::ArrayXd QuadratureRule::getWeights() const
{
  return weights_;
}

void QuadratureRule::initialize(int n)
{
  static const double sqrtPi = 1.772453851;
  Eigen::MatrixXd hermiteJacobi = findHermiteJacobi(n);
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(hermiteJacobi);
  nodes_ = es.eigenvalues().array();

  Eigen::ArrayXd firstRow = es.eigenvectors().row(0).array();
  weights_ = sqrtPi * firstRow * firstRow;
}

Eigen::MatrixXd QuadratureRule::findHermiteJacobi(int n)
{
  // Slow, because we ignore tridiagonal structure
  // but fast enough for reasonable n
  Eigen::MatrixXd hermiteJacobi = Eigen::MatrixXd::Zero(n, n);
  for(int i = 1; i < n; ++i){
    double iOver2 = i / 2.0;
    hermiteJacobi(i, i - 1) =  sqrt(iOver2);
  }
  return hermiteJacobi;
}

double integrate1D(std::function<double(double)> f,
		   double location,
		   double sigma,
		   const QuadratureRule& rule)
{
  double ret = 0;
  auto scale = sqrt(2.) * sigma;
  auto nodes = rule.getNodes();
  auto weights = rule.getWeights();
  for(auto i = 0; i < nodes.size(); ++i) {
    double node = nodes(i);
    double transformedNode = scale * node + location;
    ret += weights(i) * f(transformedNode) * exp(pow(node, 2));
  }
  return scale * ret;
}

// version for scalar input, vector output function
Eigen::ArrayXd integrate1DVector(std::function<Eigen::ArrayXd(double)> f,
				 int d,
				 double location,
				 double sigma,
				 const QuadratureRule& rule)
{
  Eigen::ArrayXd ret = Eigen::ArrayXd::Zero(d);
  auto scale = sqrt(2.) * sigma;
  auto nodes = rule.getNodes();
  auto weights = rule.getWeights();
  for(auto i = 0; i < nodes.size(); ++i) {
    double node = nodes(i);
    double transformedNode = scale * node + location;
    ret += weights(i) * f(transformedNode) * exp(pow(node, 2));
  }
  return scale * ret;
}

// version for scalar input, matrix output function
Eigen::ArrayXXd integrate1DMatrix(std::function<Eigen::ArrayXXd(double)> f,
				  int d,
				  double location,
				  double sigma,
				  const QuadratureRule& rule)
{
  Eigen::ArrayXXd ret = Eigen::ArrayXXd::Zero(d, d);
  auto scale = sqrt(2.) * sigma;
  auto nodes = rule.getNodes();
  auto weights = rule.getWeights();
  for(auto i = 0; i < nodes.size(); ++i) {
    double node = nodes(i);
    double transformedNode = scale * node + location;
    ret += weights(i) * f(transformedNode) * exp(pow(node, 2));
  }
  return scale * ret;
}
				 
