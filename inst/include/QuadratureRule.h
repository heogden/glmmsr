#ifndef GUARD_QUADRATURERULE_H
#define GUARD_QUADRATURERULE_H

#include <functional>

#include <RcppEigen.h>

class QuadratureRule
{
 public:
  QuadratureRule();
  QuadratureRule(int);

  Eigen::ArrayXd getNodes() const;
  Eigen::ArrayXd getWeights() const;

 private:
  Eigen::ArrayXd nodes_;
  Eigen::ArrayXd weights_;

  void initialize(int);
  Eigen::MatrixXd findHermiteJacobi(int);
};

double integrate1D(std::function<double(double)>, 
		   double, double,
		   const QuadratureRule&);

Eigen::ArrayXd integrate1DVector(std::function<Eigen::ArrayXd(double)>,
				 int, double, double,
				 const QuadratureRule&);

Eigen::ArrayXXd integrate1DMatrix(std::function<Eigen::ArrayXXd(double)>,
				  int, double, double,
				  const QuadratureRule&);

#endif // GUARD_QUADRATURERULE_H
