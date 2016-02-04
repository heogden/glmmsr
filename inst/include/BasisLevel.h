#ifndef GUARD_BASISLEVEL_H
#define GUARD_BASISLEVEL_H

#include <vector>

#include <RcppEigen.h>

class BasisLevel
{
  typedef Eigen::MatrixXd SplineCoefficients;
  
 public:
  BasisLevel();
  BasisLevel(int, int);
  
  double evaluate(double, int) const;
  double evaluateDerivative(double, int) const;
  double evaluateSecondDerivative(double, int) const;
  
  double getKnot(int) const;
  int findIntervalId(double) const;

 private:
  int level_;
  std::vector<double> knots_;
  SplineCoefficients splineCoefficients_;
  double scale_;
  double bandwidth_;
  std::vector<int> gridIds_;

  void initializeBandwidth(int);
  void initializeKnots();
  void initializeGridIds();
  void initializeSplineCoefficients();
  
  std::vector<double> findKnotsTotal() const;
};

#endif // GUARD_BASISLEVEL_H
