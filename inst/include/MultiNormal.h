#ifndef GUARD_MULTINORMAL_H
#define GUARD_MULTINORMAL_H

#include <RcppEigen.h>

class MultiNormal
{
 public:
  MultiNormal();
  MultiNormal(int);
  MultiNormal(const Eigen::VectorXd&, const Eigen::MatrixXd&);
  MultiNormal(const Eigen::VectorXd&, const Eigen::VectorXd&, const Eigen::MatrixXd&);
  
  void initializeFromDerivatives(const Eigen::VectorXd&, const Eigen::VectorXd&, const Eigen::MatrixXd&);

  void setMean(const Eigen::VectorXd&);
  void setPrecision(const Eigen::MatrixXd&);
  void setVariance(const Eigen::MatrixXd&);

  void multiplySubset(const MultiNormal&, const std::vector<int>&);
  void divideSubset(const MultiNormal&, const std::vector<int>&);

  bool isProper() const;
  
  // evaluate log density and derivatives
  double evaluate(const Eigen::VectorXd&) const;
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&) const;
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&) const;

  Eigen::VectorXd getMean() const;
  Eigen::MatrixXd getPrecision() const;
  Eigen::MatrixXd getVariance() const;

  int dimension() const;

  MultiNormal computeMarginal(const std::vector<int>&) const;
  MultiNormal integrate(int) const;

 private:
  Eigen::VectorXd mean_;
  Eigen::MatrixXd precision_;
  Eigen::MatrixXd variance_;
  double normalizer_;

  void initializeNormalizer();
  void multiplySubsetInternal(const MultiNormal&, 
			      const std::vector<int>&,
			      bool);
};

#endif // GUARD_MULTINORMAL_H
