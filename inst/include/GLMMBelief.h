#ifndef GUARD_GLMMBELIEF_H
#define GUARD_GLMMBELIEF_H

#include <vector>
#include <RcppEigen.h>

#include "ContinuousBeliefBase.h"
#include "Parameters.h"

/// Generalized Linear Mixed Model belief
class GLMMBelief : public ContinuousBeliefBase
{
 public:
  GLMMBelief();
  GLMMBelief(const std::vector<int>&,
	    const Eigen::MatrixXd&,
	    const Eigen::MatrixXd&,
	    const Eigen::SparseMatrix<double>&,
	    const Eigen::VectorXi&,
	    const Eigen::ArrayXd&,
	    const Eigen::ArrayXd&);

  double evaluate(const Eigen::VectorXd&, const Parameters&);
  Eigen::VectorXd evaluateDerivative(const Eigen::VectorXd&,
				     const Parameters&);
  Eigen::MatrixXd evaluateSecondDerivative(const Eigen::VectorXd&,
					   const Parameters&);
	
 private:
  int numObservations_;
  int numFixed_;
  Eigen::MatrixXd X_;
  Eigen::MatrixXd Zt_;
  Eigen::SparseMatrix<double> Lambdat_;
  Eigen::VectorXi Lind_;
  Eigen::ArrayXd response_;
  Eigen::ArrayXd weights_;

  Eigen::VectorXd theta_;
  Eigen::VectorXd beta_;

  Eigen::MatrixXd LambdatThetaZt_;
  Eigen::MatrixXd XBeta_;

  void setTheta(const Eigen::VectorXd&);
  void setBeta(const Eigen::VectorXd&);

  void initializeParameterDependents();
  
  void setLambdatThetaZt(const Eigen::VectorXd&);
  Eigen::ArrayXd computeLinearPredictor(const Eigen::VectorXd&) const;


};

#endif // GUARD_GLMMBELIEF_H
