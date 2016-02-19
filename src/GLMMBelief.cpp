#include "Parameters.h"
#include "matrixHelper.h"

#include "GLMMBelief.h"

GLMMBelief::GLMMBelief()
{
}

GLMMBelief::GLMMBelief(const std::vector<int>& items,
		       const Eigen::MatrixXd& X,
		       const Eigen::MatrixXd& Zt,
		       const Eigen::SparseMatrix<double>& Lambdat,
		       const Eigen::VectorXi& Lind,
		       const Eigen::ArrayXd& response,
		       const Eigen::ArrayXd& weights):
  ContinuousBeliefBase(items),
  numObservations_(response.size()), numFixed_(X.cols()),
  X_(X), Zt_(Zt), Lambdat_(Lambdat), Lind_(Lind),
  response_(response), weights_(weights)
{
  initializeParameterDependents();
}

double GLMMBelief::evaluate(const Eigen::VectorXd& x,
			    const Parameters& parameters)
{
  setTheta(parameters.GLMMTheta);
  setBeta(parameters.GLMMBeta);
  auto family = parameters.GLMMFamily;
  auto linearPredictor = computeLinearPredictor(x);
  return family.evaluate(linearPredictor, response_, weights_);
}

Eigen::VectorXd GLMMBelief::evaluateDerivative(const Eigen::VectorXd& x,
					       const Parameters& parameters)
{
  setTheta(parameters.GLMMTheta);
  setBeta(parameters.GLMMBeta);
  auto family = parameters.GLMMFamily;
  auto linearPredictor = computeLinearPredictor(x);
  return LambdatThetaZt_ * family.evaluateDerivative(linearPredictor, response_, weights_).matrix();
}

Eigen::MatrixXd GLMMBelief::evaluateSecondDerivative(const Eigen::VectorXd& x,
						     const Parameters& parameters)
{
  setTheta(parameters.GLMMTheta);
  setBeta(parameters.GLMMBeta);
  auto family = parameters.GLMMFamily;
  auto linearPredictor = computeLinearPredictor(x);
  return LambdatThetaZt_ * family.evaluateSecondDerivative(linearPredictor, response_, weights_).matrix().asDiagonal() * LambdatThetaZt_.transpose();
}

void GLMMBelief::setTheta(const Eigen::VectorXd& theta)
{
  if(theta.size() != theta_.size() || theta != theta_) {
    theta_ = theta;
    setLambdatThetaZt(theta);
  }
}

void GLMMBelief::setBeta(const Eigen::VectorXd& beta)
{
  if(beta.size() != beta_.size() || beta != beta_) {
    beta_ = beta;
    XBeta_ = X_ * beta_;
  }
}

void GLMMBelief::initializeParameterDependents()
{
  theta_ = Eigen::VectorXd::Zero(numItems_);
  beta_ = Eigen::VectorXd::Zero(numFixed_);
  setLambdatThetaZt(theta_);
  XBeta_ = X_ * beta_;
}

void GLMMBelief::setLambdatThetaZt(const Eigen::VectorXd& theta)
{
  Eigen::SparseMatrix<double> LambdatTheta(Lambdat_);
  int index = 0;
  for (int j = 0; j < LambdatTheta.outerSize(); ++j)
    for (Eigen::SparseMatrix<double>::InnerIterator i(LambdatTheta, j); i; ++i) {
      i.valueRef() = theta(Lind_(index));
      index++;
    }
  LambdatThetaZt_ = LambdatTheta * Zt_;
}

Eigen::ArrayXd GLMMBelief::computeLinearPredictor(const Eigen::VectorXd& x) const
{
  return (XBeta_ + LambdatThetaZt_.transpose() * x).array();
}
