#ifndef GUARD_INTEGRATEDFUNCTION_H
#define GUARD_INTEGRATEDFUNCTION_H

#include "MultiNormal.h"

#include "QuadratureRule.h"

class IntegratedFunction
{
  typedef Eigen::VectorXd Vector;
  typedef Eigen::MatrixXd Matrix;

  typedef std::function<double(const Vector&)> ScalarFun;
  typedef std::function<Vector(const Vector&)> VectorFun;
  typedef std::function<Matrix(const Vector&)> MatrixFun;

 public:
  IntegratedFunction(int, ScalarFun, VectorFun, MatrixFun, 
		     const MultiNormal&);

  double evaluate(const Vector&, const QuadratureRule&) const;
  Vector evaluateDerivative(const Vector&,
			    const QuadratureRule&) const;
  Matrix evaluateSecondDerivative(const Vector&,
				  const QuadratureRule&) const;

 private:
  int integrateOver_;

  ScalarFun function_;
  VectorFun derivativeFunction_;
  MatrixFun secondDerivativeFunction_;

  double logValueAtMean_;

  MultiNormal normal_;

  double mean1_;
  Vector mean2_;
  Vector cross_;
  double sigmaConditional_;
  
  void initializeNormal();
  void initializeConditionalTerms();

  double findConditionalMean(const Vector&) const;

  Vector extendArgument(const Vector&) const;

  double integrateNoLog(const Vector&,
			double,
			const QuadratureRule&) const;
  Vector integrateDerivativeNoLog(const Vector&,
				  double,
				  const QuadratureRule&) const;
  Matrix integrateSecondDerivativeNoLog(const Vector&,
				        double,
					const QuadratureRule&) const;
  
};

#endif // GUARD_INTEGRATEDFUNCTION_H
