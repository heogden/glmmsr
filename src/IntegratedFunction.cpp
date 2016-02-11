#include "subsetEigen.h"

#include "IntegratedFunction.h"

typedef Eigen::VectorXd Vector;
typedef Eigen::MatrixXd Matrix;

IntegratedFunction::IntegratedFunction(int integrateOver,
				       ScalarFun function,
				       VectorFun derivativeFunction,
				       MatrixFun secondDerivativeFunction,
				       const MultiNormal& normal): 
  integrateOver_(integrateOver), 
  function_(function), 
  derivativeFunction_(derivativeFunction),
  secondDerivativeFunction_(secondDerivativeFunction),
  normal_(normal),
  mean1_(0),
  mean2_(normal.getMean().size() - 1),
  cross_(normal.getMean().size() - 1),
  sigmaConditional_(0)
{
  initializeNormal();
  initializeConditionalTerms();
}

double IntegratedFunction::evaluate(const Vector& x, 
				    const QuadratureRule& rule) const
{
  auto conditionalMean = findConditionalMean(x);
  auto integral = integrateNoLog(x, conditionalMean, rule);
  return log(integral) + logValueAtMean_;
}
  
Vector IntegratedFunction::evaluateDerivative(const Vector& x,
					      const QuadratureRule& rule) const
{
  auto conditionalMean = findConditionalMean(x);
  double integral = integrateNoLog(x, conditionalMean, rule);
  Vector integralDerivative = integrateDerivativeNoLog(x, conditionalMean,
						       rule);
  return integralDerivative / integral;
}
  
Matrix IntegratedFunction::evaluateSecondDerivative(const Vector& x,
							     const QuadratureRule& rule) const
{
  auto conditionalMean = findConditionalMean(x);
  double integral = integrateNoLog(x, conditionalMean, rule);
  Vector integralDerivative = 
    integrateDerivativeNoLog(x, conditionalMean, rule);
  Matrix integralSecondDerivative = 
    integrateSecondDerivativeNoLog(x, conditionalMean, rule);
  Matrix numerator =  integral * integralSecondDerivative -
    integralDerivative * integralDerivative.transpose();
  return numerator / pow(integral, 2);
}

void IntegratedFunction::initializeNormal()
{
  if(!normal_.isProper()) {
    auto d = normal_.getMean().size();
    normal_.setPrecision(Eigen::MatrixXd::Identity(d, d));
  }

  logValueAtMean_ = function_(normal_.getMean());
}

void IntegratedFunction::initializeConditionalTerms()
{
   auto variance = normal_.getVariance();
   if(variance.size() > 1) {
     auto variance22 = getMatrixWithout(integrateOver_, variance);

     auto variance21 = getVectorWithout(integrateOver_, 
					variance.row(integrateOver_));
     auto variance11 = variance(integrateOver_, integrateOver_);

     cross_ = variance22.inverse() * variance21;
    
     double varianceConditional = variance11 - 
       (variance21.transpose() * cross_)(0, 0);
     sigmaConditional_ = sqrt(varianceConditional);
   }
   else {
     sigmaConditional_ = sqrt(variance(0, 0));
   }

   auto mean = normal_.getMean();
   mean1_ = mean(integrateOver_);
   mean2_ = getVectorWithout(integrateOver_, mean);
}

double IntegratedFunction::findConditionalMean(const Vector& x) const
{
  double mu = mean1_;
  if(x.size() > 0)
    mu -= cross_.dot(x - mean2_);
  
  return mu;
}

Vector IntegratedFunction::extendArgument(const Vector& x) const
{
  Vector xExt(x.size() + 1);
  setVectorWithout(integrateOver_, xExt, x);
  return xExt;
}

double IntegratedFunction::integrateNoLog(const Vector& x,
					  double conditionalMean,
					  const QuadratureRule& rule) const
{
  auto xExt = extendArgument(x);
  auto f = [&xExt, this](double t){
    xExt(integrateOver_) = t;
    return exp(function_(xExt) - logValueAtMean_);
  };
  return integrate1D(f, conditionalMean, sigmaConditional_, rule);
}

Vector IntegratedFunction::integrateDerivativeNoLog
(const Vector& x,
 double conditionalMean,
 const QuadratureRule& rule) const
{
  auto xExt = extendArgument(x);
  auto fDerivative = [&xExt, this](double t){
    xExt(integrateOver_) = t;
    double value = exp(function_(xExt) - logValueAtMean_);
    Vector derivative =  derivativeFunction_(xExt) * value;
    Vector derivativeRem = getVectorWithout(integrateOver_,
					    derivative);
    Vector correction = -cross_ * derivative(integrateOver_);
    Eigen::ArrayXd ret = (derivativeRem + correction).array();
    return ret;
  };
  auto d = x.size();
  auto ret = integrate1DVector(fDerivative, d, conditionalMean,
			       sigmaConditional_, rule);
  return ret.matrix();
}

Matrix IntegratedFunction::integrateSecondDerivativeNoLog
         (const Vector& x,
          double conditionalMean,
          const QuadratureRule& rule) const
{
  auto xExt = extendArgument(x);
  auto fSecondDerivative = [&xExt, this](double t){
    xExt(integrateOver_) = t;
    double value = exp(function_(xExt) - logValueAtMean_);
    Vector derivativeLog =  derivativeFunction_(xExt);
    Matrix secondDerivative 
    = (secondDerivativeFunction_(xExt) + 
       derivativeLog * derivativeLog.transpose()) * value;
    Matrix secondDerivative22 = getMatrixWithout(integrateOver_,
						 secondDerivative);
    Vector secondDerivative12 
     = getVectorWithout(integrateOver_,
			secondDerivative.row(integrateOver_));
			
    double secondDerivative11 = secondDerivative(integrateOver_,
						 integrateOver_);
    Matrix ret = secondDerivative22;
    ret -= cross_ * secondDerivative12.transpose();
    ret -= secondDerivative12 * cross_.transpose();
    ret += cross_ * cross_.transpose() * secondDerivative11;
    Eigen::ArrayXXd retArray = ret.array();
    return retArray;
  };
  auto d = x.size();
  auto ret = integrate1DMatrix(fSecondDerivative, d, conditionalMean,
			       sigmaConditional_, rule);
  return ret.matrix();
}
