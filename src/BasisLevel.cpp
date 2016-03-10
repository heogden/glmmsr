#include <vector>

#include <boost/math/distributions/normal.hpp>

#include "BasisLevel.h"


double BasisLevel::getKnot(int position) const
{
  return knots_[position];
}

BasisLevel::BasisLevel()
{
}

BasisLevel::BasisLevel(int level, int levelMax):
  splineCoefficients_(4 * (1 << (1 + level)), 1 << level),
  scale_(1.0 + (levelMax / 2.0))
{
  level_ = level;
  initializeBandwidth(levelMax);
  initializeKnots();
  initializeGridIds();
  initializeSplineCoefficients();
}

double BasisLevel::evaluate(double x, int position) const
{
  int intervalId = findIntervalId(x);
  double ret = 0;
  double xToPower = 1.0;
  if(level_ > 0) {
    for(int i = 0; i != 4; ++i) {
      ret += splineCoefficients_(4 * intervalId + i, position) * xToPower;
      xToPower *= x;
    }
  } else {
    ret = 1;
  }
  return ret;
}

double BasisLevel::evaluateDerivative(double x, int position) const
{
 int intervalId = findIntervalId(x);
  double ret = 0;
  double xToPower = 1.0;
  if(level_ > 0) {
    for(int i = 1; i != 4; ++i) {
       ret += splineCoefficients_(4 * intervalId + i, position) * i * xToPower;
       xToPower *= x;
    }
  }
  return ret;
}

double BasisLevel::evaluateSecondDerivative(double x, int position) const
{
  int intervalId = findIntervalId(x);
  double ret = 0;
  double xToPower = 1.0;
  if(level_ > 0) {
    for(int i = 2; i != 4; ++i) {
      ret += splineCoefficients_(4 * intervalId + i, position) * i * (i-1) * xToPower;
      xToPower *= x;
    }
  }
  return ret;
}

void BasisLevel::initializeBandwidth(int levelMax)
{
  if(levelMax > 0) {
     double prob = 0.5 + pow(2., -levelMax - 1);
     boost::math::normal standardNormal(0.0, 1.0);
     bandwidth_ = (scale_ * quantile(standardNormal, prob)) / 10;
  } else {
    bandwidth_ = 0;
  }
}


void BasisLevel::initializeKnots()
{
  int N = (1 << level_);
  double prob;
  double knot;
  int knotId;
  double knotToGrid;
  boost::math::normal standardNormal(0.0, 1.0);
  for(int i = 0; i < N; ++i) {
    prob = (2.0 * i + 1.0) / (2.0 * N);
    knot = scale_ * quantile(standardNormal, prob);
    knotId = knot / bandwidth_;
    knotToGrid = knotId * bandwidth_;
    knots_.push_back(knotToGrid);
  }
}

void BasisLevel::initializeGridIds()
{
  double prob;
  double knot;
  int knotId;
  int knotIdPrev = 0;
  int numKnots = (2 << level_) - 1;
  boost::math::normal standardNormal(0.0, 1.0);
  for(int i = 0; i < numKnots; ++i) {
    prob = (i + 1.0) / (numKnots + 1.0);
    knot = scale_ * quantile(standardNormal, prob);
    knotId = knot / bandwidth_;
    for(int j = 0; j < (knotId - knotIdPrev); ++j)
      gridIds_.push_back(i);
    knotIdPrev = knotId;
  }
}

void BasisLevel::initializeSplineCoefficients()
{
  // find all of the knots at level <= level_, sorted
  auto knotsTotal = findKnotsTotal();
  auto numKnots = knotsTotal.size();
  auto numIntervals = numKnots + 1;
  Eigen::MatrixXd A = Eigen::MatrixXd::Zero(4 * (numKnots - 1), 4 * (numKnots - 1));
  Eigen::MatrixXd B = Eigen::MatrixXd::Zero(4 * (numKnots - 1), numKnots);
  Eigen::MatrixXd coefficientsAllKnots = Eigen::MatrixXd::Zero(4 * numIntervals, numKnots);
  if(level_ > 1) {
    for(auto i = numKnots - numKnots; i < (numKnots - 1); ++i) {
      // spline interpolates values at knots
      for(auto j = 0; j < 4; ++j) {
	A(2 * i, 4 * i + j) = pow(knotsTotal[i], j);
	A(2 * i + 1, 4 * i + j) = pow(knotsTotal[i+1], j);
      }
      B(2 * i, i) = 1;
      B(2 * i + 1, i + 1) = 1;
    }
    for(auto i = numKnots - numKnots + 1; i < (numKnots - 1); ++i) {
      int rowFirst = 2 * (numKnots - 1) + i - 1;
      int rowSecond = rowFirst + numKnots - 2;
      // first and second derivatives at internal knots match
      for(int j = 1; j < 4; ++j) {
	int colAbove = 4 * (i - 1) + j;
	int colBelow = 4 * i + j;
	A(rowFirst, colAbove) = j * pow(knotsTotal[i], j - 1);
	A(rowFirst, colBelow) =  - A(rowFirst, colAbove);
	if(j > 1) {
	  A(rowSecond, colAbove) = j * (j - 1) * pow(knotsTotal[i], j - 2);
	  A(rowSecond, colBelow) = -A(rowSecond, colAbove);
	}
      }
    }
    // use end conditions of Forsythe, Malcolm and Moler
    // fix third derivatives...
    A(4 * numKnots - 6, 3) = 6;
    A(4 * numKnots - 5, 4 * (numKnots - 2) + 3) = 6;
    //.. to equal the third derivatives of cubics 
    // fitted through first four and last four knots
    Eigen::MatrixXd InitialKnotsPowers(4, 4);
    Eigen::MatrixXd FinalKnotsPowers(4, 4);
    for(auto i = 0; i < 4; ++i) {
      for(auto j = 0; j < 4; ++j) {
	InitialKnotsPowers(i, j) = pow(knotsTotal[i], j);
	FinalKnotsPowers(i, j) = pow(knotsTotal[numKnots - 4 + i], j);
      }
    }
    Eigen::MatrixXd InitialKnotsPowersInverse = InitialKnotsPowers.inverse();
    Eigen::MatrixXd FinalKnotsPowersInverse = FinalKnotsPowers.inverse();
    for(auto i = 0; i < 4; ++i) {
      B(4 * numKnots - 6, i) = 6 * InitialKnotsPowersInverse(3, i);
      B(4 * numKnots - 5, numKnots - 4 + i) = 6 * FinalKnotsPowersInverse(3, i);
    }
    Eigen::MatrixXd coefficientsInternal = A.colPivHouseholderQr().solve(B); 
    coefficientsAllKnots << coefficientsInternal.topRows(4),
      coefficientsInternal,
      coefficientsInternal.bottomRows(4);
  }
  else {
    if(level_ == 0) {
      // fit a constant function
      for(auto i = numIntervals - numIntervals; i < numIntervals; ++i)
    	coefficientsAllKnots(4 * i, 0) = 1;
    }
    if(level_ == 1) {
      // fit a quadratic
      Eigen::MatrixXd knotPowers(3, 3);
      for(auto i = 0; i < 3; ++i)
    	for(auto j = 0; j < 3; ++j)
    	  knotPowers(i, j) = pow(knotsTotal[i], j);
      Eigen::MatrixXd knotPowerInverse = knotPowers.inverse();
      for(auto i = numIntervals - numIntervals; i < numIntervals; ++i) {
    	for(auto j = 0; j < 3; ++j) {
    	  coefficientsAllKnots.row(4 * i + j) = knotPowerInverse.row(j);
    	}
      }
    }
  }
  // only keep coefficients for knots at this level
  for(auto i = 0; i < splineCoefficients_.cols(); ++i) {
    splineCoefficients_.col(i) << coefficientsAllKnots.col(2*i);
  }
}

int BasisLevel::findIntervalId(double x) const
{
  int ret = 0;
  if(x > knots_.at(0)) {
     unsigned int xToGrid = (x - knots_.at(0)) / bandwidth_;
     if(xToGrid < gridIds_.size())
       ret = gridIds_[xToGrid];
     else
       ret = (2 << level_) - 1;
  }
  return ret;
}

std::vector<double> BasisLevel::findKnotsTotal() const
{ 
  double prob;
  double knot;
  int knotId;
  double knotToGrid;
  std::vector<double> knotsTotal;
  boost::math::normal standardNormal(0.0, 1.0);
  int numKnots = (2 << level_) - 1;
  for(int i = 0; i < numKnots; ++i) {
    prob = (i + 1.0) / (numKnots + 1.0);
    knot = scale_ * quantile(standardNormal, prob);
    knotId = knot / bandwidth_;
    knotToGrid = knotId * bandwidth_;
    knotsTotal.push_back(knotToGrid);
  }
  return knotsTotal;
}

