#include "Basis.h"

Basis::Basis(): basisLevels_(), levelMax_(0)
{
}

Basis::Basis(int levelMax): basisLevels_(0), levelMax_(levelMax)
{
  for(int l = 0; l < (levelMax + 1); ++l)
    basisLevels_.push_back(BasisLevel(l, levelMax));
}

double Basis::evaluate(double x, int level, int position) const
{
  return basisLevels_[level].evaluate(x, position);
}

double Basis::evaluateDerivative(double x, int level, int position) const
{
  return basisLevels_[level].evaluateDerivative(x, position);
}

double Basis::evaluateSecondDerivative(double x, int level, int position) const
{
  return basisLevels_[level].evaluateSecondDerivative(x, position);
}

double Basis::getKnot(int level, int position) const
{
  return basisLevels_[level].getKnot(position);
}

int Basis::getLevelMax() const
{
  return levelMax_;
}
