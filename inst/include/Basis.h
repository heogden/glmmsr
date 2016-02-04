#ifndef GUARD_BASIS_H
#define GUARD_BASIS_H

#include <vector>

#include "BasisLevel.h"

class Basis
{
 public:
  Basis();
  Basis(int);
  
  double evaluate(double, int, int) const;
  double evaluateDerivative(double, int, int) const;
  double evaluateSecondDerivative(double, int, int) const;
  
  double getKnot(int, int) const;
  int getLevelMax() const;
 private:
  std::vector<BasisLevel> basisLevels_;
  int levelMax_;
};

#endif // GUARD_BASIS_H
