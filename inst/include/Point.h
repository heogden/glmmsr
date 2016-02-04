#ifndef GUARD_POINT_H
#define GUARD_POINT_H

#include <vector>

#include <RcppEigen.h>

#include "Basis.h"

class Point
{
 public:
  Point(int);
  Point(const std::vector<int>&, const std::vector<int>&);

  bool operator==(const Point&) const;

  int getIndex() const;
  int getLevel(int) const;
  int getPosition(int) const;

  double getGridPointInDirection(int, const Basis&) const;
  Eigen::VectorXd getGridPoint(const Basis&) const;

  double computeBasis(const Eigen::VectorXd&, const Basis&) const;
  Eigen::VectorXd computeBasisDerivative
    (const Eigen::VectorXd&, const Basis&) const;
  Eigen::MatrixXd computeBasisSecondDerivative
    (const Eigen::VectorXd&, const Basis&) const;

  void advance();
  void advanceInDirection(int);

  Point firstParent(int);

 private:
  int index_;
  std::vector<int> levels_;
  std::vector<int> positions_;
  int levelSum_;

  void setIndex();
  void setLevelSum();

  bool advanceLevels();
  bool advancePositions();
  bool advancePositionInDirection(int);

};

#endif // GUARD_POINT_H
