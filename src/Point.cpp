#include "Basis.h"
#include "counting.h"

#include "Point.h"

Point::Point(int dimension): index_(0), levels_(dimension), positions_(dimension), levelSum_(0)
{
}

Point::Point(const std::vector<int>& levels, const std::vector<int>& positions) 
  : levels_(levels), positions_(positions), levelSum_(0)
{
  setIndex();
  setLevelSum();
}

bool Point::operator==(const Point& other) const
{
  return (index_==other.index_) & (levels_==other.levels_) & (positions_==other.positions_) & (levelSum_ == other.levelSum_);
}


int Point::getIndex() const
{
  return index_;
}

int Point::getLevel(int direction) const
{
  return levels_.at(direction);
}

int Point::getPosition(int direction) const
{
  return positions_.at(direction);
}

double Point::getGridPointInDirection(int direction, const Basis& basis) const
{
    return basis.getKnot(levels_[direction], positions_[direction]);
}

Eigen::VectorXd Point::getGridPoint(const Basis& basis) const
{
  Eigen::VectorXd ret(levels_.size());
  for(auto t = levels_.size() - levels_.size(); t < levels_.size(); ++t) {
    ret(t) = getGridPointInDirection(t, basis);
  }
  return ret;
}

double Point::computeBasis(const Eigen::VectorXd& x, const Basis& basis) const
{
  double ret = 1.0;
  auto d = levels_.size();
  for(auto t = d - d; t < d; ++t)
    ret *= basis.evaluate(x[t], levels_[t], positions_[t]);
  return ret;
}

Eigen::VectorXd Point::computeBasisDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  auto d = levels_.size();
  Eigen::VectorXd ret;
  ret = Eigen::VectorXd::Ones(d);
  for(auto t = d - d; t < d; ++t) {
    double basisValue = 
      basis.evaluate(x[t], levels_[t], positions_[t]);
    double basisDerivative = 
      basis.evaluateDerivative(x[t], levels_[t], positions_[t]);
    for(auto s = d - d; s < d; ++s) {
      if(s == t)
	ret[s] *= basisDerivative;
      else
	ret[s] *= basisValue;
    }
  }
  return ret;
}

Eigen::MatrixXd Point::computeBasisSecondDerivative(const Eigen::VectorXd& x, const Basis& basis) const
{
  auto d = levels_.size();
  Eigen::MatrixXd ret;
  ret = Eigen::MatrixXd::Ones(d, d);
  for(auto t = d - d; t < d; ++t) {
    double basisValue = 
      basis.evaluate(x[t], levels_[t], positions_[t]);
    double basisDerivative = 
      basis.evaluateDerivative(x[t], levels_[t], positions_[t]);
    double basisSecondDerivative = 
      basis.evaluateSecondDerivative(x[t], levels_[t], positions_[t]);
    for(auto r = d - d; r < d; ++r) {
      for(auto s = r - r; s < (r + 1); ++s) {
        if(r == t) {
	  if(s == t)
	    ret(r, s) *= basisSecondDerivative;
	  else
	    ret(r, s) *= basisDerivative;
	} 
	else {
	  if(s == t)
	    ret(r, s) *= basisDerivative;
	  else
	    ret(r, s) *= basisValue;
	}
      }
    }
  }
  return ret.selfadjointView<Eigen::Lower>();
}

void Point::advance()
{
  index_++;
  if(!advancePositions()) {
    // couldn't advance positions within grid with these levels
    // so move to the next levels
    if(!advanceLevels()) {
      // couldn't advance level within levels of current levelSum
      // so move to next levelSum
      levelSum_++;
      // and return levels to first value for the levelSum
      for(auto i = levels_.begin(); i != levels_.end(); ++i)
	*i = 0;
      levels_.at(0) = levelSum_;
    }
  }
}

void Point::advanceInDirection(int direction)
{
  if(!advancePositionInDirection(direction)) {
    levels_.at(direction)++;
    setLevelSum();
  }
  setIndex();
}

Point Point::firstParent(int direction)
{
  Point parent = *this;
  parent.levels_.at(direction) = 0;
  parent.positions_.at(direction) = 0;
  parent.setLevelSum();
  parent.setIndex();
  return parent;
}

// Based on Algorithm 5 of 
// "Compact Data Stucture of Scalable Algorithms for the
//  Sparse Grid Technique", by
// Murarasu, Weidendorfer, Buse, Butnaru, and Pfluger
void Point::setIndex()
{
  int dimension = levels_.size();
  int index1 = 0;
  int multiplier = 1;
  for(auto t = 0; t < dimension; ++t) {
    index1 += positions_.at(t) * multiplier;
    multiplier *= (1 << levels_.at(t));
  }

  int sum = levels_.at(0);
  int index2 = 0;
  for(auto t = 1; t < dimension; ++t) {
    index2 -= choose(t + sum, t);
    sum += levels_.at(t);
    index2 += choose(t + sum, t);
  }
  index2 <<= sum;

  int index3 = 0;
  for(auto s = 0; s < sum; ++s)
    index3 += choose(dimension - 1 + s, dimension - 1) * (1 << s);

  index_ = index1 + index2 + index3;
}


void Point::setLevelSum() 
{
  levelSum_ = 0;
  for(std::vector<int>::const_iterator i = levels_.begin();
      i != levels_.end(); ++i)
    levelSum_ += *i;
}


// Based on Algorithm 4 of 
// "Compact Data Stucture of Scalable Algorithms for the
//  Sparse Grid Technique", by
// Murarasu, Weidendorfer, Buse, Butnaru, and Pfluger
bool Point::advanceLevels() 
{
  bool success = false;
  if(levels_.back() < levelSum_) {
    success = true;
    std::vector<int> oldLevels(levels_);
    int t = 0;
    while(levels_[t] == 0)
      ++t;
    levels_[t] = 0;
    levels_[0] = oldLevels[t] - 1;
    levels_[t+1] = oldLevels[t+1] + 1;
  }
  return success;
}

bool Point::advancePositions()
{
  bool success = false;
  int t = 0;
  int d = positions_.size();
  while(!success & (t < d)) {
    success = advancePositionInDirection(t);
    ++t;
  }
  return success;
}

bool Point::advancePositionInDirection(int direction)
{
  bool success;
  if(positions_.at(direction) < ((1 << levels_.at(direction)) - 1)) {
    ++positions_.at(direction);
    success = true;
  } else {
    positions_.at(direction) = 0;
    success = false;
  }
  return success;
}
