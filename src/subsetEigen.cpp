#include <algorithm>
#include <stdexcept>

#include "subsetEigen.h"

typedef Eigen::VectorXd Vector;
typedef Eigen::MatrixXd Matrix;
typedef Eigen::SparseMatrix<double> SparseMatrix;

Vector getVectorWithout(int index, const Vector& vector)
{
  Vector ret(vector.size() - 1);
  setVectorWithout(index, ret, vector);
  return ret;
}

Matrix getMatrixWithout(int index, const Matrix& matrix)
{
  Matrix ret(matrix.rows() - 1, matrix.cols() - 1);
  setMatrixWithout(index, ret, matrix);
  return ret;
}

void setVectorWithout(int index, Vector& target, const Vector& source)
{
  auto n = std::min(source.size(), target.size());
  target.head(index) = source.head(index);
  target.tail(n - index) = source.tail(n - index);
}

void setMatrixWithout(int index, Matrix& target, const Matrix& source)
{
  auto n = std::min(source.rows(), target.rows());
  target.topLeftCorner(index, index) = source.topLeftCorner(index, index);
  target.topRightCorner(index, n - index) = source.topRightCorner(index, n - index);
  target.bottomLeftCorner(n - index, index) = source.bottomLeftCorner(n - index, index);
  target.bottomRightCorner(n - index, n - index) = source.bottomRightCorner(n - index, n - index);
}

Eigen::VectorXd getVectorSubset(const Eigen::VectorXd& vector,
				const std::vector<int>& subset)
{
  Eigen::VectorXd ret(subset.size());
  for(auto i = subset.size() - subset.size(); i < subset.size(); ++i)
    ret(i) = vector(subset.at(i)); 
  return ret;
}


Eigen::MatrixXd getMatrixSubset(const Eigen::MatrixXd& matrix,
				const std::vector<int>& subset)
{
  Eigen::MatrixXd ret(subset.size(), subset.size());
  for(auto i = subset.size()- subset.size(); i < subset.size(); ++i)
    for(auto j = subset.size() - subset.size(); j < subset.size(); ++j)
      ret(i, j) = matrix(subset.at(i), subset.at(j));
  return ret;
}

void setVectorSubset(Vector& target, const Vector& source,
		     const std::vector<int>& subset)
{
  for(auto i = subset.size() - subset.size(); i < subset.size(); ++i)
    target(subset.at(i)) = source(i);
}


void setMatrixSubset(Matrix& target, const Matrix& source,
		     const std::vector<int>& subset)
{
  for(auto i = subset.size() - subset.size(); i < subset.size(); ++i)
    for(auto j = subset.size() - subset.size(); j < subset.size(); ++j)
      target(subset.at(i), subset.at(j)) = source(i, j);
}

Matrix getSparseMatrixSubset(const SparseMatrix& matrix, 
				      const std::vector<int>& subset)
{
  Matrix ret(subset.size(), subset.size());
  for(auto i = subset.size() - subset.size(); i < subset.size(); ++i)
    for(auto j = subset.size() - subset.size(); j < subset.size(); ++j)
      ret(i, j) = matrix.coeff(subset.at(i), subset.at(j));
   return ret;
}
