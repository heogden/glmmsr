#include "matrixHelper.h"

Eigen::VectorXd findSubset(const Eigen::VectorXd& vector, 
			   const std::vector<int>& subset)
{
  Eigen::VectorXd ret(subset.size());
  for(auto i = subset.size() - subset.size();
      i < subset.size(); ++i) {
    ret(i) = vector(subset.at(i));
  }
  return ret;
}

void addVectorSubset(Eigen::VectorXd& vector,
		     const std::vector<int>& subset,
		     const Eigen::VectorXd& toAdd)
{
  for(auto i = subset.size() - subset.size(); 
      i < subset.size(); ++i)
    vector(subset.at(i)) += toAdd(i);
}

void addMatrixSubset(Eigen::MatrixXd& matrix,
		     const std::vector<int>& subset,
		     const Eigen::MatrixXd& toAdd)
{
  for(auto i = subset.size() - subset.size(); i < subset.size(); ++i)
    for(auto j = subset.size() - subset.size(); j < subset.size(); ++j)
    matrix(subset.at(i), subset.at(j)) += toAdd(i, j);
}
				
