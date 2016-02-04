#include <vector>

#include "MixedContinuousBelief.h"
#include "GLMMBelief.h"
#include "NormalBelief.h"

#include "MixedContinuousBeliefVector.h"

void appendGLMMBelief(MixedContinuousBeliefVector& beliefs,
		      const std::vector<int>& items,
		      const Eigen::MatrixXd& X,
		      const Eigen::MatrixXd& Zt,
		      const Eigen::SparseMatrix<double>& Lambdat,
		      const Eigen::VectorXi& Lind,
		      const Eigen::ArrayXd& response,
		      const Eigen::ArrayXd& weights)
{
  ContinuousBelief term(items, X, Zt, Lambdat, Lind, 
			response, weights);
  MixedContinuousBelief belief(term);
  beliefs.push_back(belief);
}

void appendNormalBelief(MixedContinuousBeliefVector& beliefs,
			const std::vector<int>& items,
			const Eigen::VectorXd& mean,
			const Eigen::MatrixXd& precision)
{
  ContinuousBelief term(items, mean, precision);
  MixedContinuousBelief belief(term);
  beliefs.push_back(belief);
}
