#ifndef GUARD_MIXEDCONTINUOUSBELIEFVECTOR_H
#define GUARD_MIXEDCONTINUOUSBELIEFVECTOR_H

#include <vector>

#include "MixedContinuousBelief.h"
#include "GLMMBelief.h"
#include "NormalBelief.h"

typedef std::vector<MixedContinuousBelief> 
MixedContinuousBeliefVector;

void appendGLMMBelief(MixedContinuousBeliefVector&,
		      const std::vector<int>&,
		      const Eigen::MatrixXd&,
		      const Eigen::MatrixXd&,
		      const Eigen::SparseMatrix<double>&,
		      const Eigen::VectorXi&,
		      const Eigen::ArrayXd&,
		      const Eigen::ArrayXd&);

void appendNormalBelief(MixedContinuousBeliefVector&,
			const std::vector<int>&,
			const Eigen::VectorXd&,
			const Eigen::MatrixXd&);

#endif // GUARD_MIXEDCONTINUOUSBELIEFVECTOR_H
