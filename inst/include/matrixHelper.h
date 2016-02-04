#ifndef GUARD_MATRIXHELPER_H
#define GUARD_MATRIXHELPER_H

#include <vector>

#include <RcppEigen.h>

Eigen::VectorXd findSubset(const Eigen::VectorXd&, 
			   const std::vector<int>&);

void addVectorSubset(Eigen::VectorXd&, const std::vector<int>&,
		     const Eigen::VectorXd&);

void addMatrixSubset(Eigen::MatrixXd&, const std::vector<int>&,
		     const Eigen::MatrixXd&);

#endif // GUARD_MATRIXHELPER_H
