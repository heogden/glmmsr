#ifndef GUARD_SUBSETEIGEN_H
#define GUARD_SUBSETEIGEN_H

#include <vector>

#include <RcppEigen.h>

Eigen::VectorXd getVectorWithout(int, const Eigen::VectorXd&);

Eigen::MatrixXd getMatrixWithout(int, const Eigen::MatrixXd&);

void setVectorWithout(int, Eigen::VectorXd&, 
		      const Eigen::VectorXd&);

void setMatrixWithout(int, Eigen::MatrixXd&,
		      const Eigen::MatrixXd&);

Eigen::VectorXd getVectorSubset(const Eigen::VectorXd&,
				const std::vector<int>&);

Eigen::MatrixXd getMatrixSubset(const Eigen::MatrixXd&,
				const std::vector<int>&);

void  setVectorSubset(Eigen::VectorXd&,
		      const Eigen::VectorXd&,
		      const std::vector<int>&);

void  setMatrixSubset(Eigen::MatrixXd&,
		      const Eigen::MatrixXd&,
		      const std::vector<int>&);

Eigen::MatrixXd getSparseMatrixSubset
(const Eigen::SparseMatrix<double>&, 
 const std::vector<int>&);


#endif // GUARD_SUBSETEIGEN_H
