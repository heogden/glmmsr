#ifndef GUARD_TESTINGEIGEN_H
#define GUARD_TESTINGEIGEN_H

#include <vector>

#include <RcppEigen.h>

#include "gtest/gtest.h"

::testing::AssertionResult VectorNear(const std::vector<double>&, const std::vector<double>&, double);

::testing::AssertionResult MatrixXdNear(const Eigen::MatrixXd&, const Eigen::MatrixXd&, double);

#endif
