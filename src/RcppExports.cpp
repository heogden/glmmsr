// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/glmmsr.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// continuous_beliefs__ctor
std::vector<MixedContinuousBelief> continuous_beliefs__ctor();
RcppExport SEXP glmmsr_continuous_beliefs__ctor() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(continuous_beliefs__ctor());
    return rcpp_result_gen;
END_RCPP
}
// continuous_beliefs__append_glmm_belief
void continuous_beliefs__append_glmm_belief(glmmsr::RcppR6::RcppR6<std::vector<MixedContinuousBelief> > obj_, std::vector<int> items, Eigen::MatrixXd X, Eigen::MatrixXd Zt, Eigen::SparseMatrix<double> Lambdat, Eigen::VectorXi Lind, Eigen::ArrayXd response, Eigen::ArrayXd weights);
RcppExport SEXP glmmsr_continuous_beliefs__append_glmm_belief(SEXP obj_SEXP, SEXP itemsSEXP, SEXP XSEXP, SEXP ZtSEXP, SEXP LambdatSEXP, SEXP LindSEXP, SEXP responseSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<std::vector<MixedContinuousBelief> > >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type items(itemsSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type X(XSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type Zt(ZtSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type Lambdat(LambdatSEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXi >::type Lind(LindSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type response(responseSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type weights(weightsSEXP);
    continuous_beliefs__append_glmm_belief(obj_, items, X, Zt, Lambdat, Lind, response, weights);
    return R_NilValue;
END_RCPP
}
// continuous_beliefs__append_normal_belief
void continuous_beliefs__append_normal_belief(glmmsr::RcppR6::RcppR6<std::vector<MixedContinuousBelief> > obj_, std::vector<int> items, Eigen::VectorXd mean, Eigen::MatrixXd precision);
RcppExport SEXP glmmsr_continuous_beliefs__append_normal_belief(SEXP obj_SEXP, SEXP itemsSEXP, SEXP meanSEXP, SEXP precisionSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<std::vector<MixedContinuousBelief> > >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type items(itemsSEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type precision(precisionSEXP);
    continuous_beliefs__append_normal_belief(obj_, items, mean, precision);
    return R_NilValue;
END_RCPP
}
// continuous_beliefs__size
int continuous_beliefs__size(glmmsr::RcppR6::RcppR6<std::vector<MixedContinuousBelief> > obj_);
RcppExport SEXP glmmsr_continuous_beliefs__size(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<std::vector<MixedContinuousBelief> > >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(continuous_beliefs__size(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__ctor
Parameters calibration_parameters__ctor();
RcppExport SEXP glmmsr_calibration_parameters__ctor() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__ctor());
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__theta__get
Eigen::VectorXd calibration_parameters__theta__get(glmmsr::RcppR6::RcppR6<Parameters> obj_);
RcppExport SEXP glmmsr_calibration_parameters__theta__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__theta__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__theta__set
void calibration_parameters__theta__set(glmmsr::RcppR6::RcppR6<Parameters> obj_, Eigen::VectorXd value);
RcppExport SEXP glmmsr_calibration_parameters__theta__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd >::type value(valueSEXP);
    calibration_parameters__theta__set(obj_, value);
    return R_NilValue;
END_RCPP
}
// calibration_parameters__beta__get
Eigen::VectorXd calibration_parameters__beta__get(glmmsr::RcppR6::RcppR6<Parameters> obj_);
RcppExport SEXP glmmsr_calibration_parameters__beta__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__beta__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__beta__set
void calibration_parameters__beta__set(glmmsr::RcppR6::RcppR6<Parameters> obj_, Eigen::VectorXd value);
RcppExport SEXP glmmsr_calibration_parameters__beta__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd >::type value(valueSEXP);
    calibration_parameters__beta__set(obj_, value);
    return R_NilValue;
END_RCPP
}
// calibration_parameters__family__get
std::string calibration_parameters__family__get(glmmsr::RcppR6::RcppR6<Parameters> obj_);
RcppExport SEXP glmmsr_calibration_parameters__family__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__family__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__family__set
void calibration_parameters__family__set(glmmsr::RcppR6::RcppR6<Parameters> obj_, std::string value);
RcppExport SEXP glmmsr_calibration_parameters__family__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< std::string >::type value(valueSEXP);
    calibration_parameters__family__set(obj_, value);
    return R_NilValue;
END_RCPP
}
// calibration_parameters__link__get
std::string calibration_parameters__link__get(glmmsr::RcppR6::RcppR6<Parameters> obj_);
RcppExport SEXP glmmsr_calibration_parameters__link__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__link__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__link__set
void calibration_parameters__link__set(glmmsr::RcppR6::RcppR6<Parameters> obj_, std::string value);
RcppExport SEXP glmmsr_calibration_parameters__link__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< std::string >::type value(valueSEXP);
    calibration_parameters__link__set(obj_, value);
    return R_NilValue;
END_RCPP
}
// calibration_parameters__n_sparse_levels__get
int calibration_parameters__n_sparse_levels__get(glmmsr::RcppR6::RcppR6<Parameters> obj_);
RcppExport SEXP glmmsr_calibration_parameters__n_sparse_levels__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__n_sparse_levels__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__n_sparse_levels__set
void calibration_parameters__n_sparse_levels__set(glmmsr::RcppR6::RcppR6<Parameters> obj_, int value);
RcppExport SEXP glmmsr_calibration_parameters__n_sparse_levels__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type value(valueSEXP);
    calibration_parameters__n_sparse_levels__set(obj_, value);
    return R_NilValue;
END_RCPP
}
// calibration_parameters__n_quadrature_points__get
int calibration_parameters__n_quadrature_points__get(glmmsr::RcppR6::RcppR6<Parameters> obj_);
RcppExport SEXP glmmsr_calibration_parameters__n_quadrature_points__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_parameters__n_quadrature_points__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// calibration_parameters__n_quadrature_points__set
void calibration_parameters__n_quadrature_points__set(glmmsr::RcppR6::RcppR6<Parameters> obj_, int value);
RcppExport SEXP glmmsr_calibration_parameters__n_quadrature_points__set(SEXP obj_SEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Parameters> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< int >::type value(valueSEXP);
    calibration_parameters__n_quadrature_points__set(obj_, value);
    return R_NilValue;
END_RCPP
}
// cluster_graph__ctor
ClusterGraph cluster_graph__ctor(std::vector<MixedContinuousBelief> beliefs);
RcppExport SEXP glmmsr_cluster_graph__ctor(SEXP beliefsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<MixedContinuousBelief> >::type beliefs(beliefsSEXP);
    rcpp_result_gen = Rcpp::wrap(cluster_graph__ctor(beliefs));
    return rcpp_result_gen;
END_RCPP
}
// cluster_graph__compute_log_normalizing_constant
double cluster_graph__compute_log_normalizing_constant(glmmsr::RcppR6::RcppR6<ClusterGraph> obj_, Eigen::VectorXd mean, Eigen::SparseMatrix<double> precision, Parameters parameters);
RcppExport SEXP glmmsr_cluster_graph__compute_log_normalizing_constant(SEXP obj_SEXP, SEXP meanSEXP, SEXP precisionSEXP, SEXP parametersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<ClusterGraph> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< Parameters >::type parameters(parametersSEXP);
    rcpp_result_gen = Rcpp::wrap(cluster_graph__compute_log_normalizing_constant(obj_, mean, precision, parameters));
    return rcpp_result_gen;
END_RCPP
}
// cluster_graph__width__get
int cluster_graph__width__get(glmmsr::RcppR6::RcppR6<ClusterGraph> obj_);
RcppExport SEXP glmmsr_cluster_graph__width__get(SEXP obj_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<ClusterGraph> >::type obj_(obj_SEXP);
    rcpp_result_gen = Rcpp::wrap(cluster_graph__width__get(obj_));
    return rcpp_result_gen;
END_RCPP
}
// extended_family__ctor
Family extended_family__ctor(std::string family, std::string link);
RcppExport SEXP glmmsr_extended_family__ctor(SEXP familySEXP, SEXP linkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type family(familySEXP);
    Rcpp::traits::input_parameter< std::string >::type link(linkSEXP);
    rcpp_result_gen = Rcpp::wrap(extended_family__ctor(family, link));
    return rcpp_result_gen;
END_RCPP
}
// extended_family__evaluate
double extended_family__evaluate(glmmsr::RcppR6::RcppR6<Family> obj_, Eigen::ArrayXd linear_predictor, Eigen::ArrayXd response, Eigen::ArrayXd weights);
RcppExport SEXP glmmsr_extended_family__evaluate(SEXP obj_SEXP, SEXP linear_predictorSEXP, SEXP responseSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Family> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type linear_predictor(linear_predictorSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type response(responseSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(extended_family__evaluate(obj_, linear_predictor, response, weights));
    return rcpp_result_gen;
END_RCPP
}
// extended_family__evaluate_d1
Eigen::ArrayXd extended_family__evaluate_d1(glmmsr::RcppR6::RcppR6<Family> obj_, Eigen::ArrayXd linear_predictor, Eigen::ArrayXd response, Eigen::ArrayXd weights);
RcppExport SEXP glmmsr_extended_family__evaluate_d1(SEXP obj_SEXP, SEXP linear_predictorSEXP, SEXP responseSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Family> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type linear_predictor(linear_predictorSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type response(responseSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(extended_family__evaluate_d1(obj_, linear_predictor, response, weights));
    return rcpp_result_gen;
END_RCPP
}
// extended_family__evaluate_d2
Eigen::ArrayXd extended_family__evaluate_d2(glmmsr::RcppR6::RcppR6<Family> obj_, Eigen::ArrayXd linear_predictor, Eigen::ArrayXd response, Eigen::ArrayXd weights);
RcppExport SEXP glmmsr_extended_family__evaluate_d2(SEXP obj_SEXP, SEXP linear_predictorSEXP, SEXP responseSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Family> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type linear_predictor(linear_predictorSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type response(responseSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(extended_family__evaluate_d2(obj_, linear_predictor, response, weights));
    return rcpp_result_gen;
END_RCPP
}
// extended_family__evaluate_d3
Eigen::ArrayXd extended_family__evaluate_d3(glmmsr::RcppR6::RcppR6<Family> obj_, Eigen::ArrayXd linear_predictor, Eigen::ArrayXd response, Eigen::ArrayXd weights);
RcppExport SEXP glmmsr_extended_family__evaluate_d3(SEXP obj_SEXP, SEXP linear_predictorSEXP, SEXP responseSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Family> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type linear_predictor(linear_predictorSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type response(responseSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(extended_family__evaluate_d3(obj_, linear_predictor, response, weights));
    return rcpp_result_gen;
END_RCPP
}
// extended_family__evaluate_d4
Eigen::ArrayXd extended_family__evaluate_d4(glmmsr::RcppR6::RcppR6<Family> obj_, Eigen::ArrayXd linear_predictor, Eigen::ArrayXd response, Eigen::ArrayXd weights);
RcppExport SEXP glmmsr_extended_family__evaluate_d4(SEXP obj_SEXP, SEXP linear_predictorSEXP, SEXP responseSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< glmmsr::RcppR6::RcppR6<Family> >::type obj_(obj_SEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type linear_predictor(linear_predictorSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type response(responseSEXP);
    Rcpp::traits::input_parameter< Eigen::ArrayXd >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(extended_family__evaluate_d4(obj_, linear_predictor, response, weights));
    return rcpp_result_gen;
END_RCPP
}
