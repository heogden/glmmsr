#include "Rcpp.h"
#include "accessParameters.h"

Parameters makeParametersR()
{
  Parameters parameters;
  Eigen::VectorXd emptyVector = Eigen::VectorXd::Zero(0);
  parameters.GLMMTheta = emptyVector;
  parameters.GLMMBeta = emptyVector;
  setParametersFamily(parameters, "binomial");
  setParametersLink(parameters, "logit");
  setParametersNSparseLevels(parameters, 1);
  setParametersNQuadraturePoints(parameters, 1);
  return parameters;
}

std::string getParametersFamily(const Parameters& parameters)
{
  Family family = parameters.GLMMFamily;
  FamilyGivenMean familyGivenMean = family.getFamilyGivenMean();
  return familyGivenMean.getName();
}

void setParametersFamily(Parameters& parameters, 
			 std::string familyName)
{
  FamilyGivenMean newFamilyGivenMean(familyName);
  Family family = parameters.GLMMFamily;
  family.setFamilyGivenMean(newFamilyGivenMean);
  parameters.GLMMFamily = family;
}

std::string getParametersLink(const Parameters& parameters)
{
  Family family = parameters.GLMMFamily;
  Link link = family.getLink();
  return link.getName();
}

void setParametersLink(Parameters& parameters, 
		       std::string linkName)
{
  Link newLink(linkName);
  Family family = parameters.GLMMFamily;
  family.setLink(newLink);
  parameters.GLMMFamily = family;
}


int getParametersNSparseLevels(const Parameters& parameters)
{
  Basis basis = parameters.sparseBasis;
  return basis.getLevelMax();
}

void setParametersNSparseLevels(Parameters& parameters, 
				int nSparseLevels)
{
  Basis newBasis(nSparseLevels);
  parameters.sparseBasis = newBasis;
}

int getParametersNQuadraturePoints(const Parameters& parameters)
{
  QuadratureRule rule = parameters.quadratureRule;
  return rule.getNodes().size();
}

void setParametersNQuadraturePoints(Parameters& parameters, 
				    int nQuadraturePoints)
{
  QuadratureRule newRule(nQuadraturePoints);
  parameters.quadratureRule = newRule;
}
