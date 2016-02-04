#ifndef GUARD_ACCESSPARAMETERS_H
#define GUARD_ACCESSPARAMETERS_H

#include "Parameters.h"

Parameters makeParametersR();

std::string getParametersFamily(const Parameters&);
void setParametersFamily(Parameters&, std::string);

std::string getParametersLink(const Parameters&);
void setParametersLink(Parameters&, std::string);

int getParametersNSparseLevels(const Parameters&);
void setParametersNSparseLevels(Parameters&, int);

int getParametersNQuadraturePoints(const Parameters&);
void setParametersNQuadraturePoints(Parameters&, int);

#endif // GUARD_ACCESSPARAMETERS_H
