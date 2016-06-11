#pragma once

#include "object.h"

extern mpfr_prec_t const MPC_PREC;

Object numberc_new_r_istr(mpfr_t const real, char *imag);
