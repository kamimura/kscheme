#pragma once

#include "object.h"

/* mpfr_prec_t default 53 */
/* #define MPC_PREC 53 */
extern mpfr_prec_t const MPC_PREC;

Object numberr_new(char const *str, int base);
