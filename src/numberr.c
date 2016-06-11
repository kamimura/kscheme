#include "numberr.h"

mpfr_prec_t const MPC_PREC = 53;

Object numberr_new(char const *str, int base) {
  Object out = {.type = NUMBERR};
  mpfr_init_set_str(out.numberr, str, 10, MPFR_RNDN);
  return out;
}
