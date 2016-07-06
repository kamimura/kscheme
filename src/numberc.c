#include "numberc.h"

Object numberc_new_r_istr(mpfr_t const real, char *imag, int base) {
  Object out;
  mpc_init2(out.numberc, MPC_PREC);
  mpfr_t op;
  mpfr_init_set_str(op, imag, base, MPFR_RNDN);
  if (mpfr_zero_p(op)) {
    out.type = NUMBERR;
    mpfr_init_set(out.numberr, real, MPFR_RNDN);
  } else {
    out.type = NUMBERC;
    mpc_set_fr_fr(out.numberc, real, op, MPC_RNDNN);
  }
  mpfr_clear(op);
  return out;
}
