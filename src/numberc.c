#include "numberc.h"

mpz_t opz;
mpz_t opz1;
mpq_t opq;
mpq_t opq1;
mpf_t opf;
mpfr_t opfr;
mpfr_t opfr1;
mpfr_t opfr2;
mpfr_t opfr3;
mpfr_t opfr4;
mpfr_t real;
mpfr_t imag;
mpfr_t argopfr;
mpfr_t sopfr;
mpfr_t copfr;
mpc_t opc;
mpc_t opc1;
mpc_t opc2;
mpc_t opc3;

Object numberc_new_r_istr(mpfr_t const real, char *imag, int base) {
  Object out;
  mpc_init2(out.numberc, MPC_PREC);
  mpfr_set_str(opfr, imag, base, MPFR_RNDN);
  if (mpfr_zero_p(opfr)) {
    out.type = NUMBERR;
    mpfr_init_set(out.numberr, real, MPFR_RNDN);
  } else {
    out.type = NUMBERC;
    mpc_set_fr_fr(out.numberc, real, opfr, MPC_RNDNN);
  }
  return out;
}
