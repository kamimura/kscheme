#include "numberq.h"

Object numberq_new(char const *str, int base) {
  Object out;
  mpq_init(out.numberq);
  mpq_set_str(out.numberq, str, base);
  if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
    mpz_init_set(opz, mpq_numref(out.numberq));
    mpq_clear(out.numberq);
    mpz_init_set(out.numberz, opz);
    out.type = NUMBERZ;
  } else {
    out.type = NUMBERQ;
  }
  return out;
}
