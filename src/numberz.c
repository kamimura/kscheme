#include "numberz.h"

Object numberz_new(char const *str, int base) {
  Object out = {.type = NUMBERZ};
  mpz_init_set_str(out.numberz, str, base);
  return out;
}
Object numberz_copy(Object const obj) {
  Object out = {.type = NUMBERZ};
  mpz_init_set(out.numberz, obj.numberz);
  return out;
}
void numberz_free(mpz_t op) { mpz_clear(op); }
