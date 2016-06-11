#pragma once

#include "object.h"

Object numberz_new(char const *str, int base);
Object numberz_copy(Object const obj);
void numberz_free(mpz_t op);
