#pragma once

#include "object.h"

void pair_write(FILE *stream, Object const obj);
void pair_write_shared(FILE *stream, Object const obj);
void pair_write_simple(FILE *stream, Object const obj);

#include <stdbool.h>
bool list_p(Object obj);
