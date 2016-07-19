#pragma once

#include "object.h"

extern Object quote_sym;
extern Object quasiquote_sym;
extern Object unquote_sym;
;

Object identifier_new(char const *str);
Object identifier_vertical_new(char const *str);

extern size_t identifier_vertical_i;
extern char *identifier_vertical_str;
void identifier_vertical_func(gpointer data, gpointer str);
