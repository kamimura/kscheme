#pragma once

#include "object.h"

#define MEMORY_SIZE 1000000

extern Object memory_from[2][MEMORY_SIZE];
extern Object memory_to[2][MEMORY_SIZE];
extern Object *cars;
extern Object *cdrs;
extern Object *new_cars;
extern Object *new_cdrs;

#define REGISTER_COUNT 13
extern Object root;
extern Object expr, env, val, cont, proc, argl, unev, cur_in, cur_out, cur_err;
extern Object global;
extern Object stack;
;

Object cons(Object obj1, Object obj2);
Object car(Object obj);
Object cdr(Object obj);
Object carref(Object obj);
Object cdrref(Object obj);

void save(Object obj);
void restore(Object *ptr);

Object string_cons(Object obj1, Object obj2);
Object string_carref(Object obj);
Object string_cdrref(Object obj);

Object list2vector(Object obj);
Object list2bytevector(Object obj);

Object continuation_cons(Object ob1, Object obj2j);
Object continuation_carref(Object obj);
Object continuation_cdrref(Object obj);

Object implementation_defined_object_carref(Object obj);
Object implementation_defined_object_cdrref(Object obj);

Object port_carref(Object obj);
Object port_cdrref(Object obj);
