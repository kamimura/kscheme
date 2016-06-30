#pragma once

#include "object.h"

Object extend_environment();
/* Object extend_environment(Object vars, Object vals, Object base_env); */
Object lookup_variable_valueref(Object var, Object env);
Object lookup_variable_value(Object var, Object env);
Object set_variable_value(Object var, Object val, Object env);
Object define_variable(Object var, Object val, Object env);
