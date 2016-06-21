#include "environment.h"

Object make_frame(Object params, Object args) {
  if (params.type == EMPTY && args.type == EMPTY) {
    return cons(params, args);
  }
  Object vars = empty;
  Object vals = empty;
  Object p, a;
  for (p = params, a = args; p.type != EMPTY; p = cdrref(p), a = cdrref(a)) {
    if (p.type == IDENTIFIER) {
      vars = cons(p, vars);
      vals = cons(a, vals);
      return cons(vars, vals);
    }
    if (a.type == EMPTY) {
      return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
    }
    vars = cons(carref(p), vars);
    vals = cons(car(a), vals);
  }
  if (a.type != EMPTY) {
    return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
  }
  return cons(vars, vals);
  ;
}
Object extend_environment(Object vars, Object vals, Object base_env) {
  Object frame = make_frame(vars, vals);
  return frame.type == WRONG_NUMBER_OF_ARGUMENTS ? frame
                                                 : cons(frame, base_env);
}

Object lookup_variable_valueref(Object var, Object env) {
  for (Object t = env; t.type != EMPTY; t = cdrref(t)) {
    Object frame = carref(t);
    Object vars = carref(frame);
    Object vals = cdrref(frame);
    for (; vars.type != EMPTY; vars = cdrref(vars), vals = cdrref(vals)) {
      if (carref(vars).identifier == var.identifier) {
        return carref(vals);
      }
    }
  }
  return none;
}
Object lookup_variable_value(Object var, Object env) {
  for (Object t = env; t.type != EMPTY; t = cdrref(t)) {
    Object frame = carref(t);
    Object vars = carref(frame);
    Object vals = cdrref(frame);
    for (; vars.type != EMPTY; vars = cdrref(vars), vals = cdrref(vals)) {
      if (carref(vars).identifier == var.identifier) {
        return car(vals);
      }
    }
  }
  return none;
}
Object set_variable_value(Object var, Object val, Object env) {
  for (Object t = env; t.type != EMPTY; t = cdrref(t)) {
    Object frame = carref(t);
    Object vars = carref(frame);
    Object vals = cdrref(frame);
    for (; vars.type != EMPTY; vars = cdrref(vars), vals = cdrref(vals)) {
      if (carref(vars).identifier == var.identifier) {
        object_free(&cars[vals.index]);
        cars[vals.index] = val;
        return unspecified;
      }
    }
  }
  return none;
}
Object define_variable(Object var, Object val, Object env) {
  Object frame = carref(env);
  Object vars = carref(frame);
  Object vals = cdrref(frame);
  for (; vars.type != EMPTY; vars = cdrref(vars), vals = cdrref(vals)) {
    if (carref(vars).identifier == var.identifier) {
      object_free(&cars[vals.index]);
      cars[vals.index] = val;
      return unspecified;
    }
  }
  cars[frame.index] = cons(var, cars[frame.index]);
  cdrs[frame.index] = cons(val, cdrs[frame.index]);
  return unspecified;
}
