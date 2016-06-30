#include "environment.h"

static Object make_frame() {
  if (unev.type == EMPTY && argl.type == EMPTY) {
    return cons(unev, argl);
  }
  Object vars = empty;
  Object vals = empty;
  for (; unev.type != EMPTY; unev = cdrref(unev), argl = cdrref(argl)) {
    if (unev.type == IDENTIFIER) {
      vars = cons(unev, vars);
      vals = cons(argl, vals);
      return cons(vars, vals);
    }
    if (argl.type == EMPTY) {
      return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
    }
    save(vals);
    vars = cons(carref(unev), vars);
    restore(&vals);
    vals = cons(car(argl), vals);
  }
  if (argl.type != EMPTY) {
    return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
  }
  return cons(vars, vals);
}
Object extend_environment() {
  Object frame = make_frame();
  return frame.type == WRONG_NUMBER_OF_ARGUMENTS ? frame : cons(frame, env);
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
  save(frame);
  Object t = cons(var, cars[frame.index]);
  restore(&frame);
  cars[frame.index] = t;
  save(frame);
  t = cons(val, cdrs[frame.index]);
  restore(&frame);
  cdrs[frame.index] = t;
  return unspecified;
}
