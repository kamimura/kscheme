/** \file  */
#include "procedures.h"

static void error(char const *msg) {
  fprintf(stderr, "kscheme error: %s\n", msg);
  exit(1);
}
extern FILE *yyout;
static Object arguments(Object obj, char const *s) {
  fprintf(yyout, "Error: (%s) wrong number of arguments -- ", s);
  object_write(yyout, obj);
  fprintf(yyout, "\n");
  return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
}
static Object wrong_type(char const *prog_name, Object obj) {
  fprintf(yyout, "Error: (%s) wrong type argument -- ", prog_name);
  object_write(yyout, obj);
  fprintf(yyout, "\n");
  return (Object){.type = WRONG_TYPE_ARGUMENT};
}
static size_t args_length(Object const args) {
  size_t len = 0;
  for (Object o = args; o.type != EMPTY; o = cdrref(o)) {
    len += 1;
  }
  return len;
}
static Object value(Object const obj) {
  if (obj.type != MULTIPLE) {
    return obj;
  }
  Object o;
  for (o = carref(obj); o.type != MULTIPLE; o = carref(o)) {
    ;
  }
  return o;
}
Object scm_eqv_p(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "eqv?");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  switch (obj1.type) {
  case TRUE_TYPE:
  case FALSE_TYPE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case AND:
  case OR:
  case EMPTY:
  case STRING_EMPTY:
  case UNSPECIFIED:
  case MULTIPLE_ZERO:
    return obj1.type == obj2.type ? true_obj : false_obj;
  case IDENTIFIER:
    return obj2.type == IDENTIFIER && obj1.identifier == obj2.identifier
               ? true_obj
               : false_obj;
  case NUMBERZ:
    switch (obj2.type) {
    case NUMBERZ:
      return mpz_cmp(obj1.numberz, obj2.numberz) == 0 ? true_obj : false_obj;
    case NUMBERQ:
      return mpq_cmp_z(obj2.numberq, obj1.numberz) == 0 ? true_obj : false_obj;
    case NUMBERR:
    case NUMBERC:
    case CHARACTER:
    case STRING_EMPTY:
    case STRING:
    case VECTOR:
    case BYTEVECTOR:
    case EMPTY:
    case PAIR:
    case IDENTIFIER:
    case TRUE_TYPE:
    case FALSE_TYPE:
    case QUOTE:
    case LAMBDA:
    case IF:
    case SET:
    case DEFINE:
    case BEGIN_TYPE:
    case AND:
    case OR:
    case PRIMITIVE_PROCEDURE:
    case PRIMITIVE_PROCEDURE_APPLY:
    case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
    case CONTINUATION:
    case PRIMITIVE_PROCEDURE_RAISE:
    case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    case PROCEDURE:
    case EOF_OBJ:
    case FILE_ERROR:
    case PORT_INPUT_TEXT:
    case PORT_INPUT_BINARY:
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_BINARY:
    case IMPLEMENTATION_DEFINED_OBJECT:
    case UNSPECIFIED:
    case MULTIPLE_ZERO:
      return false_obj;
    default:
      error("eqv?");
    }
  case NUMBERQ: {
    switch (obj2.type) {
    case NUMBERZ:
      return mpq_cmp_z(obj1.numberq, obj2.numberz) == 0 ? true_obj : false_obj;
    case NUMBERQ:
      return mpq_cmp(obj1.numberq, obj2.numberq) == 0 ? true_obj : false_obj;
    case NUMBERR:
    case NUMBERC:
    case CHARACTER:
    case STRING_EMPTY:
    case STRING:
    case VECTOR:
    case BYTEVECTOR:
    case EMPTY:
    case PAIR:
    case IDENTIFIER:
    case TRUE_TYPE:
    case FALSE_TYPE:
    case QUOTE:
    case LAMBDA:
    case IF:
    case SET:
    case DEFINE:
    case BEGIN_TYPE:
    case AND:
    case OR:
    case PRIMITIVE_PROCEDURE:
    case PRIMITIVE_PROCEDURE_APPLY:
    case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
    case CONTINUATION:
    case PRIMITIVE_PROCEDURE_RAISE:
    case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    case PROCEDURE:
    case EOF_OBJ:
    case FILE_ERROR:
    case PORT_INPUT_TEXT:
    case PORT_INPUT_BINARY:
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_BINARY:
    case IMPLEMENTATION_DEFINED_OBJECT:
    case UNSPECIFIED:
    case MULTIPLE_ZERO:
      return false_obj;
    default:
      error("eqv?");
    }
  }
  case NUMBERR: {
    switch (obj2.type) {
    case NUMBERR:
      return mpfr_cmp(obj1.numberr, obj2.numberr) == 0 ? true_obj : false_obj;
    case NUMBERC: {
      if (mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return mpfr_cmp(obj1.numberr, mpc_realref(obj2.numberc)) == 0
                   ? true_obj
                   : false_obj;
      }
      return false_obj;
    }
    case NUMBERZ:
    case NUMBERQ:
    case CHARACTER:
    case STRING_EMPTY:
    case STRING:
    case VECTOR:
    case BYTEVECTOR:
    case EMPTY:
    case PAIR:
    case IDENTIFIER:
    case TRUE_TYPE:
    case FALSE_TYPE:
    case QUOTE:
    case LAMBDA:
    case IF:
    case SET:
    case DEFINE:
    case BEGIN_TYPE:
    case AND:
    case OR:
    case PRIMITIVE_PROCEDURE:
    case PRIMITIVE_PROCEDURE_APPLY:
    case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
    case CONTINUATION:
    case PRIMITIVE_PROCEDURE_RAISE:
    case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    case PROCEDURE:
    case EOF_OBJ:
    case FILE_ERROR:
    case PORT_INPUT_TEXT:
    case PORT_INPUT_BINARY:
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_BINARY:
    case IMPLEMENTATION_DEFINED_OBJECT:
    case UNSPECIFIED:
    case MULTIPLE_ZERO:
      return false_obj;
    default:
      error("eqv?");
    }
  }
  case NUMBERC: {
    switch (obj2.type) {
    case NUMBERR: {
      if (mpfr_zero_p(mpc_imagref(obj1.numberc))) {
        return mpfr_cmp(mpc_realref(obj1.numberc), obj2.numberr) == 0
                   ? true_obj
                   : false_obj;
      }
      return false_obj;
    }
    case NUMBERC: {
      return mpc_cmp(obj1.numberc, obj2.numberc) == 0 ? true_obj : false_obj;
    }
    case NUMBERZ:
    case NUMBERQ:
    case CHARACTER:
    case STRING_EMPTY:
    case STRING:
    case VECTOR:
    case BYTEVECTOR:
    case EMPTY:
    case PAIR:
    case IDENTIFIER:
    case TRUE_TYPE:
    case FALSE_TYPE:
    case QUOTE:
    case LAMBDA:
    case IF:
    case SET:
    case DEFINE:
    case BEGIN_TYPE:
    case AND:
    case OR:
    case PRIMITIVE_PROCEDURE:
    case PRIMITIVE_PROCEDURE_APPLY:
    case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
    case CONTINUATION:
    case PRIMITIVE_PROCEDURE_RAISE:
    case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    case PROCEDURE:
    case EOF_OBJ:
    case FILE_ERROR:
    case PORT_INPUT_TEXT:
    case PORT_INPUT_BINARY:
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_BINARY:
    case IMPLEMENTATION_DEFINED_OBJECT:
    case UNSPECIFIED:
    case MULTIPLE_ZERO:
      return false_obj;
    default:
      error("eqv?");
    }
  }
  case CHARACTER:
    return obj2.type == CHARACTER && obj1.character == obj2.character
               ? true_obj
               : false_obj;
  case STRING:
    return obj1.type == obj2.type && obj1.index && obj2.index ? true_obj
                                                              : false_obj;
  case PRIMITIVE_PROCEDURE:
    return obj2.type == PRIMITIVE_PROCEDURE && obj1.proc == obj2.proc
               ? true_obj
               : false_obj;
  case PROCEDURE:
    return obj2.type == PROCEDURE && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case IMPLEMENTATION_DEFINED_OBJECT:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case EOF_OBJ:
  case FILE_ERROR:
    return false_obj;
  case PAIR:
    return obj2.type == PAIR && obj1.index == obj2.index ? true_obj : false_obj;
  case VECTOR:
    return obj2.type == VECTOR && obj1.index == obj2.index ? true_obj
                                                           : false_obj;
  case BYTEVECTOR:
    return obj2.type == BYTEVECTOR && obj1.index == obj2.index ? true_obj
                                                               : false_obj;
  case NONE:
    error("scm_eqv_p");
  default:
    return wrong_type("eqv?", args);
  }
}
Object scm_eq_p(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "eq?");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  switch (obj1.type) {
  case TRUE_TYPE:
  case FALSE_TYPE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case AND:
  case OR:
  case EMPTY:
  case MULTIPLE_ZERO:
    return obj1.type == obj2.type ? true_obj : false_obj;
  case IDENTIFIER:
    return obj2.type == IDENTIFIER && obj1.identifier == obj2.identifier
               ? true_obj
               : false_obj;
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
    return scm_eqv_p(args);
  case CHARACTER:
    return obj2.type == CHARACTER && obj1.character == obj2.character
               ? true_obj
               : false_obj;
  case STRING_EMPTY:
    return obj1.type == obj2.type ? true_obj : false_obj;
  case STRING:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case VECTOR:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case BYTEVECTOR:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case PRIMITIVE_PROCEDURE:
    return obj2.type == PRIMITIVE_PROCEDURE && obj1.index == obj2.index
               ? true_obj
               : false_obj;
  case PROCEDURE:
    return obj2.type == PROCEDURE && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case IMPLEMENTATION_DEFINED_OBJECT:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
                                                              : false_obj;
  case EOF_OBJ:
  case FILE_ERROR:
  case UNSPECIFIED:
    return false_obj;
  case PAIR:
    return obj2.type == PAIR && obj1.index == obj2.index ? true_obj : false_obj;
  case NONE:
    error("scm_eq_p");
  default:
    return wrong_type("eq?", args);
  }
}

/* Equivalence predicates end */

/* Numbers */
Object scm_number_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "number?");
  }
  Object arg = value(carref(args));
  switch (arg.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
    return true_obj;
  default:
    return false_obj;
  }
}
Object scm_complex_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "complex?");
  }
  Object arg = value(carref(args));
  switch (arg.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
    return true_obj;
  case NONE:
    error("scm_complex_p");
  default:
    return false_obj;
  }
}
Object scm_real_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "real?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
    return true_obj;
  case NUMBERC:
    return mpfr_zero_p(mpc_imagref(obj.numberc)) ? true_obj : false_obj;
  case NONE:
    error("scm_real_p");
  default:
    return false_obj;
  }
}
Object scm_rational_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "rational?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return true_obj;
  case NUMBERR: {
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, obj.numberr, MPFR_RNDN);
    mpq_t opq;
    mpq_init(opq);
    mpq_set_f(opq, op);
    mpf_clear(op);
    Object out = mpfr_cmp_q(obj.numberr, opq) == 0 ? true_obj : false_obj;
    mpq_clear(opq);
    return out;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return false_obj;
    }
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, mpc_realref(obj.numberc), MPFR_RNDN);
    mpq_t opq;
    mpq_init(opq);
    mpq_set_f(opq, op);
    mpf_clear(op);
    Object out = mpfr_cmp_q(obj.numberr, opq) == 0 ? true_obj : false_obj;
    mpq_clear(opq);
    return out;
  }
  case NONE:
    error("scm_rational_p");
  default:
    return false_obj;
  }
}
Object scm_integer_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "rational?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    return true_obj;
  }
  case NUMBERQ: {
    mpq_canonicalize(obj.numberq);
    return mpz_cmp_ui(mpq_denref(obj.numberq), 1) == 0 ? true_obj : false_obj;
  }
  case NUMBERR: {
    mpfr_t op;
    mpfr_init(op);
    mpfr_round(op, obj.numberr);
    int b = mpfr_equal_p(obj.numberr, op);
    mpfr_clear(op);
    return b ? true_obj : false_obj;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return false_obj;
    }
    mpfr_t op;
    mpfr_init(op);
    mpfr_round(op, obj.numberr);
    int b = mpfr_equal_p(mpc_realref(obj.numberc), op);
    mpfr_clear(op);
    return b ? true_obj : false_obj;
  }
  case NONE:
    error("scm_integer_p");
  default:
    return false_obj;
  }
}
Object scm_exact_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "exact?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return true_obj;
  case NUMBERR:
  case NUMBERC:
    return false_obj;
  case NONE:
    error("scm_exact_p");
  default:
    return wrong_type("exact?", args);
  }
}
Object scm_inexact_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "inexact?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return false_obj;
  case NUMBERR:
  case NUMBERC:
    return true_obj;
  case NONE:
    error("scm_inexact_p");
  default:
    return wrong_type("inexact?", args);
  }
}
Object scm_exact_integer_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "exact-integer?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
    return true_obj;
  case NUMBERQ: {
    mpq_canonicalize(obj.numberq);
    return mpz_cmp_ui(mpq_denref(obj.numberq), 1) == 0 ? true_obj : false_obj;
  }
  case NUMBERR:
  case NUMBERC:
    return false_obj;
  case NONE:
    error("scm_exact_integer_p");
  default:
    return wrong_type("exact-integer?", args);
  }
}

Object scm_finite_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "finite?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return true_obj;
  case NUMBERR:
    return mpfr_number_p(obj.numberr) ? true_obj : false_obj;
  case NUMBERC: {
    return mpfr_number_p(mpc_realref(obj.numberc)) &&
                   mpfr_number_p(mpc_imagref(obj.numberc))
               ? true_obj
               : false_obj;
  }
  case NONE:
    error("scm_finite_p");
  default:
    return wrong_type("finite?", args);
  }
}
Object scm_infinite_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "infinite?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return false_obj;
  case NUMBERR:
    return mpfr_inf_p(obj.numberr) ? true_obj : false_obj;
  case NUMBERC:
    return mpfr_inf_p(mpc_realref(obj.numberc)) ||
                   mpfr_inf_p(mpc_imagref(obj.numberc))
               ? true_obj
               : false_obj;
  case NONE:
    error("scm_infinite_p");
  default:
    return wrong_type("infinite?", args);
  }
}
Object scm_nan_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "nan?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return false_obj;
  case NUMBERR:
    return mpfr_nan_p(obj.numberr) ? true_obj : false_obj;
  case NUMBERC:
    return mpfr_nan_p(mpc_realref(obj.numberc)) ||
                   mpfr_nan_p(mpc_imagref(obj.numberc))
               ? true_obj
               : false_obj;
  case NONE:
    error("scm_nan_p");
  default:
    return wrong_type("nan?", args);
  }
}
#include <stdbool.h>
static Object math_equal_p(Object const obj1, Object const obj2,
                           Object const args) {
  bool b;
  switch (obj1.type) {
  case NUMBERZ: {
    switch (obj2.type) {
    case NUMBERZ: {
      b = mpz_cmp(obj1.numberz, obj2.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERQ: {
      b = mpq_cmp_z(obj2.numberq, obj1.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERR: {
      b = mpfr_cmp_z(obj2.numberr, obj1.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERC: {
      if (mpfr_cmp_ui(mpc_imagref(obj2.numberc), 0) != 0) {
        return false_obj;
      }
      b = mpfr_cmp_z(mpc_realref(obj2.numberc), obj1.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NONE:
      error("scm_math_equal_p");
    default:
      return wrong_type("=", args);
    }
  }
  case NUMBERQ: {
    switch (obj2.type) {
    case NUMBERZ: {
      b = mpq_cmp_z(obj1.numberq, obj2.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERQ: {
      b = mpq_cmp(obj2.numberq, obj1.numberq);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERR: {
      b = mpfr_cmp_q(obj2.numberr, obj1.numberq);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERC: {
      if (mpfr_cmp_ui(mpc_imagref(obj2.numberc), 0) != 0) {
        return false_obj;
      }
      b = mpfr_cmp_q(mpc_realref(obj2.numberc), obj1.numberq);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NONE:
      error("scm_math_equal_p");
    default:
      return wrong_type("=", args);
    }
  }
  case NUMBERR: {
    switch (obj2.type) {
    case NUMBERZ: {
      b = mpfr_cmp_z(obj1.numberr, obj2.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERQ: {
      b = mpfr_cmp_q(obj1.numberr, obj2.numberq);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERR: {
      b = mpfr_cmp(obj2.numberr, obj1.numberr);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERC: {
      if (mpfr_cmp_ui(mpc_imagref(obj2.numberc), 0) != 0) {
        return false_obj;
      }
      b = mpfr_cmp(obj1.numberr, mpc_realref(obj2.numberc));
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NONE:
      error("scm_math_equal_p");
    default:
      return wrong_type("=", args);
    }
  }
  case NUMBERC: {
    switch (obj2.type) {
    case NUMBERZ: {
      if (mpfr_cmp_ui(mpc_imagref(obj1.numberc), 0) != 0) {
        return false_obj;
      }
      b = mpfr_cmp_z(mpc_realref(obj1.numberc), obj2.numberz);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERQ: {
      if (mpfr_cmp_ui(mpc_imagref(obj1.numberc), 0) != 0) {
        return false_obj;
      }
      b = mpfr_cmp_q(mpc_realref(obj1.numberc), obj2.numberq);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERR: {
      if (mpfr_cmp_ui(mpc_imagref(obj1.numberc), 0) != 0) {
        return false_obj;
      }
      b = mpfr_cmp(mpc_realref(obj1.numberc), obj2.numberr);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NUMBERC: {
      b = mpc_cmp(obj1.numberc, obj2.numberc);
      if (b != 0) {
        return false_obj;
      }
      return true_obj;
    }
    case NONE:
      error("scm_math_equal_p");
    default:
      return wrong_type("=", args);
    }
  }
  case NONE:
    error("scm_math_equal_p");
  default:
    return wrong_type("=", args);
  }
}
Object scm_math_equal_p(Object const args) {
  if (args_length(args) < 2) {
    return arguments(args, "=");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  Object obj = math_equal_p(obj1, obj2, args);
  if (obj.type != TRUE_TYPE) {
    return obj;
  }
  for (Object rest = cdrref(cdrref(args)); rest.type != EMPTY;
       rest = cdrref(rest)) {
    Object obj2 = carref(rest);
    obj = math_equal_p(obj1, obj2, args);
    if (obj.type != TRUE_TYPE) {
      return obj;
    }
  }
  return true_obj;
}
Object scm_zero_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "zero?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
    return mpz_sgn(obj.numberz) == 0 ? true_obj : false_obj;
  case NUMBERQ:
    return mpq_sgn(obj.numberq) == 0 ? true_obj : false_obj;
  case NUMBERR:
    return mpfr_zero_p(obj.numberr) ? true_obj : false_obj;
  case NUMBERC:
    return mpc_cmp_si(obj.numberc, 0) == 0 ? true_obj : false_obj;
  case NONE:
    error("scm_zero_p");
  default:
    return wrong_type("zero?", args);
  }
}
Object scm_positive_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "posotive?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
    return mpz_sgn(obj.numberz) > 0 ? true_obj : false_obj;
  case NUMBERQ:
    return mpq_sgn(obj.numberq) > 0 ? true_obj : false_obj;
  case NUMBERR:
    return mpfr_sgn(obj.numberr) > 0 ? true_obj : false_obj;
  case NUMBERC:
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return wrong_type("positive?", args);
    }
    return mpfr_sgn(mpc_realref(obj.numberc)) > 0 ? true_obj : false_obj;
  case NONE:
    error("scm_positive_p");
  default:
    return wrong_type("positive?", args);
  }
}
Object scm_negative_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "negative?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
    return mpz_sgn(obj.numberz) < 0 ? true_obj : false_obj;
  case NUMBERQ:
    return mpq_sgn(obj.numberq) < 0 ? true_obj : false_obj;
  case NUMBERR:
    return mpfr_sgn(obj.numberr) < 0 ? true_obj : false_obj;
  case NUMBERC:
    return mpfr_sgn(obj.numberr) < 0 ? true_obj : false_obj;
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return wrong_type("negative?", args);
    }
    return mpfr_sgn(mpc_realref(obj.numberc)) < 0 ? true_obj : false_obj;
  case NONE:
    error("scm_negative_p");
  default:
    return wrong_type("negative?", args);
  }
}

Object scm_add(Object const args) {
  mpz_t opz;
  mpq_t opq, opq1;
  mpfr_t opfr;
  mpc_t opc;
  mpz_init_set_ui(opz, 0);
  mpq_inits(opq, opq1, NULL);
  mpfr_init(opfr);
  mpc_init2(opc, MPC_PREC);
  Type type = NUMBERZ;
  for (Object a = args; a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        mpz_add(opz, opz, arg.numberz);
        break;
      }
      case NUMBERQ: {
        type = NUMBERQ;
        mpq_set_z(opq, opz);
        mpq_add(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_add_z(opfr, arg.numberr, opz, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_z(opfr, opz, MPFR_RNDN);
        mpc_add_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("+", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_set_z(opq1, arg.numberz);
        mpq_add(opq, opq, opq1);
        break;
      }
      case NUMBERQ: {
        mpq_add(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_add_q(opfr, arg.numberr, opq, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_q(opfr, opq, MPFR_RNDN);
        mpc_add_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("+", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_add_z(opfr, opfr, arg.numberz, MPFR_RNDN);
        break;
      }
      case NUMBERQ: {
        mpfr_add_q(opfr, opfr, arg.numberq, MPFR_RNDN);
        break;
      }
      case NUMBERR: {
        mpfr_add(opfr, opfr, arg.numberr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpc_add_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("+", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_set_z(opfr, arg.numberz, MPFR_RNDN);
        mpc_add_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERQ: {
        mpfr_set_q(opfr, arg.numberq, MPFR_RNDN);
        mpc_add_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERR: {
        mpc_add_fr(opc, opc, arg.numberr, MPC_RNDNN);
        break;
      }
      case NUMBERC: {
        mpc_add(opc, opc, arg.numberc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("+", args);
      }
      break;
    }
    default:
      mpz_clear(opz);
      mpq_clears(opq, opq1, NULL);
      mpfr_clear(opfr);
      mpc_clear(opc);
      return wrong_type("+", args);
    }
  }
  Object out = none;
  switch (type) {
  case NUMBERZ: {
    out.type = NUMBERZ;
    mpz_init_set(out.numberz, opz);
    break;
  }
  case NUMBERQ: {
    out.type = NUMBERQ;
    mpq_canonicalize(opq);
    mpq_init(out.numberq);
    mpq_set(out.numberq, opq);
    break;
  }
  case NUMBERR: {
    out.type = NUMBERR;
    mpfr_init_set(out.numberr, opfr, MPFR_RNDN);
    break;
  }
  case NUMBERC: {
    out.type = NUMBERC;
    mpc_init2(out.numberc, MPC_PREC);
    mpc_set(out.numberc, opc, MPC_RNDNN);
    break;
  }
  default:
    error("scm_add");
    break;
  }
  mpz_clear(opz);
  mpq_clears(opq, opq1, NULL);
  mpfr_clear(opfr);
  mpc_clear(opc);
  return out;
}

Object scm_mul(Object const args) {
  mpz_t opz;
  mpq_t opq, opq1;
  mpfr_t opfr;
  mpc_t opc;
  mpz_init_set_ui(opz, 1);
  mpq_inits(opq, opq1, NULL);
  mpfr_init(opfr);
  mpc_init2(opc, MPC_PREC);
  Type type = NUMBERZ;
  for (Object a = args; a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        mpz_mul(opz, opz, arg.numberz);
        break;
      }
      case NUMBERQ: {
        type = NUMBERQ;
        mpq_set_z(opq, opz);
        mpq_mul(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_mul_z(opfr, arg.numberr, opz, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_z(opfr, opz, MPFR_RNDN);
        mpc_mul_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("*", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_set_z(opq1, arg.numberz);
        mpq_mul(opq, opq, opq1);
        break;
      }
      case NUMBERQ: {
        mpq_mul(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_mul_q(opfr, arg.numberr, opq, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_q(opfr, opq, MPFR_RNDN);
        mpc_mul_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("*", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_mul_z(opfr, opfr, arg.numberz, MPFR_RNDN);
        break;
      }
      case NUMBERQ: {
        mpfr_mul_q(opfr, opfr, arg.numberq, MPFR_RNDN);
        break;
      }
      case NUMBERR: {
        mpfr_mul(opfr, opfr, arg.numberr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpc_mul_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("*", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_set_z(opfr, arg.numberz, MPFR_RNDN);
        mpc_mul_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERQ: {
        mpfr_set_q(opfr, arg.numberq, MPFR_RNDN);
        mpc_mul_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERR: {
        mpc_mul_fr(opc, opc, arg.numberr, MPC_RNDNN);
        break;
      }
      case NUMBERC: {
        mpc_mul(opc, opc, arg.numberc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("*", args);
      }
      break;
    }
    default:
      mpz_clear(opz);
      mpq_clears(opq, opq1, NULL);
      mpfr_clear(opfr);
      mpc_clear(opc);
      return wrong_type("*", args);
    }
  }
  Object out = none;
  switch (type) {
  case NUMBERZ: {
    out.type = NUMBERZ;
    mpz_init_set(out.numberz, opz);
    break;
  }
  case NUMBERQ: {
    mpq_canonicalize(opq);
    out.type = NUMBERQ;
    mpq_init(out.numberq);
    mpq_set(out.numberq, opq);
    break;
  }
  case NUMBERR: {
    out.type = NUMBERR;
    mpfr_init_set(out.numberr, opfr, MPFR_RNDN);
    break;
  }
  case NUMBERC: {
    out.type = NUMBERC;
    mpc_init2(out.numberc, MPC_PREC);
    mpc_set(out.numberc, opc, MPC_RNDNN);
    break;
  }
  default:
    error("scm_mul");
    break;
  }
  mpz_clear(opz);
  mpq_clears(opq, opq1, NULL);
  mpfr_clear(opfr);
  mpc_clear(opc);
  return out;
}

Object scm_sub(Object const args) {
  if (args_length(args) == 0) {
    return arguments(args, "-");
  }
  Object out = none;
  size_t len = args_length(args);
  Object obj1 = value(carref(args));
  mpz_t opz;
  mpq_t opq, opq1;
  mpfr_t opfr;
  mpc_t opc;
  Type type = obj1.type;
  mpz_init(opz);
  mpq_inits(opq, opq1, NULL);
  mpfr_init(opfr);
  mpc_init2(opc, MPC_PREC);
  switch (type) {
  case NUMBERZ: {
    mpz_set(opz, obj1.numberz);
    break;
  }
  case NUMBERQ: {
    mpq_set(opq, obj1.numberq);
    break;
  }
  case NUMBERR: {
    mpfr_set(opfr, obj1.numberr, MPFR_RNDN);
    break;
  }
  case NUMBERC: {
    mpc_set(opc, obj1.numberc, MPC_RNDNN);
    break;
  }
  case NONE:
    error("scm_sub");
  default:
    return wrong_type("-", args);
  }
  if (len == 1) {
    out.type = type;
    switch (type) {
    case NUMBERZ: {
      mpz_init(out.numberz);
      mpz_neg(out.numberz, opz);
      break;
    }
    case NUMBERQ: {
      mpq_init(out.numberq);
      mpq_neg(out.numberq, opq);
      break;
    }
    case NUMBERR: {
      mpfr_init(out.numberr);
      mpfr_neg(out.numberr, opfr, MPFR_RNDN);
      break;
    }
    case NUMBERC: {
      mpc_init2(out.numberc, MPC_PREC);
      mpc_neg(out.numberc, opc, MPC_RNDNN);
      break;
    }
    case NONE:
      error("scm_sub");
    default:
      return wrong_type("-", args);
    }
    mpz_clear(opz);
    mpq_clears(opq, opq1, NULL);
    mpfr_clear(opfr);
    mpc_clear(opc);
    return out;
  }
  for (Object a = cdrref(args); a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        mpz_sub(opz, opz, arg.numberz);
        break;
      }
      case NUMBERQ: {
        type = NUMBERQ;
        mpq_set_z(opq, opz);
        mpq_sub(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_sub_z(opfr, arg.numberr, opz, MPFR_RNDN);
        mpfr_neg(opfr, opfr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_z(opfr, opz, MPFR_RNDN);
        mpc_sub_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        mpc_neg(opc, opc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("-", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_set_z(opq1, arg.numberz);
        mpq_sub(opq, opq, opq1);
        break;
      }
      case NUMBERQ: {
        mpq_sub(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_sub_q(opfr, arg.numberr, opq, MPFR_RNDN);
        mpfr_neg(opfr, opfr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_q(opfr, opq, MPFR_RNDN);
        mpc_sub_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        mpc_neg(opc, opc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("-", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_sub_z(opfr, opfr, arg.numberz, MPFR_RNDN);
        break;
      }
      case NUMBERQ: {
        mpfr_sub_q(opfr, opfr, arg.numberq, MPFR_RNDN);
        break;
      }
      case NUMBERR: {
        mpfr_sub(opfr, opfr, arg.numberr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpc_sub_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        mpc_neg(opc, opc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("-", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_set_z(opfr, arg.numberz, MPFR_RNDN);
        mpc_sub_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERQ: {
        mpfr_set_q(opfr, arg.numberq, MPFR_RNDN);
        mpc_sub_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERR: {
        mpc_sub_fr(opc, opc, arg.numberr, MPC_RNDNN);
        break;
      }
      case NUMBERC: {
        mpc_sub(opc, opc, arg.numberc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("-", args);
      }
      break;
    }
    default:
      mpz_clear(opz);
      mpq_clears(opq, opq1, NULL);
      mpfr_clear(opfr);
      mpc_clear(opc);
      return wrong_type("-", args);
    }
  }
  switch (type) {
  case NUMBERZ: {
    out.type = NUMBERZ;
    mpz_init_set(out.numberz, opz);
    break;
  }
  case NUMBERQ: {
    mpq_canonicalize(opq);
    out.type = NUMBERQ;
    mpq_init(out.numberq);
    mpq_set(out.numberq, opq);
    break;
  }
  case NUMBERR: {
    out.type = NUMBERR;
    mpfr_init_set(out.numberr, opfr, MPFR_RNDN);
    break;
  }
  case NUMBERC: {
    out.type = NUMBERC;
    mpc_init2(out.numberc, MPC_PREC);
    mpc_set(out.numberc, opc, MPC_RNDNN);
    break;
  }
  default:
    error("scm_sub");
    break;
  }
  mpz_clear(opz);
  mpq_clears(opq, opq1, NULL);
  mpfr_clear(opfr);
  mpc_clear(opc);
  return out;
}
Object scm_div(Object const args) {
  if (args_length(args) == 0) {
    return arguments(args, "/");
  }
  Object out = none;
  size_t len = args_length(args);
  Object obj1 = value(carref(args));
  mpz_t opz;
  mpq_t opq, opq1;
  mpfr_t opfr;
  mpc_t opc;
  Type type = obj1.type;
  mpz_init(opz);
  mpq_inits(opq, opq1, NULL);
  mpfr_init(opfr);
  mpc_init2(opc, MPC_PREC);
  switch (type) {
  case NUMBERZ: {
    mpz_set(opz, obj1.numberz);
    break;
  }
  case NUMBERQ: {
    mpq_set(opq, obj1.numberq);
    break;
  }
  case NUMBERR: {
    mpfr_set(opfr, obj1.numberr, MPFR_RNDN);
    break;
  }
  case NUMBERC: {
    mpc_set(opc, obj1.numberc, MPC_RNDNN);
    break;
  }
  case NONE:
    error("scm_div");
  default:
    return wrong_type("/", args);
  }
  if (len == 1) {
    out.type = type;
    switch (type) {
    case NUMBERZ: {
      out.type = NUMBERQ;
      mpq_init(out.numberq);
      mpz_set_ui(mpq_numref(out.numberq), 1);
      mpz_set(mpq_denref(out.numberq), opz);
      break;
    }
    case NUMBERQ: {
      mpq_init(out.numberq);
      mpq_inv(out.numberq, opq);
      break;
    }
    case NUMBERR: {
      mpfr_init(out.numberr);
      mpfr_ui_div(out.numberr, 1, opfr, MPFR_RNDN);
      break;
    }
    case NUMBERC: {
      mpc_init2(out.numberc, MPC_PREC);
      mpc_ui_div(out.numberc, 1, opc, MPC_RNDNN);
      break;
    }
    case NONE:
      error("scm_div");
    default:
      return wrong_type("/", args);
    }
    mpz_clear(opz);
    mpq_clears(opq, opq1, NULL);
    mpfr_clear(opfr);
    mpc_clear(opc);
    return out;
  }
  for (Object a = cdrref(args); a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        type = NUMBERQ;
        mpz_set(mpq_numref(opq), opz);
        mpz_set(mpq_denref(opq), arg.numberz);
        break;
      }
      case NUMBERQ: {
        type = NUMBERQ;
        mpq_set_z(opq, opz);
        mpq_div(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_div_z(opfr, arg.numberr, opz, MPFR_RNDN);
        mpfr_ui_div(opfr, 1, opfr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_z(opfr, opz, MPFR_RNDN);
        mpc_div_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        mpc_ui_div(opc, 1, opc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("/", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_set_z(opq1, arg.numberz);
        mpq_div(opq, opq, opq1);
        break;
      }
      case NUMBERQ: {
        mpq_div(opq, opq, arg.numberq);
        break;
      }
      case NUMBERR: {
        type = NUMBERR;
        mpfr_div_q(opfr, arg.numberr, opq, MPFR_RNDN);
        mpfr_ui_div(opfr, 1, opfr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpfr_set_q(opfr, opq, MPFR_RNDN);
        mpc_div_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        mpc_ui_div(opc, 1, opc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("/", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_div_z(opfr, opfr, arg.numberz, MPFR_RNDN);
        break;
      }
      case NUMBERQ: {
        mpfr_div_q(opfr, opfr, arg.numberq, MPFR_RNDN);
        break;
      }
      case NUMBERR: {
        mpfr_div(opfr, opfr, arg.numberr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        type = NUMBERC;
        mpc_div_fr(opc, arg.numberc, opfr, MPC_RNDNN);
        mpc_ui_div(opc, 1, opc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("/", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_set_z(opfr, arg.numberz, MPFR_RNDN);
        mpc_div_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERQ: {
        mpfr_set_q(opfr, arg.numberq, MPFR_RNDN);
        mpc_div_fr(opc, opc, opfr, MPC_RNDNN);
        break;
      }
      case NUMBERR: {
        mpc_div_fr(opc, opc, arg.numberr, MPC_RNDNN);
        break;
      }
      case NUMBERC: {
        mpc_div(opc, opc, arg.numberc, MPC_RNDNN);
        break;
      }
      default:
        mpz_clear(opz);
        mpq_clears(opq, opq1, NULL);
        mpfr_clear(opfr);
        mpc_clear(opc);
        return wrong_type("/", args);
      }
      break;
    }
    default:
      mpz_clear(opz);
      mpq_clears(opq, opq1, NULL);
      mpfr_clear(opfr);
      mpc_clear(opc);
      return wrong_type("/", args);
    }
  }
  switch (type) {
  case NUMBERZ: {
    out.type = NUMBERZ;
    mpz_init_set(out.numberz, opz);
    break;
  }
  case NUMBERQ: {
    mpq_canonicalize(opq);
    out.type = NUMBERQ;
    mpq_init(out.numberq);
    mpq_set(out.numberq, opq);
    break;
  }
  case NUMBERR: {
    out.type = NUMBERR;
    mpfr_init_set(out.numberr, opfr, MPFR_RNDN);
    break;
  }
  case NUMBERC: {
    out.type = NUMBERC;
    mpc_init2(out.numberc, MPC_PREC);
    mpc_set(out.numberc, opc, MPC_RNDNN);
    break;
  }
  default:
    error("scm_div");
    break;
  }
  mpz_clear(opz);
  mpq_clears(opq, opq1, NULL);
  mpfr_clear(opfr);
  mpc_clear(opc);
  return out;
}

Object scm_abs(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "abs");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERZ};
    mpz_init(out.numberz);
    mpz_abs(out.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERQ};
    mpq_init(out.numberq);
    mpq_abs(out.numberq, obj.numberq);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_abs(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NONE:
    error("scm_abs");
  default:
    return wrong_type("abs", args);
  }
}
Object scm_numerator(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "numerator");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, mpq_numref(obj.numberq));
    return out;
  }
  case NUMBERR: {
    if (scm_rational_p(args).type == FALSE_TYPE) {
      return wrong_type("numberator", args);
    }
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, obj.numberr, MPFR_RNDN);
    mpq_t opq;
    mpq_init(opq);
    mpq_set_f(opq, op);
    mpf_clear(op);
    Object out = {.type = NUMBERR};
    mpfr_set_z(out.numberr, mpq_numref(opq), MPFR_RNDN);
    mpq_clear(opq);
    return out;
  }
  case NUMBERC: {
    if (scm_rational_p(args).type == FALSE_TYPE) {
      return wrong_type("numerator", args);
    }
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, mpc_realref(obj.numberc), MPFR_RNDN);
    mpq_t opq;
    mpq_init(opq);
    mpq_set_f(opq, op);
    mpf_clear(op);
    Object out = {.type = NUMBERR};
    mpfr_set_z(out.numberr, mpq_numref(opq), MPFR_RNDN);
    mpq_clear(opq);
    return out;
  }
  default:
    return wrong_type("numerator", args);
  }
}
Object scm_denominator(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "denominator");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    return numberz_new("1", 10);
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, mpq_denref(obj.numberq));
    return out;
  }
  case NUMBERR: {
    if (scm_rational_p(args).type == FALSE_TYPE) {
      return wrong_type("denominator", args);
    }
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, obj.numberr, MPFR_RNDN);
    mpq_t opq;
    mpq_init(opq);
    mpq_set_f(opq, op);
    mpf_clear(op);
    Object out = {.type = NUMBERR};
    mpfr_init_set_z(out.numberr, mpq_denref(opq), MPFR_RNDN);
    mpq_clear(opq);
    return out;
  }
  case NUMBERC: {
    if (scm_rational_p(args).type == FALSE_TYPE) {
      return wrong_type("denominator", args);
    }
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, mpc_realref(obj.numberc), MPFR_RNDN);
    mpq_t opq;
    mpq_init(opq);
    mpq_set_f(opq, op);
    mpf_clear(op);
    Object out = {.type = NUMBERR};
    mpfr_set_z(out.numberr, mpq_denref(opq), MPFR_RNDN);
    mpq_clear(opq);
    return out;
  }
  default:
    return wrong_type("denominator", args);
  }
}
Object scm_floor(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "floor");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERZ};
    mpz_init(out.numberz);
    mpz_fdiv_q(out.numberz, mpq_numref(obj.numberq), mpq_denref(obj.numberq));
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_floor(out.numberr, obj.numberr);
    return out;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return wrong_type("floor", args);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_floor(out.numberr, mpc_realref(obj.numberc));
    return out;
  }
  default:
    return wrong_type("floor", args);
  }
}
Object scm_ceiling(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "ceiling");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERZ};
    mpz_init(out.numberz);
    mpz_cdiv_q(out.numberz, mpq_numref(obj.numberq), mpq_denref(obj.numberq));
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_ceil(out.numberr, obj.numberr);
    return out;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return wrong_type("ceiling", args);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_ceil(out.numberr, mpc_realref(obj.numberc));
    return out;
  }
  default:
    return wrong_type("ceiling", args);
  }
}
Object scm_truncate(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "truncate");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERZ};
    mpz_init(out.numberz);
    mpz_tdiv_q(out.numberz, mpq_numref(obj.numberq), mpq_denref(obj.numberq));
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_trunc(out.numberr, obj.numberr);
    return out;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return wrong_type("truncate", args);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_trunc(out.numberr, mpc_realref(obj.numberc));
    return out;
  }
  default:
    return wrong_type("truncate", args);
  }
}
Object scm_exp(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "exp");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_sgn(obj.numberz) == 0) {
      return numberz_new("1", 10);
    }
    mpfr_t op;
    mpfr_init_set_z(op, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_exp(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("1", 10);
    }
    mpfr_t op;
    mpfr_init_set_q(op, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_exp(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_exp(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_exp(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_exp");
  default:
    return wrong_type("exp", args);
  }
}
Object scm_log(Object const args) {
  size_t len = args_length(args);
  switch (len) {
  case 1: {
    Object obj = value(carref(args));
    switch (obj.type) {
    case NUMBERZ: {
      if (mpz_cmp_ui(obj.numberz, 1) == 0) {
        return numberz_new("0", 10);
      }
      mpc_t op;
      mpc_init2(op, MPC_PREC);
      mpc_set_z(op, obj.numberz, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, op, MPC_RNDNN);
      mpc_clear(op);
      return out;
    }
    case NUMBERQ: {
      if (mpq_cmp_ui(obj.numberq, 1, 1) == 0) {
        return numberz_new("0", 10);
      }
      mpc_t op;
      mpc_init2(op, MPC_PREC);
      mpc_set_q(op, obj.numberq, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, op, MPC_RNDNN);
      mpc_clear(op);
      return out;
    }
    case NUMBERR: {
      mpc_t op;
      mpc_init2(op, MPC_PREC);
      mpc_set_fr(op, obj.numberr, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, op, MPC_RNDNN);
      mpc_clear(op);
      return out;
    }
    case NUMBERC: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, obj.numberc, MPC_RNDNN);
      return out;
    }
    case NONE:
      error("scm_log");
    default:
      return wrong_type("log", args);
    }
  }
  case 2: {
    Object obj1 = value(carref(args));
    Object obj2 = value(carref(cdrref(args)));
    switch (obj1.type) {
    case NUMBERZ: {
      switch (obj2.type) {
      case NUMBERZ: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
        mpc_set_z(op2, obj2.numberz, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERQ: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
        mpc_set_q(op2, obj2.numberq, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERR: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
        mpc_set_fr(op2, obj2.numberr, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERC: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NONE:
        error("scm_log");
      default:
        return wrong_type("log", args);
      }
    }
    case NUMBERQ: {
      switch (obj2.type) {
      case NUMBERZ: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
        mpc_set_z(op2, obj2.numberz, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERQ: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
        mpc_set_q(op2, obj2.numberq, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERR: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
        mpc_set_fr(op2, obj2.numberr, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERC: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_t op1, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NONE:
        error("scm_log");
      default:
        return wrong_type("log", args);
      }
    }
    case NUMBERR: {
      switch (obj2.type) {
      case NUMBERZ: {
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
        mpc_set_z(op2, obj2.numberz, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERQ: {
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
        mpc_set_q(op2, obj2.numberq, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERR: {
        mpc_t op1, op2, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
        mpc_set_fr(op2, obj2.numberr, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERC: {
        mpc_t op1, rop1, rop2;
        mpc_init2(op1, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
        mpc_log(rop1, op1, MPC_RNDNN);
        mpc_log(rop2, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op1);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NONE:
        error("scm_log");
      default:
        return wrong_type("log", args);
      }
    }
    case NUMBERC: {
      switch (obj2.type) {
      case NUMBERZ: {
        mpc_t op2, rop1, rop2;
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_z(op2, obj2.numberz, MPC_RNDNN);
        mpc_log(rop1, obj1.numberc, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERQ: {
        mpc_t op2, rop1, rop2;
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_q(op2, obj2.numberq, MPC_RNDNN);
        mpc_log(rop1, obj1.numberc, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERR: {
        mpc_t op2, rop1, rop2;
        mpc_init2(op2, MPC_PREC);
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_set_fr(op2, obj2.numberr, MPC_RNDNN);
        mpc_log(rop1, obj1.numberc, MPC_RNDNN);
        mpc_log(rop2, op2, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(op2);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NUMBERC: {
        mpc_t rop1, rop2;
        mpc_init2(rop1, MPC_PREC);
        mpc_init2(rop2, MPC_PREC);
        mpc_log(rop1, obj1.numberc, MPC_RNDNN);
        mpc_log(rop2, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, rop1, rop2, MPC_RNDNN);
        mpc_clear(rop1);
        mpc_clear(rop2);
        return out;
      }
      case NONE:
        error("scm_log");
      default:
        return wrong_type("log", args);
      }
    }
    case NONE:
      error("scm_log");
    default:
      return wrong_type("log", args);
    }
  }
  case NONE:
    error("scm_log");
  default:
    return arguments(args, "log");
  }
}
Object scm_sin(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "sin");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_sgn(obj.numberz) == 0) {
      return numberz_new("0", 10);
    }
    mpfr_t op;
    mpfr_init_set_z(op, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sin(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("0", 0);
    }
    mpfr_t op;
    mpfr_init_set_q(op, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sin(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sin(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_sin(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_sin");
  default:
    return wrong_type("sin", args);
  }
}
Object scm_cos(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "cos");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_sgn(obj.numberz) == 0) {
      return numberz_new("1", 10);
    }
    mpfr_t op;
    mpfr_init_set_z(op, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_cos(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("1", 0);
    }
    mpfr_t op;
    mpfr_init_set_q(op, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_cos(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_cos(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_cos(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_cos");
  default:
    return wrong_type("cos", args);
  }
}
Object scm_tan(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "tan");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    mpfr_t op;
    mpfr_init_set_z(op, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_tan(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERQ: {
    mpfr_t op;
    mpfr_init_set_q(op, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_tan(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_tan(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_tan(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_tan");
  default:
    return wrong_type("tan", args);
  }
}
Object scm_asin(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "asin");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_sgn(obj.numberz) == 0) {
      return numberz_new("0", 0);
    }
    mpc_t op;
    mpc_init2(op, MPC_PREC);
    mpc_set_z(op, obj.numberz, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, op, MPC_RNDNN);
    mpc_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("0", 0);
    }
    mpc_t op;
    mpc_init2(op, MPC_PREC);
    mpc_set_q(op, obj.numberq, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, op, MPC_RNDNN);
    mpc_clear(op);
    return out;
  }
  case NUMBERR: {
    mpc_t op;
    mpc_init2(op, MPC_PREC);
    mpc_set_fr(op, obj.numberr, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, op, MPC_RNDNN);
    mpc_clear(op);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_asin");
  default:
    return wrong_type("asin", args);
  }
}
Object scm_acos(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "acos");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_cmp_ui(obj.numberz, 1) == 0) {
      return numberz_new("0", 0);
    }
    mpc_t op;
    mpc_init2(op, MPC_PREC);
    mpc_set_z(op, obj.numberz, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_acos(out.numberc, op, MPC_RNDNN);
    mpc_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpq_cmp_ui(obj.numberq, 1, 1) == 0) {
      return numberz_new("0", 0);
    }
    mpc_t op;
    mpc_init2(op, MPC_PREC);
    mpc_set_q(op, obj.numberq, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_acos(out.numberc, op, MPC_RNDNN);
    mpc_clear(op);
    return out;
  }
  case NUMBERR: {
    mpc_t op;
    mpc_init2(op, MPC_PREC);
    mpc_set_fr(op, obj.numberr, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, op, MPC_RNDNN);
    mpc_clear(op);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_acos(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_acos");
  default:
    return wrong_type("acos", args);
  }
}

Object scm_atan(Object const args) {
  size_t len = args_length(args);
  switch (len) {
  case 1: {
    Object obj = value(carref(args));
    switch (obj.type) {
    case NUMBERZ: {
      if (mpz_sgn(obj.numberz) == 0) {
        return numberz_new("0", 10);
      }
      mpfr_t op;
      mpfr_init_set_z(op, obj.numberz, MPFR_RNDN);
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_atan(out.numberr, op, MPC_RNDNN);
      mpfr_clear(op);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj.numberq) == 0) {
        return numberz_new("0", 10);
      }
      mpfr_t op;
      mpfr_init_set_q(op, obj.numberq, MPFR_RNDN);
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_atan(out.numberr, op, MPC_RNDNN);
      mpfr_clear(op);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_atan(out.numberr, obj.numberr, MPC_RNDNN);
      return out;
    }
    case NUMBERC: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_atan(out.numberc, obj.numberc, MPC_RNDNN);
      return out;
    }
    case NONE:
      error("scm_atan");
    default:
      return wrong_type("atan", args);
    }
  }
  case 2: {
    Object obj1 = value(carref(args));
    Object obj2 = value(carref(cdrref(args)));
    switch (obj1.type) {
    case NUMBERZ: {
      int sign1 = mpz_sgn(obj1.numberz);
      switch (obj2.type) {
      case NUMBERZ: {
        if (sign1 == 0 && mpz_sgn(obj2.numberz) >= 0) {
          return numberz_new("0", 10);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpc_set_z_z(rop, obj2.numberz, obj1.numberz, MPC_RNDNN);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERQ: {
        if (sign1 == 0 && mpq_sgn(obj2.numberq) >= 0) {
          return numberz_new("0", 10);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpq_t op1;
        mpq_init(op1);
        mpq_set_z(op1, obj1.numberz);
        mpc_set_q_q(rop, obj2.numberq, op1, MPC_RNDNN);
        mpq_clear(op1);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_z(op1, obj1.numberz, MPFR_RNDN);
        mpc_set_fr_fr(rop, obj2.numberr, op1, MPC_RNDNN);
        mpfr_clear(op1);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_z(op1, obj1.numberz, MPFR_RNDN);
        mpc_set_fr_fr(rop, mpc_realref(obj2.numberc), op1, MPC_RNDNN);
        mpfr_clear(op1);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NONE:
        error("scm_atan");
      default:
        return wrong_type("atan", args);
      }
    }
    case NUMBERQ: {
      int sign1 = mpq_sgn(obj1.numberq);
      switch (obj2.type) {
      case NUMBERZ: {
        if (sign1 == 0 && mpq_sgn(obj2.numberq) >= 0) {
          return numberz_new("0", 10);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpq_t op2;
        mpq_init(op2);
        mpq_set_z(op2, obj2.numberz);
        mpc_set_q_q(rop, op2, obj1.numberq, MPC_RNDNN);
        mpq_clear(op2);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERQ: {
        if (sign1 == 0 && mpq_sgn(obj2.numberq) >= 0) {
          return numberz_new("0", 10);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpc_set_q_q(rop, obj2.numberq, obj1.numberq, MPC_RNDNN);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_q(op1, obj1.numberq, MPFR_RNDN);
        mpc_set_fr_fr(rop, obj2.numberr, op1, MPC_RNDNN);
        mpfr_clear(op1);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_q(op1, obj1.numberq, MPFR_RNDN);
        mpc_set_fr_fr(rop, mpc_realref(obj2.numberc), op1, MPC_RNDNN);
        mpfr_clear(op1);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NONE:
        error("scm_atan");
      default:
        return wrong_type("atan", args);
      }
    }
    case NUMBERR: {
      switch (obj2.type) {
      case NUMBERZ: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op2;
        mpfr_init_set_z(op2, obj2.numberz, MPFR_RNDN);
        mpc_set_fr_fr(rop, op2, obj1.numberr, MPC_RNDNN);
        mpfr_clear(op2);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERQ: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op2;
        mpfr_init_set_q(op2, obj2.numberq, MPFR_RNDN);
        mpc_set_fr_fr(rop, op2, obj1.numberr, MPC_RNDNN);
        mpfr_clear(op2);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpc_set_fr_fr(rop, obj2.numberr, obj1.numberr, MPC_RNDNN);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpc_set_fr_fr(rop, mpc_realref(obj2.numberc), obj1.numberr, MPC_RNDNN);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NONE:
        error("scm_atan");
      default:
        return wrong_type("atan", args);
      }
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj1.numberc))) {
        return wrong_type("atan", args);
      }
      switch (obj2.type) {
      case NUMBERZ: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op2;
        mpfr_init_set_z(op2, obj2.numberz, MPFR_RNDN);
        mpc_set_fr_fr(rop, op2, mpc_realref(obj1.numberc), MPC_RNDNN);
        mpfr_clear(op2);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERQ: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_init2(rop, MPC_PREC);
        mpfr_t op2;
        mpfr_init_set_q(op2, obj2.numberq, MPFR_RNDN);
        mpc_set_fr_fr(rop, op2, mpc_realref(obj1.numberc), MPC_RNDNN);
        mpfr_clear(op2);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_set_fr_fr(rop, obj2.numberr, mpc_realref(obj1.numberc), MPC_RNDNN);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_t rop;
        mpc_set_fr_fr(rop, mpc_realref(obj2.numberc), mpc_realref(obj1.numberc),
                      MPC_RNDNN);
        mpc_arg(out.numberr, rop, MPFR_RNDN);
        mpc_clear(rop);
        return out;
      }
      case NONE:
        error("scm_atan");
      default:
        return wrong_type("atan", args);
      }
    }
    case NONE:
      error("scm_atan");
    default:
      return wrong_type("atan", args);
    }
  }
  default:
    return arguments(args, "atan");
  }
}
Object scm_square(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "square");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERZ};
    mpz_init(out.numberz);
    mpz_mul(out.numberz, obj.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERQ};
    mpq_init(out.numberq);
    mpq_mul(out.numberq, obj.numberq, obj.numberq);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_mul(out.numberr, obj.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_mul(out.numberc, obj.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_square");
  default:
    return wrong_type("square", args);
  }
}
Object scm_sqrt(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "sqrt");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_perfect_square_p(obj.numberz)) {
      Object out = {.type = NUMBERZ};
      mpz_init(out.numberz);
      mpz_sqrt(out.numberz, obj.numberz);
      return out;
    }
    mpfr_t op;
    mpfr_init_set_z(op, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sqrt(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpz_perfect_square_p(mpq_numref(obj.numberq)) &&
        mpz_perfect_square_p(mpq_denref(obj.numberq))) {
      Object out = {.type = NUMBERQ};
      mpq_init(out.numberq);
      mpz_sqrt(mpq_numref(out.numberq), mpq_numref(obj.numberq));
      mpz_sqrt(mpq_denref(out.numberq), mpq_denref(obj.numberq));
      return out;
    }
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sqrt(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_sqrt(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case NONE:
    error("scm_sqrt");
  default:
    return wrong_type("sqrt", args);
  }
}
Object scm_exact_integer_sqrt(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "exact-integer-sqrt");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_sgn(obj.numberz) < 0) {
      return wrong_type("exact-integer-sqrt", args);
    }
    Object out1 = {.type = NUMBERZ};
    Object out2 = {.type = NUMBERZ};
    mpz_inits(out1.numberz, out2.numberz, NULL);
    mpz_sqrtrem(out1.numberz, out2.numberz, obj.numberz);
    Object out = empty;
    out = cons(out2, out);
    out = cons(out1, out);
    out.type = MULTIPLE;
    return out;
  }
  case NONE:
    error("scm_exact_integer_sqrt");
  default:
    return wrong_type("exact-integer-sqrt", args);
  }
}
Object scm_expt(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "real-part");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  switch (obj1.type) {
  case NUMBERZ: {
    int sign1 = mpz_sgn(obj1.numberz);
    switch (obj2.type) {
    case NUMBERZ: {
      int sign2 = mpz_sgn(obj2.numberz);
      if (sign1 == 0) {
        if (sign2 == 0) {
          return numberz_new("1", 10);
        }
        if (sign2 > 0) {
          return numberz_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpfr_t op1, rop;
      mpfr_init_set_z(op1, obj1.numberz, MPFR_RNDN);
      mpfr_init(rop);
      Object out;
      if (sign2 > 0) {
        out.type = NUMBERZ;
        mpfr_pow_z(rop, op1, obj2.numberz, MPFR_RNDN);
        mpz_init(out.numberz);
        mpfr_get_z(out.numberz, rop, MPFR_RNDN);
      } else {
        out.type = NUMBERQ;
        mpz_t op2;
        mpz_init(op2);
        mpz_neg(op2, obj2.numberz);
        mpfr_pow_z(rop, op1, op2, MPFR_RNDN);
        mpz_clear(op2);
        mpz_t op3;
        mpz_init(op3);
        mpfr_get_z(op3, rop, MPFR_RNDN);
        mpq_init(out.numberq);
        mpz_set_ui(mpq_numref(out.numberq), 1);
        mpz_set(mpq_denref(out.numberq), op3);
        mpz_clear(op3);
      }
      mpfr_clears(op1, rop, NULL);
      return out;
    }
    case NUMBERQ: {
      if (sign1 == 0) {
        int sign2 = mpq_sgn(obj2.numberq);
        if (sign2 == 0) {
          return numberz_new("1", 10);
        }
        if (sign2 > 0) {
          return numberz_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      mpfr_t op2;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpfr_init(op2);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
      mpfr_set_q(op2, obj2.numberq, MPFR_RNDN);
      mpc_pow_fr(out.numberc, op1, op2, MPFR_RNDN);
      mpc_clear(op1);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERR: {
      if (sign1 == 0) {
        int sign2 = mpfr_sgn(obj2.numberr);
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
      mpc_pow_fr(out.numberc, op1, obj2.numberr, MPFR_RNDN);
      mpc_clear(op1);
      return out;
    }
    case NUMBERC: {
      if (sign1 == 0) {
        int sign2_real = mpfr_sgn(mpc_realref(obj2.numberc));
        int sign2_imag = mpfr_sgn(mpc_imagref(obj2.numberc));
        if (sign2_real == 0 && sign2_imag == 0) {
          return numberr_new("1", 10);
        }
        if (sign2_real > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      mpc_init2(op1, MPC_PREC);
      mpc_set_z(op1, obj1.numberz, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow(out.numberc, op1, obj2.numberc, MPC_RNDNN);
      mpc_clear(op1);
      return out;
    }
    case NONE:
      error("scm_expt");
    default:
      return wrong_type("expt", args);
    }
  }
  case NUMBERQ: {
    int sign1 = mpq_sgn(obj1.numberq);
    switch (obj2.type) {
    case NUMBERZ: {
      int sign2 = mpz_sgn(obj2.numberz);
      if (sign1 == 0) {
        if (sign2 == 0) {
          return numberz_new("1", 10);
        }
        if (sign2 > 0) {
          return numberz_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      Object out = {.type = NUMBERQ};
      mpq_init(out.numberq);
      mpfr_t op1_num, op1_den, rop_num, rop_den;
      mpfr_inits(op1_num, op1_den, rop_num, rop_den, NULL);
      mpfr_set_z(op1_num, mpq_numref(obj1.numberq), MPFR_RNDN);
      mpfr_set_z(op1_den, mpq_denref(obj1.numberq), MPFR_RNDN);
      if (sign2 > 0) {
        mpfr_pow_z(rop_num, op1_num, obj2.numberz, MPFR_RNDN);
        mpfr_pow_z(rop_den, op1_den, obj2.numberz, MPFR_RNDN);
        mpfr_get_z(mpq_numref(out.numberq), rop_num, MPFR_RNDN);
        mpfr_get_z(mpq_denref(out.numberq), rop_den, MPFR_RNDN);
      } else {
        mpz_t op2;
        mpz_init(op2);
        mpz_abs(op2, obj2.numberz);
        mpfr_pow_z(rop_num, op1_num, op2, MPFR_RNDN);
        mpfr_pow_z(rop_den, op1_den, op2, MPFR_RNDN);
        mpz_clear(op2);
        mpfr_get_z(mpq_numref(out.numberq), rop_den, MPFR_RNDN);
        mpfr_get_z(mpq_denref(out.numberq), rop_num, MPFR_RNDN);
      }
      mpfr_clears(op1_num, op1_den, rop_num, rop_den, NULL);
      return out;
    }
    case NUMBERQ: {
      int sign2 = mpq_sgn(obj2.numberq);
      if (sign1 == 0) {
        if (sign2 == 0) {
          return numberz_new("1", 10);
        }
        if (sign2 > 0) {
          return numberz_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      mpfr_t op2;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpfr_init(op2);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
      mpfr_set_q(op2, obj2.numberq, MPFR_RNDN);
      mpc_pow_fr(out.numberc, op1, op2, MPC_PREC);
      mpc_clear(op1);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERR: {
      int sign2 = mpfr_sgn(obj2.numberr);
      if (sign1 == 0) {
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
      mpc_pow_fr(out.numberc, op1, obj2.numberr, MPFR_RNDN);
      mpc_clear(op1);
      return out;
    }
    case NUMBERC: {
      if (sign1 == 0) {
        int sign2_real = mpfr_sgn(mpc_realref(obj2.numberc));
        int sign2_imag = mpfr_sgn(mpc_imagref(obj2.numberc));
        if (sign2_real == 0 && sign2_imag == 0) {
          return numberr_new("1", 10);
        }
        if (sign2_real > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q(op1, obj1.numberq, MPC_RNDNN);
      mpc_pow(out.numberc, op1, obj2.numberc, MPFR_RNDN);
      mpc_clear(op1);
      return out;
    }
    case NONE:
      error("scm_expt");
    default:
      return wrong_type("expt", args);
    }
  }
  case NUMBERR: {
    int sign1 = mpfr_sgn(obj1.numberr);
    switch (obj2.type) {
    case NUMBERZ: {
      if (sign1 == 0) {
        int sign2 = mpz_sgn(obj2.numberz);
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_pow_z(out.numberr, obj1.numberr, obj2.numberz, MPFR_RNDN);
      return out;
    }
    case NUMBERQ: {
      int sign2 = mpfr_sgn(obj2.numberr);
      if (sign1 == 0) {
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      mpfr_t op2;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpfr_init(op2);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
      mpfr_set_q(op2, obj2.numberq, MPFR_RNDN);
      mpc_pow_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpc_clear(op1);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERR: {
      int sign2 = mpfr_sgn(obj2.numberr);
      if (sign1 == 0) {
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
      mpc_pow_fr(out.numberc, op1, obj2.numberr, MPC_RNDNN);
      mpc_clear(op1);
      return out;
    }
    case NUMBERC: {
      if (sign1 == 0) {
        int sign2_real = mpfr_sgn(mpc_realref(obj2.numberc));
        int sign2_imag = mpfr_sgn(mpc_imagref(obj2.numberc));
        if (sign2_real == 0 && sign2_imag) {
          return numberr_new("1", 10);
        }
        if (sign2_real > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpc_t op1;
      Object out = {.type = NUMBERC};
      mpc_init2(op1, MPC_PREC);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr(op1, obj1.numberr, MPC_RNDNN);
      mpc_pow(out.numberc, op1, obj2.numberc, MPC_RNDNN);
      mpc_clear(op1);
      return out;
    }
    case NONE:
      error("scm_expt");
    default:
      return wrong_type("expt", args);
    }
  }
  case NUMBERC: {
    int sign1_real = mpfr_sgn(mpc_realref(obj1.numberc));
    int sign1_imag = mpfr_sgn(mpc_imagref(obj1.numberc));
    switch (obj2.type) {
    case NUMBERZ: {
      if (sign1_real == 0 && sign1_imag == 0) {
        int sign2 = mpz_sgn(obj2.numberz);
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow_z(out.numberc, obj1.numberc, obj2.numberz, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      if (sign1_real == 0 && sign1_imag == 0) {
        int sign2 = mpq_sgn(obj2.numberq);
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      mpfr_t op2;
      mpfr_init_set_q(op2, obj2.numberq, MPFR_RNDN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow_fr(out.numberc, obj1.numberc, op2, MPC_RNDNN);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERR: {
      if (sign1_real == 0 && sign1_imag == 0) {
        int sign2 = mpfr_sgn(obj2.numberr);
        if (sign2 == 0) {
          return numberr_new("1", 10);
        }
        if (sign2 > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow_fr(out.numberc, obj1.numberc, obj2.numberr, MPC_RNDNN);
      return out;
    }
    case NUMBERC: {
      if (sign1_real == 0 && sign1_imag == 0) {
        int sign2_real = mpfr_sgn(mpc_realref(obj2.numberc));
        int sign2_imag = mpfr_sgn(mpc_imagref(obj2.numberc));
        if (sign2_real == 0 && sign2_imag == 0) {
          return numberr_new("1", 10);
        }
        if (sign2_real > 0) {
          return numberr_new("0", 10);
        }
        fprintf(yyout, "Error -- (expt ");
        object_write(yyout, obj1);
        fprintf(yyout, " ");
        object_write(yyout, obj2);
        fprintf(yyout, ")\n");
        return (Object){.type = EXPT_ERROR};
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow(out.numberc, obj1.numberc, obj2.numberc, MPC_RNDNN);
      return out;
    }
    case NONE:
      error("scm_expt");
    default:
      return wrong_type("expt", args);
    }
  }
  case NONE:
    error("scm_expt");
  default:
    return wrong_type("expt", args);
  }
}
Object scm_make_rectangular(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "make-rectangular");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  switch (obj1.type) {
  case NUMBERZ: {
    switch (obj2.type) {
    case NUMBERZ: {
      if (mpz_sgn(obj2.numberz) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_z_z(out.numberc, obj1.numberz, obj2.numberz, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj2.numberq) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpq_t op1;
      mpq_init(op1);
      mpq_set_z(op1, obj1.numberz);
      mpc_set_q_q(out.numberc, op1, obj2.numberq, MPC_RNDNN);
      mpq_clear(op1);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op1;
      mpfr_init_set_z(op1, obj1.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, op1, obj2.numberr, MPC_RNDNN);
      mpfr_clear(op1);
      return out;
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return wrong_type("make-rectangular", args);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op1;
      mpfr_init_set_z(op1, obj1.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, op1, mpc_realref(obj2.numberc), MPC_RNDNN);
      mpfr_clear(op1);
      return out;
    }
    case NONE:
      error("scm_make_rectangular");
    default:
      return wrong_type("make-rectangular", args);
    }
  }
  case NUMBERQ: {
    switch (obj2.type) {
    case NUMBERZ: {
      if (mpz_sgn(obj2.numberz) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpq_t op2;
      mpq_init(op2);
      mpq_set_z(op2, obj2.numberz);
      mpc_set_q_q(out.numberc, obj1.numberq, op2, MPC_RNDNN);
      mpq_clear(op2);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj2.numberq) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q_q(out.numberc, obj1.numberq, obj2.numberq, MPC_RNDNN);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op1;
      mpfr_init_set_q(op1, obj1.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, op1, obj2.numberr, MPC_RNDNN);
      mpfr_clear(op1);
      return out;
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return wrong_type("make-rectangular", args);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op1;
      mpfr_init_set_q(op1, obj1.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, op1, mpc_realref(obj2.numberc), MPC_RNDNN);
      mpfr_clear(op1);
      return out;
    }
    case NONE:
      error("scm_make_rectangular");
    default:
      return wrong_type("make-rectangular", args);
    }
  }
  case NUMBERR: {
    switch (obj2.type) {
    case NUMBERZ: {
      if (mpz_sgn(obj2.numberz) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op2;
      mpfr_init_set_z(op2, obj2.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, obj1.numberr, op2, MPC_RNDNN);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj2.numberq) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op2;
      mpfr_init_set_q(op2, obj2.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, obj1.numberr, op2, MPC_RNDNN);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, obj1.numberr, obj2.numberr, MPC_RNDNN);
      return out;
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return wrong_type("make-rectangular", args);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, obj1.numberr, mpc_realref(obj2.numberc),
                    MPC_RNDNN);
      return out;
    }
    case NONE:
      error("scm_make_rectangular");
    default:
      return wrong_type("make-rectangular", args);
    }
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj1.numberc))) {
      return wrong_type("make-rectangular", args);
    }
    switch (obj2.type) {
    case NUMBERZ: {
      if (mpz_sgn(obj2.numberz) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op2;
      mpfr_init_set_z(op2, obj2.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, mpc_realref(obj1.numberc), op2, MPC_RNDNN);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj2.numberq) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_t op2;
      mpfr_init_set_q(op2, obj2.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, mpc_realref(obj1.numberc), op2, MPC_RNDNN);
      mpfr_clear(op2);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, mpc_realref(obj1.numberc), obj2.numberr,
                    MPC_RNDNN);
      return out;
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return wrong_type("make-rectangular", args);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, mpc_realref(obj1.numberc),
                    mpc_realref(obj2.numberc), MPC_RNDNN);
      return out;
    }
    case NONE:
      error("scm_make_rectangular");
    default:
      return wrong_type("make-rectangular", args);
    }
  }
  case NONE:
    error("scm_make_rectangular");
  default:
    return wrong_type("make-rectangular", args);
  }
}
Object scm_make_polar(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "make-polar");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  switch (obj1.type) {
  case NUMBERZ: {
    switch (obj2.type) {
    case NUMBERZ: {
      Object out = {.type = NUMBERC};
      mpfr_t op, sop, cop, op1, op2;
      mpfr_inits(op, sop, cop, op1, op2, NULL);
      mpfr_set_z(op, obj2.numberz, MPFR_RNDN);
      mpfr_sin_cos(sop, cop, op, MPFR_RNDN);
      mpfr_mul_z(op1, cop, obj1.numberz, MPFR_RNDN);
      mpfr_mul_z(op2, sop, obj1.numberz, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(op, sop, cop, op1, op2, NULL);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERC};
      mpfr_t op, sop, cop, op1, op2;
      mpfr_inits(op, sop, cop, op1, op2, NULL);
      mpfr_set_q(op, obj2.numberq, MPFR_RNDN);
      mpfr_sin_cos(sop, cop, op, MPFR_RNDN);
      mpfr_mul_z(op1, cop, obj1.numberz, MPFR_RNDN);
      mpfr_mul_z(op2, sop, obj1.numberz, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(op, sop, cop, op1, op2, NULL);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpfr_t sop, cop, op1, op2;
      mpfr_inits(sop, cop, op1, op2, NULL);
      mpfr_sin_cos(sop, cop, obj2.numberr, MPFR_RNDN);
      mpfr_mul_z(op1, cop, obj1.numberz, MPFR_RNDN);
      mpfr_mul_z(op2, sop, obj1.numberz, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(sop, cop, op1, op2, NULL);
      return out;
    }
    case NONE:
      error("scm_make_polar");
    default:
      return wrong_type("make-polar", args);
    }
  }
  case NUMBERQ: {
    switch (obj2.type) {
    case NUMBERZ: {
      Object out = {.type = NUMBERC};
      mpfr_t op, sop, cop, op1, op2;
      mpfr_inits(op, sop, cop, op1, op2, NULL);
      mpfr_set_z(op, obj2.numberz, MPFR_RNDN);
      mpfr_sin_cos(sop, cop, op, MPFR_RNDN);
      mpfr_mul_q(op1, cop, obj1.numberq, MPFR_RNDN);
      mpfr_mul_q(op2, sop, obj1.numberq, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(op, sop, cop, op1, op2, NULL);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERC};
      mpfr_t op, sop, cop, op1, op2;
      mpfr_inits(op, sop, cop, op1, op2, NULL);
      mpfr_set_q(op, obj2.numberq, MPFR_RNDN);
      mpfr_sin_cos(sop, cop, op, MPFR_RNDN);
      mpfr_mul_q(op1, cop, obj1.numberq, MPFR_RNDN);
      mpfr_mul_q(op2, sop, obj1.numberq, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(op, sop, cop, op1, op2, NULL);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpfr_t sop, cop, op1, op2;
      mpfr_inits(sop, cop, op1, op2, NULL);
      mpfr_sin_cos(sop, cop, obj2.numberr, MPFR_RNDN);
      mpfr_mul_q(op1, cop, obj1.numberq, MPFR_RNDN);
      mpfr_mul_q(op2, sop, obj1.numberq, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(sop, cop, op1, op2, NULL);
      return out;
    }
    case NONE:
      error("scm_make_polar");
    default:
      return wrong_type("make-polar", args);
    }
  }
  case NUMBERR: {
    switch (obj2.type) {
    case NUMBERZ: {
      Object out = {.type = NUMBERC};
      mpfr_t op, sop, cop, op1, op2;
      mpfr_inits(op, sop, cop, op1, op2, NULL);
      mpfr_set_z(op, obj2.numberz, MPFR_RNDN);
      mpfr_sin_cos(sop, cop, op, MPFR_RNDN);
      mpfr_mul(op1, cop, obj1.numberr, MPFR_RNDN);
      mpfr_mul(op2, sop, obj1.numberr, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(op, sop, cop, op1, op2, NULL);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERC};
      mpfr_t op, sop, cop, op1, op2;
      mpfr_inits(op, sop, cop, op1, op2, NULL);
      mpfr_set_q(op, obj2.numberq, MPFR_RNDN);
      mpfr_sin_cos(sop, cop, op, MPFR_RNDN);
      mpfr_mul(op1, cop, obj1.numberr, MPFR_RNDN);
      mpfr_mul(op2, sop, obj1.numberr, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(op, sop, cop, op1, op2, NULL);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpfr_t sop, cop, op1, op2;
      mpfr_inits(sop, cop, op1, op2, NULL);
      mpfr_sin_cos(sop, cop, obj2.numberr, MPFR_RNDN);
      mpfr_mul(op1, cop, obj1.numberr, MPFR_RNDN);
      mpfr_mul(op2, sop, obj1.numberr, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, op1, op2, MPC_RNDNN);
      mpfr_clears(sop, cop, op1, op2, NULL);
      return out;
    }
    case NONE:
      error("scm_make_polar");
    default:
      return wrong_type("make-polar", args);
    }
  }
  case NONE:
    error("scm_make_polar");
  default:
    return wrong_type("make-polar", args);
  }
}
Object scm_real_part(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "real-part");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
    return object_copy(obj);
  case NUMBERC: {
    Object out = {.type = NUMBERR};
    mpfr_init_set(out.numberr, mpc_realref(obj.numberc), MPFR_RNDN);
    return out;
  }
  case NONE:
    error("scm_real_part");
  default:
    return wrong_type("real-part", args);
  }
}
Object scm_imag_part(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "imag-part");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
    return numberz_new("0", 10);
  case NUMBERC: {
    Object out = {.type = NUMBERR};
    mpfr_init_set(out.numberr, mpc_imagref(obj.numberc), MPFR_RNDN);
    return out;
  }
  case NONE:
    error("scm_imag_part");
  default:
    return wrong_type("imag-part", args);
  }
}
Object scm_magnitude(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "magnitude");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = obj.type};
    mpz_init(out.numberz);
    mpz_abs(out.numberz, obj.numberz);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = obj.type};
    mpq_init(out.numberq);
    mpq_abs(out.numberq, obj.numberq);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = obj.type};
    mpfr_init(out.numberr);
    mpfr_abs(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpc_abs(out.numberr, obj.numberc, MPFR_RNDN);
    return out;
  }
  case NONE:
    error("scm_magnitude");
  default:
    return wrong_type("magnitude", args);
  }
}
Object scm_angle(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "angle");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    if (mpz_sgn(obj.numberz) >= 0) {
      return numberz_new("0", 10);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_const_pi(out.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) >= 0) {
      return numberz_new("0", 10);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_const_pi(out.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERR: {
    if (mpfr_sgn(obj.numberr) >= 0) {
      return numberr_new("0", 10);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_const_pi(out.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpc_arg(out.numberr, obj.numberc, MPFR_RNDN);
    return out;
  }
  case NONE:
    error("angle");
  default:
    return wrong_type("angle", args);
  }
}
Object scm_inexact(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "inexact");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    Object out = {.type = NUMBERR};
    mpfr_init_set_z(out.numberr, obj.numberz, MPFR_RNDN);
    return out;
  }
  case NUMBERQ: {
    Object out = {.type = NUMBERR};
    mpfr_init_set_q(out.numberr, obj.numberq, MPFR_RNDN);
    return out;
  }
  case NUMBERR:
  case NUMBERC:
    return object_copy(obj);
  case NONE:
    error("scm_inexact");
  default:
    return wrong_type("inexact", args);
  }
}
Object scm_exact(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "exact");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
    return object_copy(obj);
  case NUMBERR: {
    Object out = {.type = NUMBERQ};
    mpq_init(out.numberq);
    mpf_t op;
    mpf_init(op);
    mpfr_get_f(op, obj.numberr, MPFR_RNDN);
    mpq_set_f(out.numberq, op);
    mpf_clear(op);
    return out;
  }
  case NUMBERC: {
    fprintf(yyout, "Error -- (exact ");
    object_write(yyout, obj);
    fprintf(yyout, ")\n");
    return (Object){.type = EXACT_ERROR};
  }
  case NONE:
    error("scm_inexact");
  default:
    return wrong_type("ixact", args);
  }
}
/* Numbers end */

/* Booleans */
Object scm_not(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "not");
  }
  switch (value(carref(args)).type) {
  case FALSE_TYPE:
    return true_obj;
  case NONE:
    error("scm_not");
  default:
    return false_obj;
  }
}
Object scm_boolean_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "not");
  }
  switch (value(carref(args)).type) {
  case TRUE_TYPE:
  case FALSE_TYPE:
    return true_obj;
  case NONE:
    error("scm_not");
  default:
    return false_obj;
  }
}

Object scm_boolean_eq_p(Object const args) {
  if (args_length(args) < 2) {
    return arguments(args, "not");
  }
  Object obj1 = value(carref(args));
  bool flag;
  switch (obj1.type) {
  case TRUE_TYPE:
    flag = true;
    break;
  case FALSE_TYPE:
    flag = false;
    break;
  case NONE:
    error("scm_boolean_eq_p");
  default:
    return wrong_type("boolean=?", args);
  }
  Object obj = value(carref(cdrref(args)));
  if (flag) {
    switch (obj.type) {
    case TRUE_TYPE:
      break;
    case FALSE_TYPE:
      return false_obj;
    case NONE:
      error("scm_boolean_eq_p");
    default:
      return wrong_type("boolean=?", args);
    }
  } else {
    switch (obj.type) {
    case TRUE_TYPE:
      return false_obj;
    case FALSE_TYPE:
      break;
    case NONE:
      error("scm_boolean_eq_p");
    default:
      return wrong_type("boolean=?", args);
    }
  }
  if (flag) {
    for (Object rest = cdrref(cdrref(args)); rest.type != EMPTY;
         rest = cdrref(rest)) {
      Object obj = value(carref(rest));
      switch (obj.type) {
      case TRUE_TYPE:
        break;
      case FALSE_TYPE:
        return false_obj;
      case NONE:
        error("scm_boolean_eq_p");
      default:
        return wrong_type("boolean=?", args);
      }
    }
  } else {
    for (Object rest = cdrref(cdrref(args)); rest.type != EMPTY;
         rest = cdrref(rest)) {
      Object obj = value(carref(rest));
      switch (obj.type) {
      case TRUE_TYPE:
        return false_obj;
      case FALSE_TYPE:
        break;
      case NONE:
        error("scm_boolean_eq_p");
      default:
        return wrong_type("boolean=?", args);
      }
    }
  }
  return true_obj;
}
/* Booleans end */

/* Pairs and lists */
Object scm_pair_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "pair?");
  }
  switch (value(carref(args)).type) {
  case PAIR:
    return true_obj;
  case NONE:
    error("scm_pair_p");
  default:
    return false_obj;
  }
}
Object scm_cons(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "cons");
  }
  return cons(car(args), car(cdrref(args)));
}
Object scm_car(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "car");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PAIR:
    return car(obj);
  case NONE:
    error("scm_car");
  default:
    return wrong_type("car", args);
  }
}
Object scm_cdr(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "cdr");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PAIR:
    return cdr(obj);
  case NONE:
    error("scm_cdr");
  default:
    return wrong_type("cdr", args);
  }
}
Object scm_null_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "cdr");
  }
  switch (value(carref(args)).type) {
  case EMPTY:
    return true_obj;
  case NONE:
    error("scm_null_p");
  default:
    return false_obj;
  }
}
Object scm_list(Object const args) { return args; }

Object scm_set_car(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "set-car!");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PAIR: {
    object_free(&cars[obj.index]);
    cars[obj.index] = car(cdrref(args));
    return unspecified;
  }
  case NONE:
    error("scm_set_car");
  default:
    return wrong_type("set-car!", args);
  }
}
Object scm_set_cdr(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "set-cdr!");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PAIR: {
    object_free(&cdrs[obj.index]);
    cdrs[obj.index] = car(cdrref(args));
    return unspecified;
  }
  case NONE:
    error("scm_set_cdr");
  default:
    return wrong_type("set-cdr!", args);
  }
}
/* Pairs and lists and */

/* Symbols */
Object scm_symbol_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "symbol?");
  }
  switch (value(carref(args)).type) {
  case IDENTIFIER:
    return true_obj;
  case NONE:
    error("scm_symbol_p");
  default:
    return false_obj;
  }
}
/* Symbols end */
/* Characters */
Object scm_char_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char?");
  }
  switch (value(carref(args)).type) {
  case CHARACTER:
    return true_obj;
  case NONE:
    error("scm_char_p");
  default:
    return false_obj;
  }
}
Object scm_char_alphabetic_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-alphabetic?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return g_unichar_isalpha(obj.character) ? true_obj : false_obj;
  case NONE:
    error("scm_char_alphabetic_p");
  default:
    return wrong_type("char-alphabetic?", args);
  }
}
Object scm_char_numeric_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-numeric?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return g_unichar_isdigit(obj.character) ? true_obj : false_obj;
  case NONE:
    error("scm_char_numeric_p");
  default:
    return wrong_type("char-numeric?", args);
  }
}
Object scm_char_whitespace_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-whitespace?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return g_unichar_isspace(obj.character) ? true_obj : false_obj;
  case NONE:
    error("scm_char_whitespace_p");
  default:
    return wrong_type("char-whitespace?", args);
  }
}
Object scm_char_upper_case_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-upper-case?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return !g_unichar_isalpha(obj.character)
               ? wrong_type("char-upper-case?", args)
               : g_unichar_isupper(obj.character) ? true_obj : false_obj;
  default:
    return wrong_type("char-upper-case?", args);
  }
  exit(1);
}
Object scm_char_lower_case_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-lower-case?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return !g_unichar_isalpha(obj.character)
               ? wrong_type("char-lower-case?", args)
               : g_unichar_islower(obj.character) ? true_obj : false_obj;
  default:
    return wrong_type("char-lower-case?", args);
  }
  exit(1);
}
Object scm_digit_value(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "digit-value");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER: {
    gint n = g_unichar_digit_value(obj.character);
    if (n == -1)
      return false_obj;
    Object out = {.type = NUMBERZ};
    mpz_init_set_si(out.numberz, n);
    return out;
  }
  default:
    return wrong_type("digit-value", args);
  }
  exit(1);
}
Object scm_char_tointeger(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char->integer");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER: {
    Object out = {.type = NUMBERZ};
    mpz_init_set_si(out.numberz, obj.character);
    return out;
  }
  default:
    return wrong_type("char->integer", args);
  }
  exit(1);
}
Object scm_integer_tochar(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "integer->char");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    return (Object){.type = CHARACTER, .character = mpz_get_ui(obj.numberz)};
  }
  case NUMBERQ: {
    mpq_canonicalize(obj.numberq);
    if (mpz_cmp_ui(mpq_denref(obj.numberq), 1) == 0) {
      return (Object){.type = CHARACTER,
                      .character = mpz_get_ui(mpq_numref(obj.numberq))};
    }
    return wrong_type("integer->char", args);
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("integer->char", args);
  }
  exit(1);
}
Object scm_char_upcase(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-upcase");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER: {
    return (Object){.type = CHARACTER,
                    .character = g_unichar_toupper(obj.character)};
  }
  default:
    return wrong_type("char-upcase", args);
  }
  exit(1);
}
Object scm_char_downcase(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-downcase");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER: {
    return (Object){.type = CHARACTER,
                    .character = g_unichar_tolower(obj.character)};
  }
  default:
    return wrong_type("char-downcase", args);
  }
  exit(1);
}
Object scm_char_foldcase(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-foldcase");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER: {
    char outbuf[7];
    gint len = g_unichar_to_utf8(obj.character, outbuf);
    outbuf[len] = '\0';
    char *s = g_utf8_casefold(outbuf, -1);
    Object out = character_new(s);
    free(s);
    return out;
  }
  default:
    return wrong_type("char-foldcase", args);
  }
  exit(1);
}

/* Characters end */

/* Strings */
Object scm_string_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "string?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_make_string(Object const args) {
  switch (args_length(args)) {
  case 1: {
    Object k = value(carref(args));
    if (k.type != NUMBERZ) {
      return wrong_type("make-string", args);
    }
    Object c = {.type = CHARACTER, .character = ' '};
    Object out = string_empty;
    for (size_t i = mpz_get_ui(k.numberz); i > 0; i--) {
      out = string_cons(c, out);
    }
    return out;
  }
  case 2: {
    Object k = value(carref(args));
    Object c = carref(cdrref(args));
    if (k.type != NUMBERZ || c.type != CHARACTER) {
      return wrong_type("make-string", args);
    }
    Object out = string_empty;
    for (size_t i = mpz_get_ui(k.numberz); i > 0; i--) {
      out = string_cons(c, out);
    }
    return out;
  }
  default:
    return arguments(args, "make-string");
  }
}
static Object string_reverse(Object obj) {
  Object out = string_empty;
  for (Object t = obj; t.type != STRING_EMPTY; t = string_cdrref(t)) {
    out = string_cons(carref(t), out);
  }
  return out;
}
Object scm_string(Object const args) {
  Object out = string_empty;
  for (Object t = args; t.type != EMPTY; t = cdrref(t)) {
    Object c = value(carref(t));
    if (c.type != CHARACTER) {
      return wrong_type("string", args);
    }
    out = string_cons(c, out);
  }
  return string_reverse(out);
}
Object scm_string_length(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "string-length");
  }
  size_t len = 0;
  switch (value(carref(args)).type) {
  case STRING_EMPTY:
  case STRING: {
    for (Object obj = carref(args); obj.type != STRING_EMPTY;
         obj = string_cdrref(obj)) {
      len++;
    }
    Object out = {.type = NUMBERZ};
    mpz_init_set_ui(out.numberz, len);
    return out;
  }
  default:
    return wrong_type("string-length", args);
  }
  exit(1);
}
Object scm_string_ref(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "string-ref");
  }
  Object s = value(carref(args));
  Object k = value(carref(cdrref(args)));
  if ((s.type != STRING_EMPTY && s.type != STRING) || k.type != NUMBERZ) {
    return wrong_type("string-ref", args);
  }
  size_t i = mpz_get_ui(k.numberz);
  for (; i > 0; i--) {
    s = string_cdrref(s);
  }
  Object c = string_carref(s);
  return c;
}
Object scm_string_set(Object const args) {
  if (args_length(args) != 3) {
    return arguments(args, "string-set!");
  }
  Object s = value(carref(args));
  Object k = value(carref(cdrref(args)));
  if ((s.type != STRING_EMPTY && s.type != STRING) || k.type != NUMBERZ) {
    return wrong_type("string-set!", args);
  }
  Object c = value(carref(cdrref(cdrref(args))));
  size_t i = mpz_get_ui(k.numberz);
  for (; i > 0; i--) {
    s = string_cdrref(s);
  }
  cars[s.index] = c;
  return unspecified;
}
/* Strings end */
/* Vectors */
Object scm_vector_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "vector?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case VECTOR:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_vector(Object const args) { return list2vector(args); }
Object scm_vector_length(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "vector-length");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case VECTOR: {
    Object out = {.type = NUMBERZ};
    mpz_init_set_ui(out.numberz, cdrs[obj.index].vector_length);
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("vector-length", args);
  }
  exit(1);
}
Object scm_vector_ref(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "vector-ref");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case VECTOR: {
    Object k = value(carref(cdrref(args)));
    if (k.type != NUMBERZ) {
      return wrong_type("vector-ref", args);
    }
    size_t i = mpz_get_ui(k.numberz);
    if (i >= cdrs[obj.index].vector_length) {
      exit(1);
    }
    return object_copy(cars[obj.index + i]);
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("vector-ref", args);
  }
  exit(1);
}
Object scm_vector_set(Object const args) {
  if (args_length(args) != 3) {
    return arguments(args, "vector-set!");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case VECTOR: {
    Object k = value(carref(cdrref(args)));
    if (k.type != NUMBERZ) {
      return wrong_type("vector-set!", args);
    }
    Object obj0 = car(cdrref(cdrref(args)));
    size_t i = mpz_get_ui(k.numberz);
    if (i >= cdrs[obj.index].vector_length) {
      exit(1);
    }
    object_free(&cars[obj.index + i]);
    cars[obj.index + i] = obj0;
    return unspecified;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("vector-set!", args);
  }
  exit(1);
}

/* Vectors end */
/* Bytevectors */
Object scm_bytevector_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "bytevector?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case BYTEVECTOR:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_bytevector(Object const args) { return list2bytevector(args); }
Object scm_bytevector_length(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "bytevector-length");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case BYTEVECTOR: {
    Object out = {.type = NUMBERZ};
    mpz_init_set_ui(out.numberz, cdrs[obj.index].vector_length);
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("bytevetor-length", args);
  }
  exit(1);
}

Object scm_bytevector_ueight_ref(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "bytevector-u8-ref");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case BYTEVECTOR: {
    Object k = value(carref(cdrref(args)));
    if (k.type != NUMBERZ) {
      wrong_type("bytevetor-u8-ref", args);
    }
    size_t i = mpz_get_ui(k.numberz);
    if (i >= cdrs[obj.index].bytevector_length) {
      exit(1);
    }
    return object_copy(cars[obj.index + i]);
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("bytevetor-u8-ref", args);
  }
  exit(1);
}
Object scm_bytevector_ueight_set(Object const args) {
  if (args_length(args) != 3) {
    return arguments(args, "bytevector-u8-set!");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case BYTEVECTOR: {
    Object k = value(carref(cdrref(args)));
    if (k.type != NUMBERZ) {
      return wrong_type("bytevetor-u8-set!", args);
    }
    Object obj0 = car(cdrref(cdrref(args)));
    size_t i = mpz_get_ui(k.numberz);
    if (i >= cdrs[obj.index].bytevector_length) {
      exit(1);
    }
    object_free(&cars[obj.index + i]);
    cars[obj.index + i] = obj0;
    return unspecified;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("bytevetor-u8-set!", args);
  }
  exit(1);
}
Object scm_utfeight_tostring(Object const args) {
  Object obj;
  size_t start;
  size_t end;
  switch (args_length(args)) {
  case 1: {
    obj = value(carref(args));
    if (obj.type != BYTEVECTOR) {
      return wrong_type("utf8->bytevetor", args);
    }
    start = 0;
    end = cdrs[obj.index].bytevector_length;
    break;
  }
  case 2: {
    obj = value(carref(args));
    if (obj.type != BYTEVECTOR || carref(cdrref(args)).type != NUMBERZ) {
      return wrong_type("utf8->string", args);
    }
    start = mpz_get_ui(carref(cdrref(args)).numberz);
    end = cdrs[obj.index].bytevector_length;
    break;
  }
  case 3: {
    obj = value(carref(args));
    if (obj.type != BYTEVECTOR || carref(cdrref(args)).type != NUMBERZ ||
        carref(cdrref(cdrref(args))).type != NUMBERZ) {
      return wrong_type("utf8->string", args);
    }
    start = mpz_get_ui(carref(cdrref(args)).numberz);
    end = mpz_get_ui(carref(cdrref(cdrref(args))).numberz);
    break;
  }
  default:
    return arguments(args, "utf8->string");
  }
  if (end <= start) {
    exit(1);
  }
  switch (obj.type) {
  case BYTEVECTOR: {
    size_t bv_len = cdrs[obj.index].bytevector_length;
    size_t len = end - start;
    if (bv_len < len) {
      exit(1);
    }
    char str[len];
    for (size_t i = 0; i < len; i++) {
      str[i] = mpz_get_ui(cars[obj.index + start + i].numberz);
    }
    glong items_read, items_written;
    GError *error;
    gunichar *unichars =
        g_utf8_to_ucs4(str, len, &items_read, &items_written, &error);
    if (items_read != len) {
      fprintf(stderr, "ksi error scm_utfeight_tostring\n");
      exit(1);
    }
    Object out = string_empty;
    for (glong i = items_written - 1; i >= 0; i--) {
      out = string_cons((Object){.type = CHARACTER, .character = unichars[i]},
                        out);
    }
    g_free(unichars);
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("ut8->string", args);
  }
  exit(1);
}
Object scm_string_toutfeight(Object const args) {
  Object obj;
  size_t start;
  size_t end;
  switch (args_length(args)) {
  case 1: {
    obj = value(carref(args));
    start = 0;
    if (obj.type != STRING && obj.type != STRING_EMPTY) {
      return wrong_type("string->utf8", args);
    }
    end = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = cdrref(o)) {
      end++;
    }
    break;
  }
  case 2: {
    obj = value(carref(args));
    if (value(carref(cdrref(args))).type != NUMBERZ) {
      return wrong_type("string->utf8", args);
    }
    start = mpz_get_ui(value(carref(cdrref(args))).numberz);
    if (obj.type != STRING && obj.type != STRING_EMPTY) {
      return wrong_type("string->utf8", args);
    }
    end = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = cdrref(o)) {
      end++;
    }
    break;
  }
  case 3: {
    obj = value(carref(args));
    if (value(carref(cdrref(args))).type != NUMBERZ) {
      return wrong_type("string->utf8", args);
    }
    start = mpz_get_ui(value(carref(cdrref(args))).numberz);
    if (carref(cdrref(cdrref(args))).type != NUMBERZ) {
      return wrong_type("string->utf8", args);
    }
    end = mpz_get_ui(carref(cdrref(cdrref(args))).numberz);
    break;
  }
  default:
    return arguments(args, "string->utf8");
  }
  if (end <= start) {
    return list2bytevector(empty);
  }
  switch (obj.type) {
  case STRING:
  case STRING_EMPTY: {
    size_t len = end - start;
    gunichar str[len];
    for (size_t i = 0; i < start; i++) {
      obj = cdrref(obj);
    }
    for (size_t i = 0; i < len; i++) {
      str[i] = carref(obj).character;
      obj = cdrref(obj);
    }
    glong items_read, items_written;
    GError *error;
    gchar *s = g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
    Object t = empty;
    for (glong i = items_written - 1; i >= 0; i--) {
      Object o = {.type = NUMBERZ};
      mpz_init_set_ui(o.numberz, (uint8_t)s[i]);
      t = cons(o, t);
    }
    g_free(s);
    Object out = list2bytevector(t);
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("string->utf8", args);
  }
  exit(1);
}
/* Bytevectors end */
/* Control features */
Object scm_procedure_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "procedure?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PRIMITIVE_PROCEDURE:
  case PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    return true_obj;

  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
/* Control features end */
/* Exceptions */
Object scm_error_implementation_defined_object(Object const args) {
  if (args_length(args) < 1) {
    return arguments(args, "error-implementation-defined-object");
  }
  Object out = args;
  out.type = IMPLEMENTATION_DEFINED_OBJECT;
  return out;
}
Object scm_error_object_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "error-object?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case IMPLEMENTATION_DEFINED_OBJECT: {
    return true_obj;
  }
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_error_object_irritants(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "error-object-irrtants");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case IMPLEMENTATION_DEFINED_OBJECT:
    return implementation_defined_object_cdrref(obj);
  case NONE:
    exit(1);
  default:
    return wrong_type("error-object-irritants", args);
  }
  exit(1);
}
Object scm_error_object_message(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "error-object-message");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case IMPLEMENTATION_DEFINED_OBJECT:
    return implementation_defined_object_carref(obj);
  case NONE:
    exit(1);
  default:
    return wrong_type("error-object-message", args);
  }
  exit(1);
}
Object scm_file_error_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "file-error?");
  }
  return value(carref(args)).type == FILE_ERROR ? true_obj : false_obj;
}
/* Exceptions end */
/* Input and output */
Object scm_input_port_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "input-port?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_output_port_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "output-port?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_textual_port_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "textual-port?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT:
  case PORT_INPUT_TEXT:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_binary_port_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "binary-port?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_BINARY:
  case PORT_INPUT_BINARY:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_input_port_open_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "input-port-open?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
    return port_carref(obj).port != NULL ? true_obj : false_obj;
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
    return false_obj;
  case NONE:
    exit(1);
  default:
    exit(1);
  }
  return wrong_type("input-port-open?", args);
}
Object scm_output_port_open_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "output-port-open?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
    return port_carref(obj).port != NULL ? true_obj : false_obj;
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
    return false_obj;
  case NONE:
    exit(1);
  default:
    return wrong_type("output-port-open?", args);
  }
  exit(1);
}
Object scm_open_output_file(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "open-output-file");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING: {
    size_t len = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = string_cdrref(o)) {
      len++;
    }
    gunichar str[len];
    Object o = obj;
    for (size_t i = 0; i < len; i++) {
      str[i] = string_carref(o).character;
      o = string_cdrref(o);
    }
    glong items_read, items_written;
    GError *error;
    char *s = g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
    FILE *f = fopen(s, "w");
    if (f == NULL) {
      exit(1);
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_OUTPUT_TEXT;
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("open-output-file", args);
  }
  exit(1);
}
Object scm_open_binary_output_file(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "open-binary-output-file");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING: {
    size_t len = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = string_cdrref(o)) {
      len++;
    }
    gunichar str[len];
    Object o = obj;
    for (size_t i = 0; i < len; i++) {
      str[i] = string_carref(o).character;
      o = string_cdrref(o);
    }
    glong items_read, items_written;
    GError *error;
    char *s = g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
    FILE *f = fopen(s, "wb");
    if (f == NULL) {
      exit(1);
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_OUTPUT_BINARY;
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("open-binary-output-file", args);
  }
  exit(1);
}
Object scm_open_input_file(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "open-input-file");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING: {
    size_t len = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = string_cdrref(o)) {
      len++;
    }
    gunichar str[len];
    Object o = obj;
    for (size_t i = 0; i < len; i++) {
      str[i] = string_carref(o).character;
      o = string_cdrref(o);
    }
    glong items_read, items_written;
    GError *error;
    char *s = g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
    FILE *f = fopen(s, "r");
    if (f == NULL) {
      exit(1);
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_INPUT_TEXT;
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("open-input-file", args);
  }
  exit(1);
}
Object scm_open_binary_input_file(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "open-binary-input-file");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING: {
    size_t len = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = string_cdrref(o)) {
      len++;
    }
    gunichar str[len];
    Object o = obj;
    for (size_t i = 0; i < len; i++) {
      str[i] = string_carref(o).character;
      o = string_cdrref(o);
    }
    glong items_read, items_written;
    GError *error;
    char *s = g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
    FILE *f = fopen(s, "rb");
    if (f == NULL) {
      exit(1);
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_INPUT_BINARY;
    return out;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("open-binary-input-file", args);
  }
  exit(1);
}
Object scm_close_port(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "close-port");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY: {
    fclose(port_carref(obj).port);
    cars[obj.index].port = NULL;
    return unspecified;
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("close-port", args);
  }
  exit(1);
}
extern FILE *yyin;
extern void yyrestart(FILE *);
extern Object kread();
int interactive_mode = 1;
Object scm_read(Object const args) {
  switch (args_length(args)) {
  case 0: {
    interactive_mode = 0;
    Object out = kread();
    interactive_mode = 1;
    return out;
  }
  case 1: {
    Object obj = value(carref(args));
    if (obj.type != PORT_INPUT_TEXT) {
      return wrong_type("read", args);
    }
    if (obj.port == NULL) {
      exit(1);
    }
    FILE *stream = yyin;
    yyrestart(port_carref(obj).port);
    interactive_mode = 0;
    Object out = kread();
    interactive_mode = 1;
    yyrestart(stream);
    return out;
  }
  default:
    return arguments(args, "read");
  }
  exit(1);
}
Object scm_read_char(Object const args) {
  switch (args_length(args)) {
  case 0: {
    char s[6];
    for (size_t i = 0; i < 6; i++) {
      char ch = fgetc(yyin);
      if (ch == EOF) {
        return (Object){.type = EOF_OBJ};
      }
      s[i] = ch;
      gunichar c = g_utf8_get_char_validated(s, 6);
      if (c != -1) {
        return (Object){.type = CHARACTER, .character = c};
      }
    }
    exit(1);
  }
  case 1: {
    Object obj = value(carref(args));
    if (obj.type != PORT_INPUT_TEXT) {
      return wrong_type("read-char", args);
    }
    FILE *stream = port_carref(obj).port;
    char s[6];
    for (size_t i = 0; i < 6; i++) {
      char ch = fgetc(stream);
      if (ch == EOF) {
        return (Object){.type = EOF_OBJ};
      }
      s[i] = ch;
      gunichar c = g_utf8_get_char_validated(s, 6);
      if (c != -1) {
        return (Object){.type = CHARACTER, .character = c};
      }
    }
    exit(1);
  }
  default:
    return arguments(args, "read-char");
  }
  exit(1);
}
Object scm_eof_object_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "eof-object?");
  }
  return value(carref(args)).type == EOF_OBJ ? true_obj : false_obj;
}
Object scm_eof_object(Object const args) {
  if (args_length(args) != 0) {
    return arguments(args, "eof-object");
  }
  return (Object){.type = EOF_OBJ};
}
extern FILE *yyout;
Object scm_write(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_write(yyout, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj = value(carref(args));
    if (obj.type != PORT_OUTPUT_TEXT) {
      exit(1);
    }
    object_write(port_carref(obj).port, value(carref(cdrref(args))));
    return unspecified;
  }
  default:
    return arguments(args, "write");
  }
  exit(1);
}
Object scm_write_shared(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_write_shared(yyout, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj = value(carref(args));
    if (obj.type != PORT_OUTPUT_TEXT) {
      return wrong_type("write-shared", args);
    }
    object_write_shared(port_carref(obj).port, value(carref(cdrref(args))));
    return unspecified;
  }
  default:
    return arguments(args, "write-shared");
  }
  exit(1);
}
Object scm_write_simple(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_write_simple(yyout, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj = value(carref(args));
    if (obj.type != PORT_OUTPUT_TEXT) {
      return wrong_type("write-simple", args);
    }
    object_write_simple(port_carref(obj).port, value(carref(cdrref(args))));
    return unspecified;
  }
  default:
    return arguments(args, "write-simple");
  }
  exit(1);
}
Object scm_display(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_display(yyout, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj = value(carref(args));
    if (obj.type != PORT_OUTPUT_TEXT) {
      return wrong_type("display", args);
    }
    object_display(port_carref(obj).port, carref(cdrref(args)));
    return unspecified;
  }
  default:
    return arguments(args, "display");
  }
  exit(1);
}
/* Input and output end */
/* System interface */
Object scm_file_exists_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "file-exists?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING: {
    size_t len = 0;
    for (Object o = obj; o.type != STRING_EMPTY; o = string_cdrref(o)) {
      len++;
    }
    gunichar str[len];
    Object o = obj;
    for (size_t i = 0; i < len; i++) {
      str[i] = string_carref(o).character;
      o = string_cdrref(o);
    }
    glong items_read, items_written;
    GError *error;
    char *filename =
        g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
    gboolean b = g_file_test(filename, G_FILE_TEST_EXISTS);
    g_free(filename);
    return b ? true_obj : false_obj;
  }
  default:
    return wrong_type("file-exists?", args);
  }
  exit(1);
}
#include <errno.h>
#include <string.h>
Object scm_primitive_delete_file(Object const args) {
  Object obj = value(carref(args));
  size_t len = 0;
  for (Object o = obj; o.type != STRING_EMPTY; o = string_cdrref(o)) {
    len++;
  }
  gunichar str[len];
  Object o = obj;
  for (size_t i = 0; i < len; i++) {
    str[i] = string_carref(o).character;
    o = string_cdrref(o);
  }
  glong items_read, items_written;
  GError *error;
  char *filename =
      g_ucs4_to_utf8(str, len, &items_read, &items_written, &error);
  int n = remove(filename);
  g_free(filename);
  return n == 0 ? unspecified : (Object){.type = FILE_ERROR,
                                         .message = strdup(strerror(errno))};
}
#include <unistd.h>
extern FILE *yyout;
Object scm_emergency_exit(Object const args) {
  if (args_length(args) == 0) {
    _exit(0);
  }
  if (args_length(args) == 1) {
    if (value(carref(args)).type == TRUE_TYPE) {
      _exit(0);
    }
    fprintf(yyout, "emergency-exit value ");
    object_write(yyout, carref(args));
    fprintf(yyout, "\n");
    _exit(1);
  }
  return arguments(args, "emergency-exit");
}
