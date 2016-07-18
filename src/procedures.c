/** \file  */
#include "procedures.h"

static void error(char const *msg) {
  fprintf(stderr, "kscheme error: %s\n", msg);
  exit(1);
}
extern FILE *yyout;
Object arguments(Object obj, char const *s) {
  fprintf(yyout, "Error: (%s) wrong number of arguments -- ", s);
  object_write(carref(cur_err).port, obj);
  fprintf(carref(cur_err).port, "\n");
  return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
}
Object wrong_type(char const *prog_name, Object obj) {
  fprintf(carref(cur_err).port, "Error: (%s) wrong type argument -- ",
          prog_name);
  object_write(carref(cur_err).port, obj);
  fprintf(carref(cur_err).port, "\n");
  return (Object){.type = WRONG_TYPE_ARGUMENT};
}
static size_t args_length(Object const args) {
  size_t len = 0;
  for (Object o = args; o.type != EMPTY; o = cdrref(o)) {
    len += 1;
  }
  return len;
}

/* Derived expression types */
Object scm_promise_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "promise?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PROMISE:
    return true_obj;
  case NONE:
    error("scm_promise_p");
  default:
    return false_obj;
  }
}
/* Derived expression types end */

Object scm_eqv_p(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "eqv?");
  }
  Object obj1 = value(carref(args));
  Object obj2 = value(carref(cdrref(args)));
  switch (obj1.type) {
  case TRUE_TYPE:
  case FALSE_TYPE:
  case PRIMITIVE_PROCEDURE_FORCE:
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
  case DELAY:
  case DELAY_FORCE:
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
    case NONE:
      error("scm_eav_p");
    default:
      return false_obj;
    }
  case NUMBERQ: {
    switch (obj2.type) {
    case NUMBERZ:
      return mpq_cmp_z(obj1.numberq, obj2.numberz) == 0 ? true_obj : false_obj;
    case NUMBERQ:
      return mpq_cmp(obj1.numberq, obj2.numberq) == 0 ? true_obj : false_obj;
    case NONE:
      error("scm_eqv_p");
    default:
      return false_obj;
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
    case NONE:
      error("scm_eqv_p");
    default:
      return false_obj;
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
    case NONE:
      error("scm_eqv_p");
    default:
      return false_obj;
    }
  }
  case CHARACTER:
    return obj2.type == CHARACTER && obj1.character == obj2.character
               ? true_obj
               : false_obj;
  case STRING:
    return obj1.type == obj2.type && obj1.index && obj2.index ? true_obj
                                                              : false_obj;
  case STRING_IMMUTABLE:
    return obj1.type == obj2.type &&
                   obj1.string_immutable == obj2.string_immutable
               ? true_obj
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
  case PORT_INPUT_TEXT_STRING:
  case PORT_OUTPUT_TEXT_STRING:
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
  case PROMISE:
    return obj1.type == obj2.type && obj1.index == obj2.index ? true_obj
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
  case PRIMITIVE_PROCEDURE_FORCE:
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
  case DELAY:
  case DELAY_FORCE:
  case EMPTY:
  case MULTIPLE_ZERO:
    return obj1.type == obj2.type ? true_obj : false_obj;
  case IDENTIFIER:
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case STRING_IMMUTABLE:
  case VECTOR:
  case BYTEVECTOR:
  case PROMISE:
  case PRIMITIVE_PROCEDURE:
  case PROCEDURE:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_INPUT_TEXT_STRING:
  case PORT_OUTPUT_TEXT_STRING:
  case PAIR:
    return scm_eqv_p(args);
  case EOF_OBJ:
  case FILE_ERROR:
  case UNSPECIFIED:
    return false_obj;
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
  case NONE:
    error("scm_number_p");
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
    mpfr_get_f(opf, obj.numberr, MPFR_RNDN);
    mpq_set_f(opq, opf);
    Object out = mpfr_cmp_q(obj.numberr, opq) == 0 ? true_obj : false_obj;
    return out;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return false_obj;
    }
    mpfr_get_f(opf, mpc_realref(obj.numberc), MPFR_RNDN);
    mpq_set_f(opq, opf);
    Object out = mpfr_cmp_q(obj.numberr, opq) == 0 ? true_obj : false_obj;
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
    mpfr_round(opfr, obj.numberr);
    int b = mpfr_equal_p(obj.numberr, opfr);
    return b ? true_obj : false_obj;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return false_obj;
    }
    mpfr_round(opfr, obj.numberr);
    int b = mpfr_equal_p(mpc_realref(obj.numberc), opfr);
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
  mpz_set_ui(opz, 0);
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
        return wrong_type("+", args);
      }
      break;
    }
    default:
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
  return out;
}

Object scm_mul(Object const args) {
  mpz_set_ui(opz, 1);
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
        return wrong_type("*", args);
      }
      break;
    }
    default:
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
  return out;
}

Object scm_sub(Object const args) {
  if (args_length(args) == 0) {
    return arguments(args, "-");
  }
  Object out = none;
  size_t len = args_length(args);
  Object obj1 = value(carref(args));
  Type type = obj1.type;
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
        return wrong_type("-", args);
      }
      break;
    }
    default:
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
  return out;
}
Object scm_div(Object const args) {
  if (args_length(args) == 0) {
    return arguments(args, "/");
  }
  Object out = none;
  size_t len = args_length(args);
  Object obj1 = value(carref(args));
  Type type = obj1.type;
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

        return wrong_type("/", args);
      }
      break;
    }
    default:

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
Object scm_gcd(Object const args) {
  mpz_set_ui(opz, 0);
  bool is_exact = true;
  for (Object obj = args; obj.type != EMPTY; obj = cdrref(obj)) {
    Object o = value(carref(obj));
    switch (o.type) {
    case NUMBERZ: {
      mpz_gcd(opz, opz, o.numberz);
      break;
    }
    case NUMBERQ: {
      mpq_canonicalize(o.numberq);
      if (mpz_cmp_ui(mpq_denref(o.numberq), 0) == 0) {
        mpz_gcd(opz, opz, mpq_numref(o.numberq));
        break;
      }
      return wrong_type("gcd", args);
    }
    case NUMBERR: {
      mpfr_round(opfr, o.numberr);
      if (mpfr_equal_p(opfr, o.numberr)) {
        mpfr_get_z(opz1, o.numberr, MPFR_RNDN);
        mpz_gcd(opz, opz, opz1);
        is_exact = false;
        break;
      }
      return wrong_type("gcd", args);
    }
    case NUMBERC: {
      if (mpfr_zero_p(mpc_imagref(o.numberc))) {
        mpfr_set(opfr, mpc_realref(o.numberc), MPFR_RNDN);
        mpfr_round(opfr1, opfr);
        if (mpfr_equal_p(opfr1, opfr)) {
          mpfr_get_z(opz1, opfr, MPFR_RNDN);
          mpz_gcd(opz, opz, opz1);
          is_exact = false;
          break;
        }
      }
      return wrong_type("gcd", args);
    }
    case NONE:
      error("scm_gcd");
    default:
      return wrong_type("gcd", args);
    }
  }
  if (is_exact) {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, opz);
    return out;
  }
  Object out = {.type = NUMBERR};
  mpfr_init_set_z(out.numberr, opz, MPFR_RNDN);
  return out;
}
Object scm_lcm(Object const args) {
  mpz_set_ui(opz, 1);
  bool is_exact = true;
  for (Object obj = args; obj.type != EMPTY; obj = cdrref(obj)) {
    Object o = value(carref(obj));
    switch (o.type) {
    case NUMBERZ: {
      mpz_lcm(opz, opz, o.numberz);
      break;
    }
    case NUMBERQ: {
      mpq_canonicalize(o.numberq);
      if (mpz_cmp_ui(mpq_denref(o.numberq), 0) == 0) {
        mpz_lcm(opz, opz, mpq_numref(o.numberq));
        break;
      }
      return wrong_type("lcm", args);
    }
    case NUMBERR: {
      mpfr_round(opfr, o.numberr);
      if (mpfr_equal_p(opfr, o.numberr)) {
        mpfr_get_z(opz1, o.numberr, MPFR_RNDN);
        mpz_lcm(opz, opz, opz1);
        is_exact = false;
        break;
      }
      return wrong_type("lcm", args);
    }
    case NUMBERC: {
      if (mpfr_zero_p(mpc_imagref(o.numberc))) {
        mpfr_set(opfr, mpc_realref(o.numberc), MPFR_RNDN);
        mpfr_round(opfr1, opfr);
        if (mpfr_equal_p(opfr1, opfr)) {
          mpfr_get_z(opz1, opfr, MPFR_RNDN);
          mpz_lcm(opz, opz, opz1);
          is_exact = false;
          break;
        }
      }
      return wrong_type("lcm", args);
    }
    case NONE:
      error("scm_lcm");
    default:
      return wrong_type("lcm", args);
    }
  }
  if (is_exact) {
    Object out = {.type = NUMBERZ};
    mpz_init_set(out.numberz, opz);
    return out;
  }
  Object out = {.type = NUMBERR};
  mpfr_init_set_z(out.numberr, opz, MPFR_RNDN);
  return out;
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
    mpfr_get_f(opf, obj.numberr, MPFR_RNDN);
    mpq_set_f(opq, opf);
    Object out = {.type = NUMBERR};
    mpfr_set_z(out.numberr, mpq_numref(opq), MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    if (scm_rational_p(args).type == FALSE_TYPE) {
      return wrong_type("numerator", args);
    }
    mpfr_get_f(opf, mpc_realref(obj.numberc), MPFR_RNDN);
    mpq_set_f(opq, opf);
    Object out = {.type = NUMBERR};
    mpfr_set_z(out.numberr, mpq_numref(opq), MPFR_RNDN);
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
    mpfr_get_f(opf, obj.numberr, MPFR_RNDN);
    mpq_set_f(opq, opf);
    Object out = {.type = NUMBERR};
    mpfr_init_set_z(out.numberr, mpq_denref(opq), MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    if (scm_rational_p(args).type == FALSE_TYPE) {
      return wrong_type("denominator", args);
    }
    mpfr_get_f(opf, mpc_realref(obj.numberc), MPFR_RNDN);
    mpq_set_f(opq, opf);
    Object out = {.type = NUMBERR};
    mpfr_set_z(out.numberr, mpq_denref(opq), MPFR_RNDN);
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
Object scm_round(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "round");
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
    mpfr_set_q(opfr, obj.numberq, MPFR_RNDN);
    mpfr_get_z(out.numberz, opfr, MPFR_RNDN);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_get_z(opz, obj.numberr, MPFR_RNDN);
    mpfr_set_z(out.numberr, opz, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    if (!mpfr_zero_p(mpc_imagref(obj.numberc))) {
      return wrong_type("round", args);
    }
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_get_z(opz, mpc_realref(obj.numberc), MPFR_RNDN);
    mpfr_set_z(out.numberr, opz, MPFR_RNDN);
    return out;
  }
  default:
    return wrong_type("round", args);
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
    mpfr_set_z(opfr, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_exp(out.numberr, opfr, MPFR_RNDN);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("1", 10);
    }
    mpfr_set_q(opfr, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_exp(out.numberr, opfr, MPFR_RNDN);
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
      mpc_set_z(opc, obj.numberz, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, opc, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      if (mpq_cmp_ui(obj.numberq, 1, 1) == 0) {
        return numberz_new("0", 10);
      }
      mpc_set_q(opc, obj.numberq, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, opc, MPC_RNDNN);
      return out;
    }
    case NUMBERR: {
      mpc_set_fr(opc, obj.numberr, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_log(out.numberc, opc, MPC_RNDNN);
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
        mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
        mpc_set_z(opc1, obj2.numberz, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERQ: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
        mpc_set_q(opc1, obj2.numberq, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERR: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
        mpc_set_fr(opc1, obj2.numberr, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERC: {
        if (mpz_cmp_ui(obj1.numberz, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
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
        mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
        mpc_set_z(opc1, obj2.numberz, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERQ: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
        mpc_set_q(opc1, obj2.numberq, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERR: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
        mpc_set_fr(opc1, obj2.numberr, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERC: {
        if (mpq_cmp_ui(obj1.numberq, 1, 1) == 0) {
          return numberz_new("0", 10);
        }
        mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
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
        mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
        mpc_set_z(opc1, obj2.numberz, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERQ: {
        mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
        mpc_set_q(opc1, obj2.numberq, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERR: {
        mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
        mpc_set_fr(opc1, obj2.numberr, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERC: {
        mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
        mpc_log(opc2, opc, MPC_RNDNN);
        mpc_log(opc3, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
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
        mpc_set_z(opc1, obj2.numberz, MPC_RNDNN);
        mpc_log(opc2, obj1.numberc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERQ: {
        mpc_set_q(opc1, obj2.numberq, MPC_RNDNN);
        mpc_log(opc2, obj1.numberc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERR: {
        mpc_set_fr(opc1, obj2.numberr, MPC_RNDNN);
        mpc_log(opc2, obj1.numberc, MPC_RNDNN);
        mpc_log(opc3, opc1, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
        return out;
      }
      case NUMBERC: {
        mpc_log(opc2, obj1.numberc, MPC_RNDNN);
        mpc_log(opc3, obj2.numberc, MPC_RNDNN);
        Object out = {.type = NUMBERC};
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, opc2, opc3, MPC_RNDNN);
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
    mpfr_set_z(opfr, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sin(out.numberr, opfr, MPFR_RNDN);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("0", 0);
    }
    mpfr_set_q(opfr, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sin(out.numberr, opfr, MPFR_RNDN);
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
    mpfr_set_z(opfr, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_cos(out.numberr, opfr, MPFR_RNDN);
    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("1", 0);
    }
    mpfr_set_q(opfr, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_cos(out.numberr, opfr, MPFR_RNDN);
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
    mpfr_set_z(opfr, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_tan(out.numberr, opfr, MPFR_RNDN);
    return out;
  }
  case NUMBERQ: {
    mpfr_set_q(opfr, obj.numberq, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_tan(out.numberr, opfr, MPFR_RNDN);
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
    mpc_set_z(opc, obj.numberz, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, opc, MPC_RNDNN);

    return out;
  }
  case NUMBERQ: {
    if (mpq_sgn(obj.numberq) == 0) {
      return numberz_new("0", 0);
    }
    mpc_set_q(opc, obj.numberq, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, opc, MPC_RNDNN);

    return out;
  }
  case NUMBERR: {
    mpc_set_fr(opc, obj.numberr, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, opc, MPC_RNDNN);
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
    mpc_set_z(opc, obj.numberz, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_acos(out.numberc, opc, MPC_RNDNN);

    return out;
  }
  case NUMBERQ: {
    if (mpq_cmp_ui(obj.numberq, 1, 1) == 0) {
      return numberz_new("0", 0);
    }
    mpc_set_q(opc, obj.numberq, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_acos(out.numberc, opc, MPC_RNDNN);

    return out;
  }
  case NUMBERR: {
    mpc_set_fr(opc, obj.numberr, MPC_RNDNN);
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_asin(out.numberc, opc, MPC_RNDNN);
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
      mpfr_init_set_z(opfr, obj.numberz, MPFR_RNDN);
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_atan(out.numberr, opfr, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj.numberq) == 0) {
        return numberz_new("0", 10);
      }
      mpfr_set_q(opfr, obj.numberq, MPFR_RNDN);
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_atan(out.numberr, opfr, MPC_RNDNN);
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

        mpc_set_z_z(opc, obj2.numberz, obj1.numberz, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);

        return out;
      }
      case NUMBERQ: {
        if (sign1 == 0 && mpq_sgn(obj2.numberq) >= 0) {
          return numberz_new("0", 10);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpq_set_z(opq, obj1.numberz);
        mpc_set_q_q(opc, obj2.numberq, opq, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpfr_set_z(opfr, obj1.numberz, MPFR_RNDN);
        mpc_set_fr_fr(opc, obj2.numberr, opfr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpfr_set_z(opfr, obj1.numberz, MPFR_RNDN);
        mpc_set_fr_fr(opc, mpc_realref(obj2.numberc), opfr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
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
        mpq_set_z(opq, obj2.numberz);
        mpc_set_q_q(opc, opq, obj1.numberq, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERQ: {
        if (sign1 == 0 && mpq_sgn(obj2.numberq) >= 0) {
          return numberz_new("0", 10);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_set_q_q(opc, obj2.numberq, obj1.numberq, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpfr_set_q(opfr, obj1.numberq, MPFR_RNDN);
        mpc_set_fr_fr(opc, obj2.numberr, opfr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpfr_set_q(opfr, obj1.numberq, MPFR_RNDN);
        mpc_set_fr_fr(opc, mpc_realref(obj2.numberc), opfr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
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
        mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
        mpc_set_fr_fr(opc, opfr, obj1.numberr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERQ: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
        mpc_set_fr_fr(opc, opfr, obj1.numberr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_set_fr_fr(opc, obj2.numberr, obj1.numberr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_set_fr_fr(opc, mpc_realref(obj2.numberc), obj1.numberr, MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
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
        mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
        mpc_set_fr_fr(opc, opfr, mpc_realref(obj1.numberc), MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERQ: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
        mpc_set_fr_fr(opc, opfr, mpc_realref(obj1.numberc), MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERR: {
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_set_fr_fr(opc, obj2.numberr, mpc_realref(obj1.numberc), MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
        return out;
      }
      case NUMBERC: {
        if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
          return wrong_type("atan", args);
        }
        Object out = {.type = NUMBERR};
        mpfr_init(out.numberr);
        mpc_set_fr_fr(opc, mpc_realref(obj2.numberc), mpc_realref(obj1.numberc),
                      MPC_RNDNN);
        mpc_arg(out.numberr, opc, MPFR_RNDN);
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
    mpfr_set_z(opfr, obj.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sqrt(out.numberr, opfr, MPFR_RNDN);
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
      mpfr_set_z(opfr, obj1.numberz, MPFR_RNDN);
      Object out;
      if (sign2 > 0) {
        out.type = NUMBERZ;
        mpfr_pow_z(opfr1, opfr, obj2.numberz, MPFR_RNDN);
        mpz_init(out.numberz);
        mpfr_get_z(out.numberz, opfr1, MPFR_RNDN);
      } else {
        out.type = NUMBERQ;
        mpz_neg(opz, obj2.numberz);
        mpfr_pow_z(opfr, opfr, opz, MPFR_RNDN);
        mpfr_get_z(opz1, opfr, MPFR_RNDN);
        mpq_init(out.numberq);
        mpz_set_ui(mpq_numref(out.numberq), 1);
        mpz_set(mpq_denref(out.numberq), opz1);
      }
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpc_pow_fr(out.numberc, opc, opfr, MPFR_RNDN);
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
      Object out = {.type = NUMBERC};
      mpc_init2(opc, MPC_PREC);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
      mpc_pow_fr(out.numberc, opc, obj2.numberr, MPFR_RNDN);
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
      mpc_set_z(opc, obj1.numberz, MPC_RNDNN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow(out.numberc, opc, obj2.numberc, MPC_RNDNN);
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
      mpfr_set_z(opfr, mpq_numref(obj1.numberq), MPFR_RNDN);
      mpfr_set_z(opfr1, mpq_denref(obj1.numberq), MPFR_RNDN);
      if (sign2 > 0) {
        mpfr_pow_z(opfr2, opfr, obj2.numberz, MPFR_RNDN);
        mpfr_pow_z(opfr3, opfr1, obj2.numberz, MPFR_RNDN);
        mpfr_get_z(mpq_numref(out.numberq), opfr2, MPFR_RNDN);
        mpfr_get_z(mpq_denref(out.numberq), opfr3, MPFR_RNDN);
      } else {
        mpz_abs(opz, obj2.numberz);
        mpfr_pow_z(opfr2, opfr, opz, MPFR_RNDN);
        mpfr_pow_z(opfr3, opfr1, opz, MPFR_RNDN);
        mpfr_get_z(mpq_numref(out.numberq), opfr3, MPFR_RNDN);
        mpfr_get_z(mpq_denref(out.numberq), opfr2, MPFR_RNDN);
      }
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpc_pow_fr(out.numberc, opc, opfr, MPC_PREC);
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
      mpc_pow_fr(out.numberc, opc, obj2.numberr, MPFR_RNDN);
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_q(opc, obj1.numberq, MPC_RNDNN);
      mpc_pow(out.numberc, opc, obj2.numberc, MPFR_RNDN);
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpc_pow_fr(out.numberc, opc, opfr, MPC_RNDNN);
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
      mpc_pow_fr(out.numberc, opc, obj2.numberr, MPC_RNDNN);
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
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr(opc, obj1.numberr, MPC_RNDNN);
      mpc_pow(out.numberc, opc, obj2.numberc, MPC_RNDNN);
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
      mpfr_init_set_q(opfr, obj2.numberq, MPFR_RNDN);
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_pow_fr(out.numberc, obj1.numberc, opfr, MPC_RNDNN);
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
      mpq_set_z(opq, obj1.numberz);
      mpc_set_q_q(out.numberc, opq, obj2.numberq, MPC_RNDNN);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_set_z(opfr, obj1.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, opfr, obj2.numberr, MPC_RNDNN);
      return out;
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return wrong_type("make-rectangular", args);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_set_z(opfr, obj1.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, opfr, mpc_realref(obj2.numberc), MPC_RNDNN);
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
      mpq_set_z(opq, obj2.numberz);
      mpc_set_q_q(out.numberc, obj1.numberq, opq, MPC_RNDNN);
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
      mpfr_set_q(opfr, obj1.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, opfr, obj2.numberr, MPC_RNDNN);
      return out;
    }
    case NUMBERC: {
      if (!mpfr_zero_p(mpc_imagref(obj2.numberc))) {
        return wrong_type("make-rectangular", args);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_set_q(opfr, obj1.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, opfr, mpc_realref(obj2.numberc), MPC_RNDNN);
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
      mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, obj1.numberr, opfr, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj2.numberq) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, obj1.numberr, opfr, MPC_RNDNN);
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
      mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, mpc_realref(obj1.numberc), opfr, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      if (mpq_sgn(obj2.numberq) == 0) {
        return object_copy(obj1);
      }
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpc_set_fr_fr(out.numberc, mpc_realref(obj1.numberc), opfr, MPC_RNDNN);
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
      mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
      mpfr_sin_cos(opfr1, opfr2, opfr, MPFR_RNDN);
      mpfr_mul_z(opfr3, opfr2, obj1.numberz, MPFR_RNDN);
      mpfr_mul_z(opfr4, opfr1, obj1.numberz, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr3, opfr4, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERC};
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpfr_sin_cos(opfr1, opfr2, opfr, MPFR_RNDN);
      mpfr_mul_z(opfr3, opfr2, obj1.numberz, MPFR_RNDN);
      mpfr_mul_z(opfr4, opfr1, obj1.numberz, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr3, opfr4, MPC_RNDNN);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpfr_sin_cos(opfr, opfr1, obj2.numberr, MPFR_RNDN);
      mpfr_mul_z(opfr2, opfr1, obj1.numberz, MPFR_RNDN);
      mpfr_mul_z(opfr3, opfr, obj1.numberz, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr2, opfr3, MPC_RNDNN);
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
      mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
      mpfr_sin_cos(opfr1, opfr2, opfr, MPFR_RNDN);
      mpfr_mul_q(opfr3, opfr2, obj1.numberq, MPFR_RNDN);
      mpfr_mul_q(opfr4, opfr1, obj1.numberq, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr3, opfr4, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERC};
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpfr_sin_cos(opfr1, opfr2, opfr, MPFR_RNDN);
      mpfr_mul_q(opfr3, opfr2, obj1.numberq, MPFR_RNDN);
      mpfr_mul_q(opfr4, opfr1, obj1.numberq, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr3, opfr4, MPC_RNDNN);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpfr_sin_cos(opfr, opfr1, obj2.numberr, MPFR_RNDN);
      mpfr_mul_q(opfr2, opfr1, obj1.numberq, MPFR_RNDN);
      mpfr_mul_q(opfr3, opfr, obj1.numberq, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr2, opfr3, MPC_RNDNN);
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
      mpfr_set_z(opfr, obj2.numberz, MPFR_RNDN);
      mpfr_sin_cos(opfr1, opfr2, opfr, MPFR_RNDN);
      mpfr_mul(opfr3, opfr2, obj1.numberr, MPFR_RNDN);
      mpfr_mul(opfr4, opfr1, obj1.numberr, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr3, opfr4, MPC_RNDNN);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERC};
      mpfr_set_q(opfr, obj2.numberq, MPFR_RNDN);
      mpfr_sin_cos(opfr1, opfr2, opfr, MPFR_RNDN);
      mpfr_mul(opfr3, opfr2, obj1.numberr, MPFR_RNDN);
      mpfr_mul(opfr4, opfr1, obj1.numberr, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr3, opfr4, MPC_RNDNN);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERC};
      mpfr_sin_cos(opfr, opfr1, obj2.numberr, MPFR_RNDN);
      mpfr_mul(opfr2, opfr1, obj1.numberr, MPFR_RNDN);
      mpfr_mul(opfr3, opfr, obj1.numberr, MPFR_RNDN);
      mpc_init2(out.numberc, MPC_PREC);
      mpc_set_fr_fr(out.numberc, opfr2, opfr3, MPC_RNDNN);
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
    mpfr_get_f(opf, obj.numberr, MPFR_RNDN);
    mpq_set_f(out.numberq, opf);
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

#include <string.h> // strlen
Object scm_number_tostring(Object const args) {
  size_t len = args_length(args);
  int base;
  Object obj1;
  Object obj2;
  if (len == 1) {
    obj1 = value(carref(args));
    base = 10;
  } else if (len == 2) {
    obj2 = value(carref(cdrref(args)));
    if (obj2.type != NUMBERZ) {
      return wrong_type("number->string", args);
    }
    if (!(mpz_cmp_ui(obj2.numberz, 2) >= 0 &&
          mpz_cmp_ui(obj2.numberz, 62) <= 0)) {
      return wrong_type("number->string", args);
    }
    base = mpz_get_ui(obj2.numberz);
    obj1 = value(carref(args));
  } else {
    return arguments(args, "number->string");
  }
  Object out = {.type = STRING_EMPTY};
  switch (obj1.type) {
  case NUMBERZ: {
    char *str = NULL;
    str = mpz_get_str(str, base, obj1.numberz);
    for (size_t i = strlen(str) - 1;; i--) {
      out = string_cons((Object){.type = CHARACTER, .character = str[i]}, out);
      if (i == 0) {
        break;
      }
    }
    free(str);
    return out;
  }
  case NUMBERQ: {
    char *str = NULL;
    str = mpq_get_str(str, base, obj1.numberq);
    for (size_t i = strlen(str) - 1;; i--) {
      out = string_cons((Object){.type = CHARACTER, .character = str[i]}, out);
      if (i == 0) {
        break;
      }
    }
    free(str);
    return out;
  }
  case NUMBERR: {
    char *str = NULL;
    mpfr_exp_t exp = 0;
    str = mpfr_get_str(str, &exp, base, 0, obj1.numberr, MPFR_RNDN);
    size_t sign = mpfr_sgn(obj1.numberr) < 0 ? 1 : 0;
    for (size_t i = strlen(str) - 1;; i--) {
      if (i == exp - 1 + sign) {
        out = string_cons((Object){.type = CHARACTER, .character = '.'}, out);
      }
      out = string_cons((Object){.type = CHARACTER, .character = str[i]}, out);
      if (i == 0) {
        break;
      }
    }
    mpfr_free_str(str);
    return out;
  }
  case NUMBERC: {
    char *str_real = NULL;
    char *str_imag = NULL;
    mpfr_exp_t exp_real = 0;
    mpfr_exp_t exp_imag = 0;
    size_t sign_real = mpfr_sgn(mpc_realref(obj1.numberc)) < 0 ? 1 : 0;
    size_t sign_imag = mpfr_sgn(mpc_imagref(obj1.numberc)) < 0 ? 1 : 0;
    str_real = mpfr_get_str(str_real, &exp_real, base, 0,
                            mpc_realref(obj1.numberc), MPFR_RNDN);
    str_imag = mpfr_get_str(str_imag, &exp_imag, base, 0,
                            mpc_imagref(obj1.numberc), MPFR_RNDN);
    if (!mpfr_zero_p(mpc_imagref(obj1.numberc))) {
      out = string_cons((Object){.type = CHARACTER, .character = 'i'}, out);
      for (size_t i = strlen(str_imag) - 1;; i--) {
        if (i == exp_imag - 1 + sign_imag) {
          out = string_cons((Object){.type = CHARACTER, .character = '.'}, out);
        }
        out = string_cons((Object){.type = CHARACTER, .character = str_imag[i]},
                          out);
        if (i == 0) {
          break;
        }
      }
      if (mpfr_sgn(mpc_imagref(obj1.numberc)) > 0) {
        out = string_cons((Object){.type = CHARACTER, .character = '+'}, out);
      }
    }
    if (!mpfr_zero_p(mpc_realref(obj1.numberc))) {
      for (size_t i = strlen(str_real) - 1;; i--) {
        if (i == exp_real - 1 + sign_real) {
          out = string_cons((Object){.type = CHARACTER, .character = '.'}, out);
        }
        out = string_cons((Object){.type = CHARACTER, .character = str_real[i]},
                          out);
        if (i == 0) {
          break;
        }
      }
    }
    mpfr_free_str(str_real);
    mpfr_free_str(str_imag);
    return out;
  }
  case NONE:
    error("scm_number_tostring");
  default:
    return wrong_type("number->string", args);
  }
}

extern void yyrestart(FILE *);
extern Object kread();

extern int stringto(char *str);
Object scm_string_tonumber(Object const args) {
  size_t len = args_length(args);
  if (!(len == 1 || len == 2)) {
    return arguments(args, "string->number");
  }
  Object obj1 = value(carref(args));
  int base = 10;
  if (len == 2) {
    Object obj2 = value(carref(cdrref(args)));
    if (obj2.type != NUMBERZ) {
      return wrong_type("string->numberz", args);
    }
    if (!(mpz_cmp_ui(obj2.numberz, 2) >= 0 &&
          mpz_cmp_ui(obj2.numberz, 62) <= 0)) {
      return wrong_type("string->numberz", args);
    }
    base = mpz_get_ui(obj2.numberz);
  }
  Object out;
  /* char *s = NULL; */
  char ch0;
  char ch1;
  char ch2;
  char ch3;
  int n;
  switch (obj1.type) {
  case STRING_EMPTY:
  case STRING: {
    Object o = obj1;
    size_t uni_len = 0;
    gint chs_len = 0;
    for (; o.type != STRING_EMPTY; uni_len++) {
      chs_len += g_unichar_to_utf8(string_carref(o).character, NULL);
      o = string_cdrref(o);
    }
    char str[chs_len + 1];
    o = obj1;
    size_t uni_i = 0;
    gint str_i = 0;
    for (; str_i < chs_len; uni_i++) {
      gchar outbuf[6];
      gint len = g_unichar_to_utf8(string_carref(o).character, outbuf);
      for (gint i = 0; i < len; i++) {
        str[str_i] = outbuf[i];
        str_i++;
      }
      o = string_cdrref(o);
    }
    str[str_i] = '\0';
    FILE *stream = carref(cur_in).port;
    n = stringto(str);
    yyrestart(stream);
    out = kread_obj;
    ch0 = str[0];
    ch1 = str[1];
    ch2 = str[2];
    ch3 = str[3];
    break;
  }
  case STRING_IMMUTABLE: {
    FILE *stream = carref(cur_in).port;
    n = stringto(obj1.string_immutable);
    yyrestart(stream);
    out = kread_obj;
    ch0 = obj1.string_immutable[0];
    ch1 = obj1.string_immutable[1];
    ch2 = obj1.string_immutable[2];
    ch3 = obj1.string_immutable[3];
    break;
  }
  case NONE:
    error("scm_string_tonumber");
  default:
    return wrong_type("string->number", args);
  }
  if (n == 1) {
    object_free(&out);
    return false_obj;
  }
  if (ch0 == '#') {
    if (ch1 == 'b' || ch1 == 'o' || ch1 == 'd' || ch1 == 'x') {
      return out;
    }
    if (ch1 != '\0' && ch2 == '#') {
      if (ch3 == 'b' || ch3 == 'o' || ch3 == 'd' || ch3 == 'x') {
        return out;
      }
    }
  }
  if (base != 10) {
    char *str = NULL;
    switch (out.type) {
    case NUMBERZ: {
      str = mpz_get_str(str, 10, out.numberz);
      int n = mpz_set_str(out.numberz, str, base);
      if (n != 0) {
        object_free(&out);
        out = false_obj;
      }
      free(str);
      return out;
    }
    case NUMBERQ: {
      str = mpq_get_str(str, 10, out.numberq);
      int n = mpq_set_str(out.numberq, str, base);
      if (n != 0) {
        object_free(&out);
        out = false_obj;
      }
      free(str);
      return out;
    }
    case NUMBERR: {
      mpfr_exp_t exp = 0;
      str = mpfr_get_str(str, &exp, 10, 0, out.numberr, MPFR_RNDN);
      size_t len = strlen(str) + 1;
      char str1[len + 2];
      if (str[0] == '-') {
        exp++;
      }
      size_t j = 0;
      for (size_t i = 0; i < len; i++) {
        if (exp == i) {
          str1[j] = '.';
          j++;
        }
        str1[j] = str[i];
        j++;
      }
      mpfr_free_str(str);
      str1[j] = '\0';
      int n = mpfr_set_str(out.numberr, str1, base, MPFR_RNDN);
      if (n != 0) {
        object_free(&out);
        out = false_obj;
      }
      return out;
    }
    case NUMBERC: {
      str = mpc_get_str(10, 0, out.numberc, MPC_RNDNN);
      int n = mpc_set_str(out.numberc, str, base, MPC_RNDNN);
      if (n != 0) {
        object_free(&out);
        out = false_obj;
      }
      mpc_free_str(str);
      return out;
    }
    case NONE:
      error("scm_string_tonumber");
    default:
      return false_obj;
    }
  }
  return out;
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
Object scm_null_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "null?");
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
Object scm_list_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "list?");
  }
  Object obj = value(carref(args));
  if (obj.type == NONE) {
    error("scm_list_p");
  }
  return list_p(value(obj)) ? true_obj : false_obj;
}
Object scm_make_list(Object const args) {
  size_t len = args_length(args);
  Object obj1;
  Object obj2;
  if (len == 1) {
    obj1 = value(carref(args));
    obj2 = empty;
  } else if (len == 2) {
    obj1 = value(carref(args));
    obj2 = carref(cdrref(args));
  } else {
    return arguments(args, "make-list");
  }
  if (obj1.type == NUMBERZ) {
    mpz_set(opz, obj1.numberz);
  } else if (obj1.type == NUMBERQ) {
    mpq_canonicalize(obj1.numberq);
    if (mpz_cmp_ui(mpq_denref(obj1.numberq), 1) == 0) {
      mpz_set(opz, mpq_numref(obj1.numberq));
    }
    return arguments(args, "make-list");
  } else {
    return arguments(args, "make-list");
  }
  if (mpz_sgn(opz) < 0) {
    return arguments(args, "make-list");
  }
  Object out = empty;
  for (; mpz_sgn(opz) != 0;) {
    out = cons(object_copy(obj2), out);
    mpz_sub_ui(opz, opz, 1);
  }
  return out;
}
Object scm_list(Object const args) { return args; }
Object scm_length(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "length");
  }
  Object obj = value(carref(args));
  if (obj.type == NONE) {
    error("scm_length");
  }
  if (list_p(obj)) {
    size_t len = 0;
    for (Object o = obj; o.type != EMPTY;) {
      len++;
      o = value(cdrref(o));
    }
    Object out = {.type = NUMBERZ};
    mpz_init_set_ui(out.numberz, len);
    return out;
  }
  return wrong_type("length", args);
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
Object scm_symbol_equal_p(Object const args) {
  if (args_length(args) < 2) {
    return arguments(args, "symbol=?");
  }
  Object obj1 = value(carref(args));
  switch (obj1.type) {
  case IDENTIFIER: {
    for (Object o = cdrref(args); o.type != EMPTY; o = cdrref(o)) {
      Object obj2 = value(carref(o));
      switch (obj2.type) {
      case IDENTIFIER: {
        if (obj1.identifier == obj2.identifier) {
          break;
        }
        return false_obj;
      }
      case NONE:
        error("scm_symbol_equal_p");
      default:
        return wrong_type("symbol=?", args);
      }
    }
    return true_obj;
  }
  case IDENTIFIER_VERTICAL: {
    for (Object o = cdrref(args); o.type != EMPTY; o = cdrref(o)) {
      Object obj2 = value(carref(o));
      switch (obj2.type) {
      case IDENTIFIER_VERTICAL: {
        if (obj1.identifier == obj2.identifier) {
          break;
        }
        return false_obj;
      }
      case NONE:
        error("scm_symbol_equal_p");
      default:
        return wrong_type("symbol=?", args);
      }
    }
    return true_obj;
  }
  case NONE:
    error("scm_symbol_equal_p");
  default:
    return wrong_type("symbol=?", args);
  }
}

Object scm_symbol_tostring(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "char?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case IDENTIFIER: {
    return (Object){.type = STRING_IMMUTABLE,
                    .string_immutable = obj.identifier};
  }
  case IDENTIFIER_VERTICAL: {
    return (Object){.type = STRING_IMMUTABLE_VERTICAL,
                    .string_immutable_vertical = obj.identifier_vertical};
  }
  case NONE:
    error("scm_symbol_tostring");
  default:
    return wrong_type("symbol->string", args);
  }
}
Object scm_string_tosymbol(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "string->symbol");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY: {
    return identifier_vertical_new("");
  }
  case STRING: {
    Object o = obj;
    size_t uni_len = 0;
    gint chs_len = 0;
    for (; o.type != STRING_EMPTY; uni_len++) {
      switch (string_carref(o).character) {
      case '\\':
        chs_len += 2;
        break;
      default:
        chs_len += g_unichar_to_utf8(string_carref(o).character, NULL);
        break;
      }
      o = string_cdrref(o);
    }
    char str[2 * chs_len + 3];
    o = obj;
    gint str_i = 0;
    str[str_i] = '|';
    str_i++;
    for (size_t i = 0; i < chs_len; i++) {
      gchar outbuf[6];
      gint len = g_unichar_to_utf8(string_carref(o).character, outbuf);
      if (outbuf[0] == '|') {
        str[str_i] = '\\';
        str_i++;
        str[str_i] = '|';
        str_i++;
      } else {
        for (gint j = 0; j < len; j++) {
          str[str_i] = outbuf[j];
          str_i++;
        }
      }
      o = string_cdrref(o);
    }
    str[str_i] = '|';
    str[str_i + 1] = '\0';
    FILE *stream = carref(cur_in).port;
    int n = stringto(str);
    yyrestart(stream);
    if (n != 0) {
      return wrong_type("string->symbol", args);
    }
    Object out = kread_obj;
    if (out.type == IDENTIFIER || out.type == IDENTIFIER_VERTICAL) {
      return out;
    } else {
      return wrong_type("string->symbol", args);
    }
  }
  case STRING_IMMUTABLE: {
    return (Object){.type = IDENTIFIER, .identifier = obj.identifier};
  }
  case STRING_IMMUTABLE_VERTICAL: {
    return (Object){.type = IDENTIFIER_VERTICAL,
                    .identifier_vertical = obj.identifier_vertical};
  }
  case NONE:
    error("scm_string_tosymbol");
  default:
    return wrong_type("string->symbol", args);
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
  case NONE:
    error("scm_char_upper_case_p");
  default:
    return wrong_type("char-upper-case?", args);
  }
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
  case NONE:
    error("scm_char_lower_case_p");
  default:
    return wrong_type("char-lower-case?", args);
  }
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
  case NONE:
    error("scm_digit_value");
  default:
    return wrong_type("digit-value", args);
  }
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
  case NONE:
    error("scm_char_tointeger");
  default:
    return wrong_type("char->integer", args);
  }
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
    error("scm_integer_tochar");
  default:
    return wrong_type("integer->char", args);
  }
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
  case NONE:
    error("scm_char_upcase");
  default:
    return wrong_type("char-upcase", args);
  }
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
  case NONE:
    error("scm_char_downcase");
  default:
    return wrong_type("char-downcase", args);
  }
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
  case NONE:
    error("scm_char_foldcase");
  default:
    return wrong_type("char-foldcase", args);
  }
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
  case STRING_IMMUTABLE:
  case STRING_IMMUTABLE_VERTICAL:
    return true_obj;
  case NONE:
    error("scm_string_p");
  default:
    return false_obj;
  }
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
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY:
  case STRING: {
    for (Object o = obj; o.type != STRING_EMPTY; o = value(string_cdrref(o))) {
      len++;
    }
    Object out = {.type = NUMBERZ};
    mpz_init_set_ui(out.numberz, len);
    return out;
  }
  case STRING_IMMUTABLE: {
    Object out = {.type = NUMBERZ};
    mpz_init_set_si(out.numberz, g_utf8_strlen(obj.string_immutable, -1));
    return out;
  }
  case STRING_IMMUTABLE_VERTICAL: {
    Object out = {.type = NUMBERZ};
    mpz_init_set_si(out.numberz,
                    g_utf8_strlen(obj.string_immutable_vertical, -1));
    return out;
  }
  case NONE:
    error("scm_string_length");
  default:
    return wrong_type("string-length", args);
  }
}
Object scm_string_ref(Object const args) {
  if (args_length(args) != 2) {
    return arguments(args, "string-ref");
  }
  Object s = value(carref(args));
  Object k = value(carref(cdrref(args)));
  switch (s.type) {
  case STRING:
  case STRING_IMMUTABLE:
  case STRING_IMMUTABLE_VERTICAL:
    break;
  case NONE:
    error("scm_string_ref");
  default:
    return wrong_type("string-ref", args);
  }
  long int i;
  switch (k.type) {
  case NUMBERZ: {
    i = mpz_get_si(k.numberz);
    break;
  }
  case NUMBERQ: {
    mpq_canonicalize(k.numberq);
    if (mpz_cmp_ui(mpq_denref(k.numberq), 1) == 0) {
      i = mpz_get_si(mpq_denref(k.numberq));
      break;
    }
    return wrong_type("string-ref", args);
  }
  case NONE:
    error("scm_string_ref");
  default:
    return wrong_type("string-ref", args);
  }
  if (i < 0) {
    return wrong_type("string-ref", args);
  }
  if (s.type == STRING) {
    for (; i > 0; i--) {
      s = string_cdrref(s);
      if (s.type == STRING_EMPTY) {
        return wrong_type("string-ref", args);
      }
    }
    Object c = string_carref(s);
    return c;
  }
  if (s.type == STRING_IMMUTABLE) {
    if (g_utf8_strlen(s.string_immutable, -1) <= i) {
      return wrong_type("string-ref", args);
    }
    char *ch = s.string_immutable;
    for (; i > 0; i--) {
      ch = g_utf8_find_next_char(ch, NULL);
    }
    return character_new(ch);
  }
  if (g_utf8_strlen(s.string_immutable_vertical, -1) <= i) {
    return wrong_type("string-ref", args);
  }
  char *ch = s.string_immutable_vertical;
  for (; i > 0; i--) {
    ch = g_utf8_find_next_char(ch, NULL);
  }
  return character_new(ch);
}
Object scm_string_set(Object const args) {
  if (args_length(args) != 3) {
    return arguments(args, "string-set!");
  }
  Object s = value(carref(args));
  Object k = value(carref(cdrref(args)));
  if (s.type != STRING || k.type != NUMBERZ) {
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
    error("scm_vector_p");
  default:
    return false_obj;
  }
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
    error("scm_vector_length");
  default:
    return wrong_type("vector-length", args);
  }
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
    error("scm_vector_ref");
  default:
    return wrong_type("vector-ref", args);
  }
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
    error("scm_vector_set");
  default:
    return wrong_type("vector-set!", args);
  }
}

Object scm_list_tovector(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "list->vector");
  }
  Object obj = value(carref(args));
  if (list_p(obj)) {
    return list2vector(obj);
  }
  return wrong_type("list->vector", args);
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
    error("scm_bytevector_p");
  default:
    return false_obj;
  }
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
    error("scm_bytevector_length");
  default:
    return wrong_type("bytevetor-length", args);
  }
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
    error("scm_bytevector_ueight_ref");
  default:
    return wrong_type("bytevetor-u8-ref", args);
  }
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
    error("scm_bytevector_ueight_set");
  default:
    return wrong_type("bytevetor-u8-set!", args);
  }
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
    return arguments(args, "utf8->string");
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
    error("scm_utfeight_tostring");
  default:
    return wrong_type("ut8->string", args);
  }
}
Object scm_string_toutfeight(Object const args) {
  Object obj;
  long int start;
  long int end = 0;
  switch (args_length(args)) {
  case 1: {
    obj = value(carref(args));
    start = 0;
    if (obj.type != STRING && obj.type != STRING_EMPTY &&
        obj.type != STRING_IMMUTABLE && obj.type != STRING_IMMUTABLE_VERTICAL) {
      return wrong_type("string->utf8", args);
    }
    end = 0;
    if (obj.type == STRING_IMMUTABLE) {
      end = strlen(obj.string_immutable);
    } else if (obj.type == STRING_IMMUTABLE_VERTICAL) {
      end = strlen(obj.string_immutable_vertical);
    } else {
      for (Object o = obj; o.type != STRING_EMPTY; o = cdrref(o)) {
        end++;
      }
    }
    break;
  }
  case 2: {
    obj = value(carref(args));
    Object obj2 = value(carref(cdrref(args)));
    if ((obj.type != STRING && obj.type != STRING_EMPTY &&
         obj.type != STRING_IMMUTABLE &&
         obj.type != STRING_IMMUTABLE_VERTICAL) ||
        (obj2.type != NUMBERZ && obj2.type != NUMBERQ)) {
      return wrong_type("string->utf8", args);
    }
    if (obj2.type == NUMBERZ) {
      start = mpz_get_si(obj2.numberz);
    } else {
      mpq_canonicalize(obj2.numberq);
      if (mpz_cmp_ui(mpq_denref(obj2.numberq), 1) == 0) {
        start = mpz_get_si(mpq_numref(obj2.numberq));
      } else {
        return wrong_type("string->utf8", args);
      }
    }
    if (start < 0) {
      return wrong_type("string->utf8", args);
    }
    end = 0;
    if (obj.type == STRING_IMMUTABLE) {
      end = strlen(obj.string_immutable);
    } else if (obj.type == STRING_IMMUTABLE_VERTICAL) {
      end = strlen(obj.string_immutable_vertical);
    } else {
      for (Object o = obj; o.type != STRING_EMPTY; o = cdrref(o)) {
        end++;
      }
    }
    if (start > end) {
      return wrong_type("string->utf8", args);
    }
    break;
  }
  case 3: {
    obj = value(carref(args));
    Object obj2 = value(carref(cdrref(args)));
    Object obj3 = value(carref(cdrref(cdrref(args))));
    if ((obj.type != STRING && obj.type != STRING_EMPTY &&
         obj.type != STRING_IMMUTABLE &&
         obj.type != STRING_IMMUTABLE_VERTICAL) ||
        (obj2.type != NUMBERZ && obj2.type != NUMBERQ) ||
        (obj3.type != NUMBERZ && obj3.type != NUMBERQ)) {
      return wrong_type("string->utf8", args);
    }
    if (obj2.type == NUMBERZ) {
      start = mpz_get_si(obj2.numberz);
    } else {
      mpq_canonicalize(obj2.numberq);
      if (mpz_cmp_ui(mpq_denref(obj2.numberq), 1) == 0) {
        start = mpz_get_si(mpq_numref(obj2.numberq));
      } else {
        return wrong_type("string->utf8", args);
      }
      if (start < 0) {
        return wrong_type("string->utf8", args);
      }
      if (obj3.type == NUMBERZ) {
        end = mpz_get_si(obj2.numberz);
      } else {
        mpq_canonicalize(obj3.numberq);
        if (mpz_cmp_ui(mpq_denref(obj3.numberq), 1) == 0) {
          end = mpz_get_si(mpq_numref(obj3.numberq));
        } else {
          return wrong_type("string->utf8", args);
        }
      }
      if (start > end) {
        return wrong_type("string->utf8", args);
      }
    }
    break;
  }
  default:
    return arguments(args, "string->utf8");
  }
  if (end == start) {
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
  case STRING_IMMUTABLE: {
    Object t = empty;
    for (long int i = end - 1; i >= start; i--) {
      Object o = {.type = NUMBERZ};
      mpz_init_set_ui(o.numberz, (uint8_t)obj.string_immutable[i]);
      t = cons(o, t);
    }
    Object out = list2bytevector(t);
    return out;
  }
  case STRING_IMMUTABLE_VERTICAL: {
    Object t = empty;
    for (long int i = end - 1; i >= start; i--) {
      Object o = {.type = NUMBERZ};
      mpz_init_set_ui(o.numberz, (uint8_t)obj.string_immutable_vertical[i]);
      t = cons(o, t);
    }
    Object out = list2bytevector(t);
    return out;
  }
  case NONE:
    error("scm_string_toutfeight");
  default:
    return wrong_type("string->utf8", args);
  }
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
  case PRIMITIVE_PROCEDURE_FORCE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    return true_obj;

  case NONE:
    error("scm_procedure_p");
  default:
    return false_obj;
  }
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
    error("scm_error_object_p");
  default:
    return false_obj;
  }
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
    error("scm_error_object_irritants");
  default:
    return wrong_type("error-object-irritants", args);
  }
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
    error("scm_error_object_message");
  default:
    return wrong_type("error-object-message", args);
  }
}
Object scm_read_error_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "file-error?");
  }
  return value(carref(args)).type == READ_ERROR ? true_obj : false_obj;
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
  case PORT_INPUT_TEXT_STRING:
    return true_obj;
  case NONE:
    error("scm_input_port_p");
  default:
    return false_obj;
  }
}
Object scm_output_port_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "output-port?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_OUTPUT_TEXT_STRING:
    return true_obj;
  case NONE:
    error("scm_output_port_p'");
  default:
    return false_obj;
  }
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
    error("scm_textual_port_p");
  default:
    return false_obj;
  }
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
    error("scm_binary_port_p");
  default:
    return false_obj;
  }
}
Object scm_port_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "port?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_INPUT_TEXT_STRING:
  case PORT_OUTPUT_TEXT_STRING:
    return true_obj;
  case NONE:
    error("scm_port_p");
  default:
    return false_obj;
  }
}
Object scm_input_port_open_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "input-port-open?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_INPUT_TEXT_STRING:
    return carref(obj).port != NULL ? true_obj : false_obj;
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_OUTPUT_TEXT_STRING:
    return false_obj;
  case NONE:
    error("scm_input_port_open_p");
  default:
    return wrong_type("input-port-open?", args);
  }
}
Object scm_output_port_open_p(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "output-port-open?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_OUTPUT_TEXT_STRING:
    return carref(obj).port != NULL ? true_obj : false_obj;
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_INPUT_TEXT_STRING:
    return false_obj;
  case NONE:
    error("scm_output_port_open_p");
  default:
    return wrong_type("output-port-open?", args);
  }
}
#include <errno.h>
#include <string.h>
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
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_OUTPUT_TEXT;
    return out;
  }
  case STRING_IMMUTABLE: {
    FILE *f = fopen(obj.string_immutable, "w");
    if (f == NULL) {
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    Object out = cons((Object){.port = f}, obj);
    out.type = PORT_OUTPUT_TEXT;
    return out;
  }
  case NONE:
    error("scm_open_output_file");
  default:
    return wrong_type("open-output-file", args);
  }
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
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_OUTPUT_BINARY;
    return out;
  }
  case STRING_IMMUTABLE: {
    FILE *f = fopen(obj.string_immutable, "wb");
    if (f == NULL) {
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    Object out = cons((Object){.port = f}, obj);
    out.type = PORT_OUTPUT_BINARY;
    return out;
  }
  case NONE:
    error("scm_open_binary_output_file");
  default:
    return wrong_type("open-binary-output-file", args);
  }
}
Object scm_current_input_port(Object const args) {
  if (args.type != EMPTY) {
    return arguments(args, "current-input-port");
  }
  return cur_in;
}
Object scm_current_output_port(Object const args) {
  if (args.type != EMPTY) {
    return arguments(args, "current-output-port");
  }
  return cur_out;
}
Object scm_current_error_port(Object const args) {
  if (args.type != EMPTY) {
    return arguments(args, "current-error-port");
  }
  return cur_err;
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
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_INPUT_TEXT;
    return out;
  }
  case STRING_IMMUTABLE: {
    FILE *f = fopen(obj.string_immutable, "r");
    if (f == NULL) {
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
      ;
    }
    Object out = cons((Object){.port = f}, obj);
    out.type = PORT_INPUT_TEXT;
    return out;
  }
  case NONE:
    error("scm_open_input_file");
  default:
    return wrong_type("open-input-file", args);
  }
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
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
      ;
    }
    Object out = cons((Object){.port = f}, obj);
    g_free(s);
    out.type = PORT_INPUT_BINARY;
    return out;
  }
  case STRING_IMMUTABLE: {
    FILE *f = fopen(obj.string_immutable, "rb");
    if (f == NULL) {
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
      ;
    }
    Object out = cons((Object){.port = f}, obj);
    out.type = PORT_INPUT_BINARY;
    return out;
  }
  case NONE:
    error("scm_open_binary_input_file");
  default:
    return wrong_type("open-binary-input-file", args);
  }
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
  case PORT_INPUT_BINARY:
  case PORT_INPUT_TEXT_STRING:
  case PORT_OUTPUT_TEXT_STRING: {
    fclose(carref(obj).port);
    cars[obj.index].port = NULL;
    return unspecified;
  }
  case NONE:
    error("scm_close_port");
  default:
    return wrong_type("close-port", args);
  }
}

FILE *const string_port = (FILE *)1;
Object scm_open_input_string(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "open-input-string");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case STRING_EMPTY: {
  }
  case STRING: {
  }
  case STRING_IMMUTABLE: {
    FILE *stream = tmpfile();
    fprintf(stream, "%s\n", obj.string_immutable);
    fseek(stream, 0, SEEK_SET);
    Object out = cons((Object){.port = stream}, empty);
    out.type = PORT_INPUT_TEXT_STRING;
    return out;
  }
  case STRING_IMMUTABLE_VERTICAL: {
    FILE *stream = tmpfile();
    if (stream == NULL) {
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    fprintf(stream, "%s\n", obj.string_immutable_vertical);
    fseek(stream, 0, SEEK_SET);
    Object out = cons((Object){.port = stream}, empty);
    out.type = PORT_INPUT_TEXT_STRING;
    return out;
  }
  case NONE:
    error("scm_open_input_string");
  default:
    return wrong_type("open-input-string", args);
  }
}
Object scm_open_output_string(Object const args) {
  if (args.type == EMPTY) {
    FILE *stream = tmpfile();
    if (stream == NULL) {
      return (Object){.type = FILE_ERROR, .message = strdup(strerror(errno))};
    }
    Object out = cons((Object){.port = stream}, empty);
    out.type = PORT_OUTPUT_TEXT_STRING;
    return out;
  }
  return arguments(args, "open-output-string");
}
Object scm_get_output_string(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "get-output-string");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PORT_OUTPUT_TEXT_STRING: {
    Object o = empty;
    FILE *stream = carref(obj).port;
    fseek(stream, 0, SEEK_SET);
    char buf[6];
    size_t i = 0;
    while (true) {
      buf[i] = fgetc(stream);
      if (buf[i] == EOF) {
        break;
      }
      gunichar ch = g_utf8_get_char_validated(buf, i + 1);
      if (ch == -1) {
        i++;
      } else {
        o = cons((Object){.type = CHARACTER, .character = ch}, o);
        i = 0;
      }
    }
    fseek(stream, 0, SEEK_END);
    Object out = {.type = STRING_EMPTY};
    for (; o.type != EMPTY; o = cdrref(o)) {
      save(o);
      out = string_cons(carref(o), out);
      restore(&o);
    }
    return out;
  }
  case NONE:
    error("scm_get_output_string");
  default:
    return wrong_type("get-output-string", args);
  }
}

int interactive_mode = 1;
extern FILE *yyin;
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
    switch (obj.type) {
    case PORT_INPUT_TEXT:
    case PORT_INPUT_TEXT_STRING: {
      if (carref(obj).port == NULL) {
        return (Object){.type = READ_ERROR, .message = strdup("closed port")};
      }
      char ch = fgetc(carref(obj).port);
      ungetc(ch, carref(obj).port);
      if (ch == EOF) {
        return (Object){.type = EOF_OBJ};
      }
      FILE *stream = carref(cur_in).port;
      yyin = carref(obj).port;
      interactive_mode = 0;
      Object out = kread();
      interactive_mode = 1;
      yyin = stream;
      return out;
    }
    default:
      return wrong_type("read", args);
    }
  }
  default:
    return arguments(args, "read");
  }
}
Object scm_read_char(Object const args) {
  switch (args_length(args)) {
  case 0: {
    char s[6];
    for (size_t i = 0; i < 6; i++) {
      char ch = fgetc(carref(cur_in).port);
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
    FILE *stream = carref(obj).port;
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
    object_write(carref(cur_out).port, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj1 = carref(args);
    Object obj2 = value(carref(cdrref(args)));
    switch (obj2.type) {
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_TEXT_STRING: {
      object_write(carref(obj2).port, obj1);
      return unspecified;
    }
    case NONE:
      error("scm_write");
    default:
      return wrong_type("write", args);
    }
  }
  default:
    return arguments(args, "write");
  }
}
Object scm_write_shared(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_write_shared(yyout, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj1 = carref(args);
    Object obj2 = value(carref(cdrref(args)));
    switch (obj2.type) {
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_TEXT_STRING: {
      object_write_shared(carref(obj2).port, obj1);
      return unspecified;
    }
    case NONE:
      error("scm_write_shared");
    default:
      return wrong_type("write-shared", args);
    }
  }
  default:
    return arguments(args, "write-shared");
  }
}
Object scm_write_simple(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_write_simple(yyout, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj1 = carref(args);
    Object obj2 = value(carref(cdrref(args)));
    switch (obj2.type) {
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_TEXT_STRING: {
      object_write_simple(carref(obj2).port, obj1);
      return unspecified;
    }
    case NONE:
      error("scm_write_simple");
    default:
      return wrong_type("write-simple", args);
    }
  }
  default:
    return arguments(args, "write-simple");
  }
}
Object scm_display(Object const args) {
  switch (args_length(args)) {
  case 1: {
    object_display(carref(cur_out).port, carref(args));
    return unspecified;
  }
  case 2: {
    Object obj1 = carref(args);
    Object obj2 = value(carref(cdrref(args)));
    switch (obj2.type) {
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_TEXT_STRING: {
      object_display(carref(obj2).port, obj1);
      return unspecified;
    }
    case NONE:
      error("scm_display");
    default:
      return wrong_type("display", args);
    }
  }
  default:
    return arguments(args, "display");
  }
}
Object scm_newline(Object const args) {
  size_t len = args_length(args);
  if (len == 0) {
    fprintf(carref(cur_out).port, "\n");
  } else if (len == 1) {
    Object obj = value(carref(args));
    fprintf(carref(obj).port, "\n");
  } else {
    return arguments(args, "newline");
  }
  return unspecified;
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
  case STRING_IMMUTABLE: {
    gboolean b = g_file_test(obj.string_immutable, G_FILE_TEST_EXISTS);
    return b ? true_obj : false_obj;
  }
  default:
    return wrong_type("file-exists?", args);
  }
  exit(1);
}
Object scm_primitive_delete_file(Object const args) {
  if (args_length(args) != 1) {
    return arguments(args, "delete-file");
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
    int n = remove(filename);
    g_free(filename);
    return n == 0 ? unspecified : (Object){.type = FILE_ERROR,
                                           .message = strdup(strerror(errno))};
  }
  case STRING_IMMUTABLE: {
    int n = remove(obj.string_immutable);
    return n == 0 ? unspecified : (Object){.type = FILE_ERROR,
                                           .message = strdup(strerror(errno))};
  }
  case NONE:
    error("scm_primitive_delete_file");
  default:
    return wrong_type("delete-file", args);
  }
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
