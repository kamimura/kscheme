/** \file  */
#include "procedures.h"

extern FILE *yyout;
static Object arguments(Object obj, char const *s) {
  fprintf(yyout, "Error: ");
  object_write(yyout, obj);
  fprintf(yyout, " wrong number of arguments -- %s\n", s);
  return (Object){.type = WRONG_NUMBER_OF_ARGUMENTS};
}
static size_t args_length(Object args) {
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
  }
  return o;
}
Object scm_eqv_p(Object args) {
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
      exit(1);
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
      exit(1);
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
      exit(1);
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
      exit(1);
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
  default:
    exit(1);
  }
  return false_obj;
}
Object scm_eq_p(Object args) {
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
  default:
    exit(1);
  }
  exit(1);
}

/* Equivalence predicates end */

/* Numbers */
Object scm_number_p(Object args) {
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
  exit(1);
}
Object scm_complex_p(Object args) {
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
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_rational_p(Object args) {
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
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
static Object wrong_type(char const *prog_name, Object obj) {
  fprintf(yyout, "Error: (%s) wrong type argument  -- ", prog_name);
  object_write(yyout, obj);
  fprintf(yyout, "\n");
  return (Object){.type = WRONG_TYPE_ARGUMENT};
}
Object scm_exact_p(Object args) {
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
    exit(1);
  default:
    return wrong_type("exact?", args);
  }
  exit(1);
}
Object scm_inexact_p(Object args) {
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
    exit(1);
  default:
    return wrong_type("inexact?", args);
  }
  exit(1);
}
Object scm_finite_p(Object args) {
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
    exit(1);
  default:
    return wrong_type("finite?", args);
  }
  exit(1);
}
Object scm_infinite_p(Object args) {
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
    exit(1);
  default:
    return wrong_type("infinite?", args);
  }
  exit(1);
}
Object scm_nan_p(Object args) {
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
    exit(1);
  default:
    return wrong_type("nan?", args);
  }
  exit(1);
}
Object scm_zero_p(Object args) {
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
    exit(1);
  default:
    return wrong_type("zero?", args);
  }
  exit(1);
}
Object scm_positive_p(Object args) {
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
      exit(1);
    }
    return mpfr_sgn(mpc_realref(obj.numberc)) > 0 ? true_obj : false_obj;
  case NONE:
    exit(1);
  default:
    return wrong_type("positive?", args);
  }
  exit(1);
}
Object scm_negative_p(Object args) {
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
      exit(1);
    }
    return mpfr_sgn(mpc_realref(obj.numberc)) < 0 ? true_obj : false_obj;
  case NONE:
    exit(1);
  default:
    return wrong_type("negative?", args);
  }
  exit(1);
}

Object scm_add(Object args) {
  Object out = numberz_new("0", 10);
  if (args.type == EMPTY) {
    return out;
  }
  for (Object a = args; a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (out.type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        mpz_t op;
        mpz_init(op);
        mpz_add(op, out.numberz, arg.numberz);
        mpz_clear(out.numberz);
        mpz_init_set(out.numberz, op);
        mpz_clear(op);
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set_z(op, out.numberz);
        mpz_clear(out.numberz);
        mpq_init(out.numberq);
        mpq_add(out.numberq, op, arg.numberq);
        mpq_clear(op);
        out.type = NUMBERQ;
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set_z(op, out.numberz, MPFR_RNDN);
        mpz_clear(out.numberz);
        mpfr_init(out.numberr);
        mpfr_add(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_z(op, out.numberz, MPC_RNDNN);
        mpz_clear(out.numberz);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_add(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("+", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_t op1, op2;
        mpq_inits(op1, op2, NULL);
        mpq_set(op1, out.numberq);
        mpq_set_z(op2, arg.numberz);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_add(out.numberq, op1, op2);
        mpq_clears(op1, op2, NULL);
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set(op, out.numberq);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_add(out.numberq, op, arg.numberq);
        mpq_clear(op);
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init(op);
        mpfr_set_q(op, out.numberq, MPFR_RNDN);
        mpq_clear(out.numberq);
        mpfr_init(out.numberr);
        mpfr_add(out.numberr, op, arg.numberr, MPFR_RNDN);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_q(op, out.numberq, MPC_RNDNN);
        mpq_clear(out.numberq);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_add(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("+", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_add_z(out.numberr, op, arg.numberz, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERQ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_add_q(out.numberr, op, arg.numberq, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init(op);
        mpfr_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_add(out.numberr, op, arg.numberr, MPFR_RNDN);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_fr(op, out.numberr, MPC_RNDNN);
        mpfr_clear(out.numberr);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_add(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("+", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_z(op1, arg.numberz, MPFR_RNDN);
        mpc_add_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERQ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_q(op1, arg.numberq, MPFR_RNDN);
        mpc_add_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERR: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_add_fr(out.numberc, op, arg.numberr, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_add(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      default:
        object_free(&out);
        return wrong_type("+", args);
      }
      break;
    }
    default:
      object_free(&out);
      return wrong_type("+", args);
    }
  }
  return out;
}

Object scm_mul(Object args) {
  Object out = numberz_new("1", 10);
  if (args.type == EMPTY) {
    return out;
  }
  for (Object a = args; a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (out.type) {
    case NUMBERZ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpz_t op;
        mpz_init(op);
        mpz_mul(op, out.numberz, arg.numberz);
        mpz_clear(out.numberz);
        mpz_init_set(out.numberz, op);
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set_z(op, out.numberz);
        mpz_clear(out.numberz);
        mpq_init(out.numberq);
        mpq_mul(out.numberq, op, arg.numberq);
        mpq_clear(op);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        } else {
          out.type = NUMBERQ;
        }
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set_z(op, out.numberz, MPFR_RNDN);
        mpz_clear(out.numberz);
        mpfr_init(out.numberr);
        mpfr_mul(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_z(op, out.numberz, MPFR_RNDN);
        mpz_clear(out.numberz);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_mul(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }

      default:
        object_free(&out);
        return wrong_type("*", args);
      }
      break;
    }
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_t op1, op2;
        mpq_inits(op1, op2, NULL);
        mpq_set(op1, out.numberq);
        mpq_set_z(op2, arg.numberz);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_mul(out.numberq, op1, op2);
        mpq_clears(op1, op2, NULL);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        }
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set(op, out.numberq);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_mul(out.numberq, op, arg.numberq);
        mpq_clear(op);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        }
        break;
      }
      case NUMBERR: {
        mpq_t op;
        mpq_init(op);
        mpq_set(op, out.numberq);
        mpq_clear(out.numberq);
        mpfr_init(out.numberr);
        mpfr_mul_q(out.numberr, arg.numberr, op, MPFR_RNDN);
        mpq_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_q(op, out.numberq, MPFR_RNDN);
        mpq_clear(out.numberq);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_mul(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("*", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_mul_z(out.numberr, op, arg.numberz, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERQ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_mul_q(out.numberr, op, arg.numberq, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_mul(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_fr(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_mul(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("*", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_z(op1, arg.numberz, MPFR_RNDN);
        mpc_mul_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERQ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_q(op1, arg.numberq, MPFR_RNDN);
        mpc_mul_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERR: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_mul_fr(out.numberc, op, arg.numberr, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_mul(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      default:
        object_free(&out);
        return wrong_type("*", args);
      }
      break;
    }
    default:
      object_free(&out);
      return wrong_type("*", args);
    }
  }
  return out;
}

Object scm_sub(Object args) {
  if (args_length(args) == 0) {
    return arguments(args, "-");
  }
  if (args_length(args) == 1) {
    Object obj = value(carref(args));
    switch (obj.type) {
    case NUMBERZ: {
      mpz_t op;
      mpz_init(op);
      mpz_neg(op, obj.numberz);
      Object out = {.type = NUMBERZ};
      mpz_init_set(out.numberz, op);
      mpz_clear(op);
      return out;
    }
    case NUMBERQ: {
      mpq_t op;
      mpq_init(op);
      mpq_neg(op, obj.numberq);
      Object out = {.type = NUMBERQ};
      mpq_init(out.numberq);
      mpq_set(out.numberq, op);
      mpq_clear(op);
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_neg(out.numberr, obj.numberr, MPFR_RNDN);
      return out;
    }
    case NUMBERC: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_neg(out.numberc, obj.numberc, MPC_RNDNN);
      return out;
    }
    case NONE:
      exit(1);
    default:
      return wrong_type("-", args);
    }
  }
  Object out = car(args);
  for (Object a = cdrref(args); a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (out.type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        mpz_t op;
        mpz_init(op);
        mpz_sub(op, out.numberz, arg.numberz);
        mpz_clear(out.numberz);
        mpz_init_set(out.numberz, op);
        out.type = NUMBERZ;
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set_z(op, out.numberz);
        mpz_clear(out.numberz);
        mpz_init(out.numberz);
        mpq_sub(out.numberq, op, arg.numberq);
        mpq_clear(op);
        out.type = NUMBERQ;
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set_z(op, out.numberz, MPFR_RNDN);
        mpz_clear(out.numberz);
        mpfr_init(out.numberr);
        mpfr_sub(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_z(op, out.numberz, MPC_RNDNN);
        mpz_clear(out.numberz);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_sub(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("-", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_t op1, op2;
        mpq_inits(op1, op2, NULL);
        mpq_set(op1, out.numberq);
        mpq_set_z(op2, arg.numberz);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_sub(out.numberq, op1, op2);
        mpq_clears(op1, op2, NULL);
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set(op, out.numberq);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_sub(out.numberq, op, arg.numberq);
        mpq_clear(op);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        }
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set_q(op, out.numberq, MPFR_RNDN);
        mpq_clear(out.numberq);
        mpfr_init(out.numberr);
        mpfr_sub(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_q(op, out.numberq, MPFR_RNDN);
        mpq_clear(out.numberq);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_sub(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("-", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_sub_z(out.numberr, op, arg.numberz, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERQ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_sub_q(out.numberr, op, arg.numberq, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_sub(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_fr(op, out.numberr, MPC_RNDNN);
        mpfr_clear(out.numberr);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_sub(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        break;
      }
      default:
        object_free(&out);
        return wrong_type("-", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_z(op1, arg.numberz, MPFR_RNDN);
        mpc_sub_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERQ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_q(op1, arg.numberq, MPFR_RNDN);
        mpc_sub_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERR: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_sub_fr(out.numberc, op, arg.numberr, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_sub(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      default:
        object_free(&out);
        return wrong_type("-", args);
      }
      break;
    }
    default:
      object_free(&out);
      return wrong_type("-", args);
    }
  }
  return out;
}
Object scm_div(Object args) {
  if (args_length(args) == 0) {
    return arguments(args, "/");
  }
  if (args_length(args) == 1) {
    Object obj = value(carref(args));
    switch (obj.type) {
    case NUMBERZ: {
      mpz_t op;
      mpz_init(op);
      mpz_neg(op, obj.numberz);
      Object out = {.type = NUMBERQ};
      mpq_init(out.numberq);
      mpz_set_str(mpq_numref(out.numberq), "1", 10);
      mpz_set(mpq_denref(out.numberq), obj.numberz);
      mpq_canonicalize(out.numberq);
      return out;
    }
    case NUMBERQ: {
      Object out = {.type = NUMBERQ};
      mpq_init(out.numberq);
      mpq_inv(out.numberq, obj.numberq);
      if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
        mpz_t op;
        mpz_init_set(op, mpq_numref(out.numberq));
        mpq_clear(out.numberq);
        mpz_init_set(out.numberz, op);
        mpz_clear(op);
        out.type = NUMBERZ;
      }
      return out;
    }
    case NUMBERR: {
      Object out = {.type = NUMBERR};
      mpfr_init(out.numberr);
      mpfr_ui_div(out.numberr, 1, obj.numberr, MPFR_RNDN);
      return out;
    }
    case NUMBERC: {
      Object out = {.type = NUMBERC};
      mpc_init2(out.numberc, MPC_PREC);
      mpc_ui_div(out.numberc, 1, obj.numberc, MPC_RNDNN);
      return out;
    }
    case NONE:
      exit(1);
    default:
      return wrong_type("/", args);
    }
  }
  Object out = car(args);
  if (args.type == EMPTY) {
    return out;
  }
  for (Object a = cdrref(args); a.type != EMPTY; a = cdrref(a)) {
    Object arg = value(carref(a));
    switch (out.type) {
    case NUMBERZ:
      switch (arg.type) {
      case NUMBERZ: {
        mpq_t op;
        mpq_init(op);
        mpz_set(mpq_numref(op), out.numberz);
        mpz_set(mpq_denref(op), arg.numberz);
        mpq_canonicalize(op);
        mpz_clear(out.numberz);
        mpq_init(out.numberq);
        mpq_set(out.numberq, op);
        mpq_clear(op);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        } else {
          out.type = NUMBERQ;
        }
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set_z(op, out.numberz);
        mpz_clear(out.numberz);
        mpq_init(out.numberq);
        mpq_div(out.numberq, op, arg.numberq);
        mpq_clear(op);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        } else {
          out.type = NUMBERQ;
        }
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set_z(op, out.numberz, MPFR_RNDN);
        mpz_clear(out.numberz);
        mpfr_init(out.numberr);
        mpfr_div(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_z(op, out.numberz, MPC_RNDNN);
        mpz_clear(out.numberz);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("/", args);
      }
      break;
    case NUMBERQ: {
      switch (arg.type) {
      case NUMBERZ: {
        mpq_t op1, op2;
        mpq_inits(op1, op2, NULL);
        mpq_set(op1, out.numberq);
        mpq_set_z(op2, arg.numberz);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_div(out.numberq, op1, op2);
        mpq_clears(op1, op2, NULL);
        break;
      }
      case NUMBERQ: {
        mpq_t op;
        mpq_init(op);
        mpq_set(op, out.numberq);
        mpq_clear(out.numberq);
        mpq_init(out.numberq);
        mpq_div(out.numberq, op, arg.numberq);
        mpq_clear(op);
        if (mpz_cmp_d(mpq_denref(out.numberq), 1) == 0) {
          mpz_t op;
          mpz_init_set(op, mpq_numref(out.numberq));
          mpq_clear(out.numberq);
          mpz_init_set(out.numberz, op);
          mpz_clear(op);
          out.type = NUMBERZ;
        }
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set_q(op, out.numberq, MPFR_RNDN);
        mpq_clear(out.numberq);
        mpfr_init(out.numberr);
        mpfr_div(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        out.type = NUMBERR;
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_q(op, out.numberq, MPC_RNDNN);
        mpq_clear(out.numberq);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("/", args);
      }
      break;
    }
    case NUMBERR: {
      switch (arg.type) {
      case NUMBERZ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_div_z(out.numberr, op, arg.numberz, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERQ: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_div_q(out.numberr, op, arg.numberq, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERR: {
        mpfr_t op;
        mpfr_init_set(op, out.numberr, MPFR_RNDN);
        mpfr_clear(out.numberr);
        mpfr_init(out.numberr);
        mpfr_div(out.numberr, op, arg.numberr, MPFR_RNDN);
        mpfr_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set_fr(op, out.numberr, MPC_RNDNN);
        mpfr_clear(out.numberr);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, op, arg.numberc, MPFR_RNDN);
        mpc_clear(op);
        out.type = NUMBERC;
        break;
      }
      default:
        object_free(&out);
        return wrong_type("/", args);
      }
      break;
    }
    case NUMBERC: {
      switch (arg.type) {
      case NUMBERZ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_z(op1, arg.numberz, MPFR_RNDN);
        mpc_div_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERQ: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpfr_t op1;
        mpfr_init_set_q(op1, arg.numberq, MPFR_RNDN);
        mpc_div_fr(out.numberc, op, op1, MPC_RNDNN);
        mpc_clear(op);
        mpfr_clear(op1);
        break;
      }
      case NUMBERR: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div_fr(out.numberc, op, arg.numberr, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      case NUMBERC: {
        mpc_t op;
        mpc_init2(op, MPC_PREC);
        mpc_set(op, out.numberc, MPC_RNDNN);
        mpc_clear(out.numberc);
        mpc_init2(out.numberc, MPC_PREC);
        mpc_div(out.numberc, op, arg.numberc, MPC_RNDNN);
        mpc_clear(op);
        break;
      }
      default:
        object_free(&out);
        return wrong_type("/", args);
      }
      break;
    }
    default:
      object_free(&out);
      return wrong_type("/", args);
    }
  }
  return out;
}
Object scm_numerator(Object args) {
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
  exit(1);
}
Object scm_denominator(Object args) {
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
  exit(1);
}
Object scm_floor(Object args) {
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
  exit(1);
}
Object scm_ceiling(Object args) {
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
  exit(1);
}
Object scm_truncate(Object args) {
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
  exit(1);
}

Object scm_sqrt(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "sqrt");
  }
  Object arg = value(carref(args));
  switch (arg.type) {
  case NUMBERZ: {
    if (mpz_perfect_square_p(arg.numberz)) {
      Object out = {.type = NUMBERZ};
      mpz_init(out.numberz);
      mpz_sqrt(out.numberz, arg.numberz);
      return out;
    }
    mpfr_t op;
    mpfr_init_set_z(op, arg.numberz, MPFR_RNDN);
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sqrt(out.numberr, op, MPFR_RNDN);
    mpfr_clear(op);
    return out;
  }
  case NUMBERQ: {
    if (mpz_perfect_square_p(mpq_numref(arg.numberq)) &&
        mpz_perfect_square_p(mpq_denref(arg.numberq))) {
      Object out = {.type = NUMBERQ};
      mpq_init(out.numberq);
      mpz_sqrt(mpq_numref(out.numberq), mpq_numref(arg.numberq));
      mpz_sqrt(mpq_denref(out.numberq), mpq_denref(arg.numberq));
      return out;
    }
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init(out.numberr);
    mpfr_sqrt(out.numberr, arg.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_sqrt(out.numberc, arg.numberc, MPC_RNDNN);
    return out;
  }
  default:
    return wrong_type("sqrt", args);
  }
  exit(1);
}

/* Numbers end */

/* Pairs and lists */
Object scm_pair_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "pair?");
  }
  switch (value(carref(args)).type) {
  case PAIR:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_cons(Object args) {
  if (args_length(args) != 2) {
    return arguments(args, "cons");
  }
  return cons(car(args), car(cdrref(args)));
}
Object scm_car(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "car");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PAIR:
    return car(obj);
  case NONE:
    exit(1);
  default:
    return wrong_type("car", args);
  }
  exit(1);
}
Object scm_cdr(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "cdr");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case PAIR:
    return cdr(obj);
  case NONE:
    exit(1);
  default:
    return wrong_type("cdr", args);
  }
  exit(1);
}
Object scm_set_car(Object args) {
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
    exit(1);
  default:
    return wrong_type("set-car!", args);
  }
  exit(1);
}
Object scm_set_cdr(Object args) {
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
    exit(1);
  default:
    return wrong_type("set-cdr!", args);
  }
  exit(1);
}
/* Pairs and lists and */

/* Symbols */
Object scm_symbol_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "symbol?");
  }
  switch (value(carref(args)).type) {
  case IDENTIFIER:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
/* Symbols end */
/* Characters */
Object scm_char_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "char?");
  }
  switch (value(carref(args)).type) {
  case CHARACTER:
    return true_obj;
  case NONE:
    exit(1);
  default:
    return false_obj;
  }
  exit(1);
}
Object scm_char_alphabetic_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-alphabetic?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return g_unichar_isalpha(obj.character) ? true_obj : false_obj;
  default:
    return wrong_type("char-alphabetic?", args);
  }
  exit(1);
}
Object scm_char_numeric_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-numeric?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return g_unichar_isdigit(obj.character) ? true_obj : false_obj;
  default:
    return wrong_type("char-numeric?", args);
  }
  exit(1);
}
Object scm_char_whitespace_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "char-whitespace?");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case CHARACTER:
    return g_unichar_isspace(obj.character) ? true_obj : false_obj;
  default:
    return wrong_type("char-whitespace?", args);
  }
  exit(1);
}
Object scm_char_upper_case_p(Object args) {
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
Object scm_char_lower_case_p(Object args) {
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
Object scm_digit_value(Object args) {
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
Object scm_char_tointeger(Object args) {
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
Object scm_integer_tochar(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "integer->char");
  }
  Object obj = value(carref(args));
  switch (obj.type) {
  case NUMBERZ: {
    return (Object){.type = CHARACTER, .character = mpz_get_ui(obj.numberz)};
  }
  case NUMBERQ: {
    /* if (integer?) { */

    /* } */
    return wrong_type("integer->char", args);
  }
  case NUMBERR: {
    /* if (integer?) { */

    /* } */
    return wrong_type("integer->char", args);
  }
  case NUMBERC: {
    /* if (integer?) { */

    /* } */
    return wrong_type("integer->char", args);
  }
  case NONE:
    exit(1);
  default:
    return wrong_type("integer->char", args);
  }
  exit(1);
}
Object scm_char_upcase(Object args) {
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
Object scm_char_downcase(Object args) {
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
Object scm_char_foldcase(Object args) {
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
Object scm_string_p(Object args) {
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
Object scm_make_string(Object args) {
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
Object scm_string(Object args) {
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
Object scm_string_length(Object args) {
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
Object scm_string_ref(Object args) {
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
Object scm_string_set(Object args) {
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
Object scm_vector_p(Object args) {
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
Object scm_vector(Object args) { return list2vector(args); }
Object scm_vector_length(Object args) {
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
Object scm_vector_ref(Object args) {
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
Object scm_vector_set(Object args) {
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
Object scm_bytevector_p(Object args) {
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
Object scm_bytevector(Object args) { return list2bytevector(args); }
Object scm_bytevector_length(Object args) {
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

Object scm_bytevector_ueight_ref(Object args) {
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
Object scm_bytevector_ueight_set(Object args) {
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
Object scm_utfeight_tostring(Object args) {
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
Object scm_string_toutfeight(Object args) {
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
Object scm_procedure_p(Object args) {
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
Object scm_error_implementation_defined_object(Object args) {
  if (args_length(args) < 1) {
    return arguments(args, "error-implementation-defined-object");
  }
  Object out = args;
  out.type = IMPLEMENTATION_DEFINED_OBJECT;
  return out;
}
Object scm_error_object_p(Object args) {
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
Object scm_error_object_irritants(Object args) {
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
Object scm_error_object_message(Object args) {
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
Object scm_file_error_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "file-error?");
  }
  return value(carref(args)).type == FILE_ERROR ? true_obj : false_obj;
}
/* Exceptions end */
/* Input and output */
Object scm_input_port_p(Object args) {
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
Object scm_output_port_p(Object args) {
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
Object scm_textual_port_p(Object args) {
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
Object scm_binary_port_p(Object args) {
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
Object scm_input_port_open_p(Object args) {
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
Object scm_output_port_open_p(Object args) {
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
Object scm_open_output_file(Object args) {
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
Object scm_open_binary_output_file(Object args) {
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
Object scm_open_input_file(Object args) {
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
Object scm_open_binary_input_file(Object args) {
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
Object scm_close_port(Object args) {
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
Object scm_read(Object args) {
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
Object scm_read_char(Object args) {
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
Object scm_eof_object_p(Object args) {
  if (args_length(args) != 1) {
    return arguments(args, "eof-object?");
  }
  return value(carref(args)).type == EOF_OBJ ? true_obj : false_obj;
}
Object scm_eof_object(Object args) {
  if (args_length(args) != 0) {
    return arguments(args, "eof-object");
  }
  return (Object){.type = EOF_OBJ};
}
extern FILE *yyout;
Object scm_write(Object args) {
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
Object scm_write_shared(Object args) {
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
Object scm_write_simple(Object args) {
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
Object scm_display(Object args) {
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
Object scm_file_exists_p(Object args) {
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
Object scm_primitive_delete_file(Object args) {
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
Object scm_emergency_exit(Object args) {
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
