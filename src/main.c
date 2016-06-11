/** \file  */
#include "object.h"
#include "parse.tab.h"
#include "environment.h"
#include "procedures.h"

Object stack;
void save(Object obj) { stack = cons(obj, stack); }
void restore(Object *ptr) {
  *ptr = car(stack);
  stack = cdrref(stack);
}
extern void yyrestart(FILE *);
Object kread() {
  yyparse();
  return kread_obj;
}
Object reverse(Object args) {
  Object out = empty;
  for (Object a = args; a.type != EMPTY; a = cdrref(a)) {
    out = cons(car(a), out);
  }
  return out;
}
Object argl_append(Object obj) {
  Object reversed = reverse(obj);
  Object out = car(reversed);
  for (Object t = cdrref(reversed); t.type != EMPTY; t = cdrref(t)) {
    out = cons(car(t), out);
  }
  return out;
}
size_t list_length(Object obj) {
  size_t len = 0;
  for (Object t = obj; t.type != EMPTY; len++, t = cdrref(t)) {
    ;
  }
  return len;
}
#include <stdbool.h>
bool list_p(Object obj) {
  if (obj.type == EMPTY) {
    return true;
  }
  if (obj.type != PAIR) {
    return false;
  }
  Object o1 = obj;
  Object o2 = cdrref(obj);
  while (o2.type != EMPTY) {
    if (o2.type != PAIR) {
      return false;
    }
    if (o1.index == o2.index) {
      return false;
    }
    o1 = cdrref(o1);
    o2 = cdrref(o2);
    if (o2.type == EMPTY) {
      return true;
    }
    if (o2.type != PAIR) {
      return false;
    }
    o2 = cdrref(o2);
  }
  return true;
}
bool list_last_list_p(Object obj) {
  size_t len = list_length(obj);
  Object o = obj;
  for (size_t i = 0; i < len - 1; i++) {
    o = cdrref(o);
  }
  o = carref(o);
  if (o.type == EMPTY) {
    return true;
  }
  if (o.type == PAIR) {
    Object o1 = o;
    Object o2 = cdrref(o);
    while (o2.type != EMPTY) {
      if (o2.type != PAIR) {
        return false;
      }
      if (o1.index == o2.index) {
        return false;
      }
      o1 = cdrref(o1);
      o2 = cdrref(o2);
      if (o2.type == EMPTY) {
        return true;
      }
      if (o2.type != PAIR) {
        return false;
      }
      o2 = cdrref(o2);
    }
    return true;
  }
  return false;
}

extern FILE *yyin;
extern FILE *yyout;
int main() {
  stack = empty;
  cars = memory_from[0];
  cdrs = memory_from[1];
  Object expr, env, val, cont, proc, argl, unev;
  expr = env = val = cont = proc = argl = unev = (Object){.type = NONE};
  quote_sym = identifier_new("quote");
  Object lambda_sym = identifier_new("lambda");
  Object if_sym = identifier_new("if");
  Object set_sym = identifier_new("set!");
  Object define_sym = identifier_new("define");
  Object begin_sym = identifier_new("begin");
  env = cons(cons(empty, empty), empty);
  define_variable(quote_sym, (Object){.type = QUOTE}, env);
  define_variable(lambda_sym, (Object){.type = LAMBDA}, env);
  define_variable(if_sym, (Object){.type = IF}, env);
  define_variable(set_sym, (Object){.type = SET}, env);
  define_variable(define_sym, (Object){.type = DEFINE}, env);
  define_variable(begin_sym, (Object){.type = BEGIN_TYPE}, env);

  /* primitive-procedures */
  /* Equivalence predicates  */
  define_variable(identifier_new("eqv?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_eqv_p},
                  env);
  define_variable(identifier_new("eq?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_eq_p}, env);
  /* Numbers */
  define_variable(identifier_new("number?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_number_p},
                  env);
  define_variable(identifier_new("complex?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_complex_p},
                  env);
  define_variable(identifier_new("rational?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_rational_p},
                  env);
  define_variable(identifier_new("exact?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_exact_p},
                  env);
  define_variable(identifier_new("inexact?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_inexact_p},
                  env);
  define_variable(identifier_new("finite?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_finite_p},
                  env);
  define_variable(identifier_new("infinite?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_infinite_p},
                  env);
  define_variable(identifier_new("nan?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_nan_p},
                  env);
  define_variable(identifier_new("zero?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_zero_p},
                  env);
  define_variable(identifier_new("positive?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_positive_p},
                  env);
  define_variable(identifier_new("negative?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_negative_p},
                  env);
  define_variable(identifier_new("+"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_add}, env);
  define_variable(identifier_new("*"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_mul}, env);
  define_variable(identifier_new("-"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_sub}, env);
  define_variable(identifier_new("/"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_div}, env);
  define_variable(identifier_new("numerator"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_numerator},
                  env);
  define_variable(
      identifier_new("denominator"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_denominator}, env);
  define_variable(identifier_new("floor"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_floor},
                  env);
  define_variable(identifier_new("ceiling"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_ceiling},
                  env);
  define_variable(identifier_new("truncate"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_truncate},
                  env);
  define_variable(identifier_new("sqrt"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_sqrt}, env);
  /* Numbers end */

  /* Booleans */

  /* Booleans end */
  /* Pairs and lists */
  define_variable(identifier_new("pair?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_pair_p},
                  env);
  define_variable(identifier_new("cons"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_cons}, env);
  define_variable(identifier_new("car"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_car}, env);
  define_variable(identifier_new("cdr"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_cdr}, env);
  define_variable(identifier_new("set-car!"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_set_car},
                  env);
  define_variable(identifier_new("set-cdr!"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_set_cdr},
                  env);
  /* Pairs and lists end */
  /* Symbols */
  define_variable(identifier_new("symbol?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_symbol_p},
                  env);
  /* Symbols end */
  /* Characters */
  define_variable(identifier_new("char?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_p},
                  env);
  define_variable(
      identifier_new("char-alphabetic?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_alphabetic_p},
      env);
  define_variable(
      identifier_new("char-numeric?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_numeric_p}, env);
  define_variable(
      identifier_new("char-whitespace?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_whitespace_p},
      env);
  define_variable(
      identifier_new("char-upper-case?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_upper_case_p},
      env);
  define_variable(
      identifier_new("char-lower-case?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_lower_case_p},
      env);
  define_variable(
      identifier_new("digit-value"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_digit_value}, env);
  define_variable(
      identifier_new("char->integer"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_tointeger}, env);
  define_variable(
      identifier_new("integer->char"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_integer_tochar}, env);
  define_variable(
      identifier_new("char-upcase"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_upcase}, env);
  define_variable(
      identifier_new("char-downcase"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_downcase}, env);
  define_variable(
      identifier_new("char-foldcase"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_char_foldcase}, env);
  /* Characters end */
  /* Strings */
  define_variable(identifier_new("string?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_p},
                  env);
  define_variable(
      identifier_new("make-string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_make_string}, env);
  define_variable(identifier_new("string"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string},
                  env);
  define_variable(
      identifier_new("string-length"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_length}, env);
  define_variable(identifier_new("string-ref"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_ref},
                  env);
  define_variable(identifier_new("string-set!"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_set},
                  env);
  /* Strings end */
  /* Vectors */
  define_variable(identifier_new("vector?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_vector_p},
                  env);
  define_variable(identifier_new("vector"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_vector},
                  env);
  define_variable(
      identifier_new("vector-length"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_vector_length}, env);
  define_variable(identifier_new("vector-ref"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_vector_ref},
                  env);
  define_variable(identifier_new("vector-set!"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_vector_set},
                  env);
  /* Vectors end */
  /* Bytevectors */
  define_variable(
      identifier_new("bytevector?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_bytevector_p}, env);
  define_variable(identifier_new("bytevector"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_bytevector},
                  env);
  define_variable(
      identifier_new("bytevector-length"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_bytevector_length},
      env);
  define_variable(
      identifier_new("bytevector-u8-ref"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_bytevector_ueight_ref},
      env);
  define_variable(
      identifier_new("bytevector-u8-set!"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_bytevector_ueight_set},
      env);
  define_variable(
      identifier_new("utf8->string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_utfeight_string}, env);
  define_variable(
      identifier_new("string->utf8"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_utfeight}, env);
  /* Bytevectors end */
  /* Control features */
  define_variable(
      identifier_new("procedure?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_procedure_p}, env);
  define_variable(identifier_new("apply"),
                  (Object){.type = PRIMITIVE_PROCEDURE_APPLY}, env);
  define_variable(identifier_new("call-with-current-continuation"),
                  (Object){.type = PRIMITIVE_PROCEDURE_CALL_WITH_CC}, env);
  define_variable(identifier_new("call/cc"),
                  (Object){.type = PRIMITIVE_PROCEDURE_CALL_WITH_CC}, env);
  /* Control features end */
  /* Exceptions */
  define_variable(identifier_new("raise"),
                  (Object){.type = PRIMITIVE_PROCEDURE_RAISE}, env);
  define_variable(identifier_new("raise-continuable"),
                  (Object){.type = PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE}, env);
  define_variable(identifier_new("error"),
                  (Object){.type = PRIMITIVE_PROCEDURE_ERROR}, env);
  define_variable(identifier_new("error-implementation-defined-object"),
                  (Object){.type = PRIMITIVE_PROCEDURE,
                           .proc = scm_error_implementation_defined_object},
                  env);
  define_variable(
      identifier_new("error-object?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_error_object_p}, env);
  define_variable(
      identifier_new("error-object-message"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_error_object_message},
      env);
  define_variable(
      identifier_new("error-object-irritants"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_error_object_irritants},
      env);

  /* Exceptions end */
  /* Input and output */
  define_variable(
      identifier_new("input-port?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_input_port_p}, env);
  define_variable(
      identifier_new("output-port?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_output_port_p}, env);
  define_variable(
      identifier_new("textual-port?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_textual_port_p}, env);
  define_variable(
      identifier_new("binary-port?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_binary_port_p}, env);
  define_variable(
      identifier_new("input-port-open?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_input_port_open_p},
      env);
  define_variable(
      identifier_new("output-port-open?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_output_port_open_p},
      env);
  define_variable(
      identifier_new("open-input-file"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_input_file}, env);
  define_variable(
      identifier_new("open-binary-input-file"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_binary_input_file},
      env);
  define_variable(
      identifier_new("open-output-file"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_output_file}, env);
  define_variable(identifier_new("open-binary-output-file"),
                  (Object){.type = PRIMITIVE_PROCEDURE,
                           .proc = scm_open_binary_output_file},
                  env);
  define_variable(identifier_new("close-port"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_close_port},
                  env);
  define_variable(identifier_new("read"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_read}, env);
  define_variable(identifier_new("read-char"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_read_char},
                  env);
  define_variable(
      identifier_new("eof-object?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_eof_object_p}, env);
  define_variable(identifier_new("eof-object"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_eof_object},
                  env);
  define_variable(identifier_new("write"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_write},
                  env);
  define_variable(
      identifier_new("write-shared"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_write_shared}, env);
  define_variable(
      identifier_new("write-simple"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_write_simple}, env);
  define_variable(identifier_new("display"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_display},
                  env);
  /* Input and output end */
  /* System interface */
  define_variable(
      identifier_new("emergency-exit"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_emergency_exit}, env);
  /* System interface end */
  Object global = env;
  /* yyin = stdin; */
  yyrestart(stdin);
  yyout = stdout;
  char *prompt = "ksi>";
read_eval_print_loop:
  stack = empty;
  fprintf(yyout, "%s ", prompt);
  ungetc(' ', yyin);
  expr = kread();
  env = global;
  cont.cont = &&print_result;
  goto eval_dispatch;
eval_dispatch:
  switch (expr.type) {
  case TRUE_TYPE:
  case FALSE_TYPE:
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case VECTOR:
  case BYTEVECTOR:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case PRIMITIVE_PROCEDURE:
  case PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
  case PRIMITIVE_PROCEDURE_ERROR:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case EOF_OBJ:
  case UNSPECIFIED:
    goto ev_self_eval;
  case IDENTIFIER:
    goto ev_variable;
  case PAIR: {
    Object t = carref(expr);
    switch (t.type) {
    case IDENTIFIER: {
      Object t1 = lookup_variable_valueref(t, env);
      switch (t1.type) {
      case QUOTE:
        goto ev_quoted;
      case LAMBDA:
        goto ev_lambda;
      case IF:
        goto ev_if;
      case SET:
        goto ev_assignment;
      case DEFINE:
        goto ev_definition;
      case BEGIN_TYPE:
        goto ev_begin;
      case PRIMITIVE_PROCEDURE:
      case PROCEDURE:
      case PRIMITIVE_PROCEDURE_APPLY:
      case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
      case CONTINUATION:
      case PRIMITIVE_PROCEDURE_RAISE:
      case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
      case PRIMITIVE_PROCEDURE_ERROR: {
        goto ev_application;
      }
      case EMPTY:
      case PAIR:
      case IDENTIFIER:
      case TRUE_TYPE:
      case FALSE_TYPE:
      case NUMBERZ:
      case NUMBERQ:
      case NUMBERR:
      case NUMBERC:

      case CHARACTER:
      case STRING_EMPTY:
      case STRING:
      case VECTOR:
      case BYTEVECTOR:
      case EOF_OBJ:
      case PORT_INPUT_TEXT:
      case PORT_INPUT_BINARY:
      case PORT_OUTPUT_TEXT:
      case PORT_OUTPUT_BINARY:
      case IMPLEMENTATION_DEFINED_OBJECT:
      case UNSPECIFIED:

      case NONE:
        goto unknown_expression_type;
      }
    }
    case PRIMITIVE_PROCEDURE:
    case PROCEDURE:
    case PRIMITIVE_PROCEDURE_APPLY:
    case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
    case CONTINUATION:
    case PRIMITIVE_PROCEDURE_RAISE:
    case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    case PRIMITIVE_PROCEDURE_ERROR:
    case PAIR: {
      goto ev_application;
    }
    case EMPTY:
    case TRUE_TYPE:
    case FALSE_TYPE:
    case NUMBERZ:
    case NUMBERQ:
    case NUMBERR:
    case NUMBERC:
    case CHARACTER:
    case STRING_EMPTY:
    case STRING:
    case VECTOR:
    case BYTEVECTOR:
    case IMPLEMENTATION_DEFINED_OBJECT:
    case EOF_OBJ:
    case PORT_INPUT_TEXT:
    case PORT_INPUT_BINARY:
    case PORT_OUTPUT_TEXT:
    case PORT_OUTPUT_BINARY:
    case QUOTE:
    case LAMBDA:
    case IF:
    case SET:
    case DEFINE:
    case BEGIN_TYPE:
    case NONE:
    case UNSPECIFIED:
      goto unknown_expression_type;
    }
  }
  case EMPTY:
    goto unknown_expression_type;

  case NONE:;
    fprintf(stderr, "ksi error eval_dispatch\n");
    exit(1);
  }
ev_self_eval:
  assign(&val, &expr);
  goto *cont.cont;
ev_variable:;
  object_free(&val);
  val = lookup_variable_value(expr, env);
  if (val.type == NONE) {
    goto unbound_variable;
  }
  goto *cont.cont;
ev_quoted:;
  object_free(&val);
  val = car(cdrref(expr));
  goto *cont.cont;
ev_lambda:
  object_free(&val);
  val = cons(env, cdrref(expr));
  val.type = PROCEDURE;
  goto *cont.cont;
ev_application:
  save(cont);
  save(env);
  unev = cdrref(expr);
  save(unev);
  expr = carref(expr);
  cont.cont = &&ev_appl_did_operator;
  goto eval_dispatch;
ev_appl_did_operator:
  restore(&unev);
  restore(&env);
  argl = empty;
  proc = val;
  if (unev.type == EMPTY)
    goto apply_dispatch;
  save(proc);
ev_appl_operand_loop:
  save(argl);
  expr = car(unev);
  if (cdrref(unev).type == EMPTY)
    goto ev_appl_last_arg;
  save(env);
  save(unev);
  cont.cont = &&ev_appl_accumulate_arg;
  goto eval_dispatch;
ev_appl_accumulate_arg:
  restore(&unev);
  restore(&env);
  restore(&argl);
  argl = cons(object_copy(val), argl);
  unev = cdrref(unev);
  goto ev_appl_operand_loop;
ev_appl_last_arg:
  cont.cont = &&ev_appl_accum_last_arg;
  goto eval_dispatch;
ev_appl_accum_last_arg:
  restore(&argl);
  argl = cons(object_copy(val), argl);
  argl = reverse(argl);
  restore(&proc);
  goto apply_dispatch;
ev_did_apply_proc:
  proc = val;
  restore(&cont);
  restore(&argl);
  goto apply_dispatch;
apply_dispatch:
  switch (proc.type) {
  case PRIMITIVE_PROCEDURE:
    goto primitive_apply;
  case PROCEDURE: {
    goto compound_apply;
  }
  case PRIMITIVE_PROCEDURE_APPLY: {
    expr = car(argl);
    argl = cdrref(argl);
    if (list_last_list_p(argl)) {
      argl = argl_append(argl);
    } else {
      for (Object o = argl; o.type != EMPTY; o = cdrref(o)) {
        if (!list_p(carref(o))) {
          fprintf(stderr, "apply error:(improper list)");
        }
      }
    }
    save(argl);
    save(cont);
    cont.cont = &&ev_did_apply_proc;
    goto eval_dispatch;
  }
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC: {
    restore(&cont);
    Object obj = reverse(stack);
    obj = reverse(obj);
    obj = continuation_cons(cont, obj);
    expr = cons(carref(argl), cons(obj, empty));
    goto eval_dispatch;
  }
  case CONTINUATION: {
    cont = continuation_carref(proc);
    stack = continuation_cdrref(proc);
    object_free(&val);
    val = car(argl);
    goto *cont.cont;
  }
  case PRIMITIVE_PROCEDURE_RAISE: {
    goto primitive_procedure_raise;
  }
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE: {
    goto primitive_procedure_raise_continuable;
  }
  case PRIMITIVE_PROCEDURE_ERROR: {
    goto primitive_procedure_error;
  }
  case EMPTY:
  case PAIR:
  case IDENTIFIER:
  case TRUE_TYPE:
  case FALSE_TYPE:
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case VECTOR:
  case BYTEVECTOR:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case EOF_OBJ:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case UNSPECIFIED:
  case NONE:
    goto unknown_procedure_type;
  }
primitive_apply:
  object_free(&val);
  val = proc.proc(argl);
  restore(&cont);
  goto *cont.cont;
compound_apply:
  env = carref(proc);
  unev = carref(cdrref(proc));
  env = extend_environment(unev, argl, env);
  unev = cdrref(cdrref(proc));
  goto ev_sequence;
ev_begin:
  unev = cdrref(expr);
  save(cont);
  goto ev_sequence;
ev_sequence:
  expr = car(unev);
  if (cdrref(unev).type == EMPTY)
    goto ev_sequence_last_exp;
  save(unev);
  save(env);
  cont.cont = &&ev_sequence_continue;
  goto eval_dispatch;
ev_sequence_continue:
  restore(&env);
  restore(&unev);
  unev = cdrref(unev);
  goto ev_sequence;
ev_sequence_last_exp:
  restore(&cont);
  goto eval_dispatch;
ev_if:
  save(expr);
  save(env);
  save(cont);
  cont.cont = &&ev_if_decide;
  expr = car(cdrref(expr));
  goto eval_dispatch;
ev_if_decide:
  restore(&cont);
  restore(&env);
  restore(&expr);
  if (val.type != FALSE_TYPE)
    goto ev_if_consequence;
  goto ev_if_alternative;
ev_if_alternative:
  expr = cdrref(cdrref(cdrref(expr))).type == EMPTY
             ? (Object){.type = UNSPECIFIED}
             : car(cdrref(cdrref(cdrref(expr))));
  goto eval_dispatch;
ev_if_consequence:
  expr = car(cdrref(cdrref(expr)));
  goto eval_dispatch;
ev_assignment:
  unev = carref(cdrref(expr));
  save(unev);
  expr = car(cdrref(cdrref(expr)));
  save(env);
  save(cont);
  cont.cont = &&ev_assignment_1;
  goto eval_dispatch;
ev_assignment_1:
  restore(&cont);
  restore(&env);
  restore(&unev);
  val = set_variable_value(unev, val, env);
  if (val.type == NONE) {

    goto unbound_variable;
  }
  goto *cont.cont;
ev_definition:
  if (carref(cdrref(expr)).type == PAIR) {
    unev = carref(carref(cdrref(expr)));
    save(unev);
    expr = cons(lambda_sym,
                cons(cdrref(carref(cdrref(expr))), cdrref(cdrref(expr))));
  } else {
    unev = carref(cdrref(expr));
    save(unev);
    expr = car(cdrref(cdrref(expr)));
  }
  save(env);
  save(cont);
  cont.cont = &&ev_definition_1;
  goto eval_dispatch;
ev_definition_1:
  restore(&cont);
  restore(&env);
  restore(&unev);
  val = define_variable(unev, val, env);
  goto *cont.cont;
print_result:
  printf("=> ");
  object_write(yyout, val);
  fprintf(yyout, "\n");
  goto read_eval_print_loop;

unknown_expression_type:

  goto signal_error;

unknown_procedure_type:
  restore(&cont);
  goto signal_error;
unbound_variable:

  goto signal_error;
primitive_procedure_raise:
  goto signal_error;
primitive_procedure_raise_continuable:
  goto signal_error;
primitive_procedure_error:

  goto signal_error;
signal_error:;
  goto read_eval_print_loop;
}