/** \file  */
#include "object.h"
#include "parse.tab.h"
#include "environment.h"
#include "procedures.h"

extern void yyrestart(FILE *);
Object kread() {
  yyparse();
  return kread_obj;
}
Object reverse(Object args) {
  Object out = empty;
  for (Object a = args; a.type != EMPTY; a = cdrref(a)) {
    save(a);
    out = cons(car(a), out);
    restore(&a);
  }
  return out;
}
Object argl_append(Object obj) {
  Object reversed = reverse(obj);
  Object out = car(reversed);
  for (Object t = cdrref(reversed); t.type != EMPTY; t = cdrref(t)) {
    save(t);
    out = cons(car(t), out);
    restore(&t);
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

extern int yylineno;
extern FILE *yyin;
extern FILE *yyout;

#include <string.h> // strerror
#include <errno.h>  // errno
int main() {
  if (gettimeofday(&procedures_tv, NULL)) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
  procedures_n = procedures_tv.tv_sec * 1000000 + procedures_tv.tv_usec;
  /* lex, procedures */
  mpz_inits(opz, opz1, NULL);
  mpq_inits(opq, opq1, NULL);
  mpf_init(opf);
  mpfr_inits(real, imag, opfr, opfr1, opfr2, opfr3, opfr4, argopfr, sopfr,
             copfr, NULL);
  mpc_init2(opc, MPC_PREC);
  mpc_init2(opc1, MPC_PREC);
  mpc_init2(opc2, MPC_PREC);
  mpc_init2(opc3, MPC_PREC);
  /* lex, procedures end */
  stack = empty;
  cars = memory_from[0];
  cdrs = memory_from[1];
  new_cars = memory_to[0];
  new_cdrs = memory_to[1];
  for (size_t i = 0; i < MEMORY_SIZE; i++) {
    cars[i] = none;
    cdrs[i] = none;
    new_cars[i] = none;
    new_cdrs[i] = none;
  }
  expr = env = val = cont = proc = argl = unev = (Object){.type = NONE};
  quote_sym = identifier_new("quote");
  Object lambda_sym = identifier_new("lambda");
  env = cons(cons(empty, empty), empty);
  define_variable(quote_sym, (Object){.type = QUOTE}, env);
  define_variable(identifier_new("lambda"), (Object){.type = LAMBDA}, env);
  define_variable(identifier_new("if"), (Object){.type = IF}, env);
  define_variable(identifier_new("set!"), (Object){.type = SET}, env);
  define_variable(identifier_new("define"), (Object){.type = DEFINE}, env);
  define_variable(identifier_new("begin"), (Object){.type = BEGIN_TYPE}, env);
  define_variable(identifier_new("and"), (Object){.type = AND}, env);
  define_variable(identifier_new("or"), (Object){.type = OR}, env);
  define_variable(identifier_new("delay"), (Object){.type = DELAY}, env);
  define_variable(identifier_new("delay-force"), (Object){.type = DELAY_FORCE},
                  env);
  /* primitive-procedures */

  /* Derived expression types */
  define_variable(identifier_new("make-promise"),
                  (Object){.type = PRIMITIVE_PROCEDURE_MAKE_PROMISE}, env);
  define_variable(identifier_new("force"),
                  (Object){.type = PRIMITIVE_PROCEDURE_FORCE}, env);
  define_variable(identifier_new("promise?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_promise_p},
                  env);
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
  define_variable(identifier_new("real?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_real_p},
                  env);
  define_variable(identifier_new("rational?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_rational_p},
                  env);
  define_variable(identifier_new("integer?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_integer_p},
                  env);
  define_variable(identifier_new("exact?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_exact_p},
                  env);
  define_variable(identifier_new("inexact?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_inexact_p},
                  env);
  define_variable(
      identifier_new("exact-integer?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_exact_integer_p}, env);
  define_variable(identifier_new("finite?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_finite_p},
                  env);
  define_variable(identifier_new("infinite?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_infinite_p},
                  env);
  define_variable(identifier_new("nan?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_nan_p},
                  env);
  define_variable(
      identifier_new("="),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_math_equal_p}, env);
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
  define_variable(identifier_new("abs"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_abs}, env);
  define_variable(identifier_new("gcd"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_gcd}, env);
  define_variable(identifier_new("lcm"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_lcm}, env);
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
  define_variable(identifier_new("round"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_round},
                  env);
  define_variable(identifier_new("exp"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_exp}, env);
  define_variable(identifier_new("log"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_log}, env);
  define_variable(identifier_new("sin"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_sin}, env);
  define_variable(identifier_new("cos"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_cos}, env);
  define_variable(identifier_new("tan"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_tan}, env);
  define_variable(identifier_new("asin"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_asin}, env);
  define_variable(identifier_new("acos"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_acos}, env);
  define_variable(identifier_new("atan"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_atan}, env);
  define_variable(identifier_new("square"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_square},
                  env);
  define_variable(identifier_new("sqrt"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_sqrt}, env);
  define_variable(
      identifier_new("exact-integer-sqrt"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_exact_integer_sqrt},
      env);
  define_variable(identifier_new("expt"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_expt}, env);
  define_variable(
      identifier_new("make-rectangular"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_make_rectangular}, env);
  define_variable(identifier_new("make-polar"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_make_polar},
                  env);
  define_variable(identifier_new("real-part"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_real_part},
                  env);
  define_variable(identifier_new("imag-part"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_imag_part},
                  env);
  define_variable(identifier_new("magnitude"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_magnitude},
                  env);
  define_variable(identifier_new("angle"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_angle},
                  env);
  define_variable(identifier_new("inexact"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_inexact},
                  env);
  define_variable(identifier_new("exact"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_exact},
                  env);
  define_variable(
      identifier_new("number->string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_number_tostring}, env);
  define_variable(
      identifier_new("string->number"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_tonumber}, env);
  /* Numbers end */

  /* Booleans */
  define_variable(identifier_new("not"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_not}, env);
  define_variable(identifier_new("boolean?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_boolean_p},
                  env);
  define_variable(
      identifier_new("boolean=?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_boolean_eq_p}, env);
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
  define_variable(identifier_new("null?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_null_p},
                  env);
  define_variable(identifier_new("list?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_list_p},
                  env);
  define_variable(identifier_new("make-list"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_make_list},
                  env);
  define_variable(identifier_new("list"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_list}, env);
  define_variable(identifier_new("length"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_length},
                  env);
  /* Pairs and lists end */
  /* Symbols */
  define_variable(identifier_new("symbol?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_symbol_p},
                  env);
  define_variable(
      identifier_new("symbol=?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_symbol_equal_p}, env);
  define_variable(
      identifier_new("symbol->string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_symbol_tostring}, env);
  define_variable(
      identifier_new("string->symbol"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_tosymbol}, env);
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
  define_variable(
      identifier_new("list->vector"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_list_tovector}, env);
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
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_utfeight_tostring},
      env);
  define_variable(
      identifier_new("string->utf8"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_string_toutfeight},
      env);
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
  /* define_variable(identifier_new("error"), */
  /* (Object){.type = PRIMITIVE_PROCEDURE_ERROR}, env); */
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
  define_variable(
      identifier_new("read-error?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_read_error_p}, env);
  define_variable(
      identifier_new("file-error?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_file_error_p}, env);
  /* Exceptions end */
  /* Environments and evaluation */
  define_variable(identifier_new("interaction-environment"),
                  (Object){.type = PRIMITIVE_PROCEDURE,
                           .proc = scm_interaction_environment},
                  env);
  define_variable(identifier_new("eval"),
                  (Object){.type = PRIMITIVE_PROCEDURE_EVAL}, env);
  /* Environments and evaluation end */
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
  define_variable(identifier_new("port?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_port_p},
                  env);
  define_variable(
      identifier_new("input-port-open?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_input_port_open_p},
      env);
  define_variable(
      identifier_new("output-port-open?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_output_port_open_p},
      env);
  define_variable(
      identifier_new("current-input-port"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_current_input_port},
      env);
  define_variable(
      identifier_new("current-output-port"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_current_output_port},
      env);
  define_variable(
      identifier_new("current-error-port"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_current_error_port},
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
  define_variable(
      identifier_new("open-input-string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_input_string},
      env);
  define_variable(
      identifier_new("open-output-string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_output_string},
      env);
  define_variable(
      identifier_new("get-output-string"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_get_output_string},
      env);
  define_variable(
      identifier_new("open-input-bytevector"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_input_bytevector},
      env);
  define_variable(
      identifier_new("open-output-bytevector"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_open_output_bytevector},
      env);
  define_variable(
      identifier_new("get-output-bytevector"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_get_output_bytevector},
      env);
  define_variable(identifier_new("read"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_read}, env);
  define_variable(identifier_new("read-char"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_read_char},
                  env);
  define_variable(identifier_new("peek-char"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_peek_char},
                  env);
  define_variable(
      identifier_new("eof-object?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_eof_object_p}, env);
  define_variable(identifier_new("eof-object"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_eof_object},
                  env);
  define_variable(identifier_new("read-u8"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_read_u8},
                  env);
  define_variable(identifier_new("peek-u8"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_peek_u8},
                  env);
  define_variable(identifier_new("u8-ready?"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_u8_ready_p},
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
  define_variable(identifier_new("newline"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_newline},
                  env);
  define_variable(identifier_new("write-char"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_write_char},
                  env);
  define_variable(identifier_new("write-u8"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_write_u8},
                  env);
  define_variable(
      identifier_new("flush-output-port"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_flush_output_port},
      env);
  /* Input and output end */
  /* System interface */
  define_variable(
      identifier_new("file-exists?"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_file_exists_p}, env);
  define_variable(
      identifier_new("primitive-delete-file"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_primitive_delete_file},
      env);
  define_variable(
      identifier_new("emergency-exit"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_emergency_exit}, env);
  define_variable(identifier_new("get-environment-variable"),
                  (Object){.type = PRIMITIVE_PROCEDURE,
                           .proc = scm_get_environment_variable},
                  env);
  define_variable(identifier_new("get-environment-variables"),
                  (Object){.type = PRIMITIVE_PROCEDURE,
                           .proc = scm_get_environment_variables},
                  env);
  define_variable(
      identifier_new("current-second"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_current_second}, env);
  define_variable(
      identifier_new("current-jiffy"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_current_jiffy}, env);
  define_variable(
      identifier_new("jiffies-per-second"),
      (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_jiffies_per_second},
      env);
  /* System interface end */

  /* others */
  gmp_randinit_default(scm_random_state);
  define_variable(identifier_new("random"),
                  (Object){.type = PRIMITIVE_PROCEDURE, .proc = scm_random},
                  env);
  /* others end */
  global = env;
  /* yyin = stdin; */
  yyrestart(stdin);
  yyout = stdout;
  cur_in = cons((Object){.port = yyin}, empty);
  cur_in.type = PORT_INPUT_TEXT;
  cur_out = cons((Object){.port = yyout}, empty);
  cur_out.type = PORT_OUTPUT_TEXT;
  cur_err = cons((Object){.port = stderr}, empty);
  cur_err.type = PORT_OUTPUT_TEXT;
  char *prompt = "ksi>";
  Object tmp;

  /* preload: */
  FILE *fh = fopen("procedures.scm", "r");
  if (fh == NULL) {
    fprintf(carref(cur_err).port, "%s\n", strerror(errno));
    goto read_eval_print_loop;
  }
  yyrestart(fh);
  yylineno = 0;
  stack = empty;
  expr = kread();
  env = global;
  cont.cont = &&read_eval_print_loop;
  save(cont);
  cont.cont = &&preloaded;
  goto eval_dispatch;
preloaded:
  fclose(fh);
  restore(&cont);
  yyrestart(carref(cur_in).port);
  goto *cont.cont;
read_eval_print_loop:
  yylineno = 0;
  stack = empty;
  fprintf(carref(cur_out).port, "%s ", prompt);
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
  case STRING_IMMUTABLE:
  case STRING_IMMUTABLE_VERTICAL:
  case VECTOR:
  case BYTEVECTOR:
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
  case PROMISE:
  case PRIMITIVE_PROCEDURE:
  case PROCEDURE:
  case PRIMITIVE_PROCEDURE_MAKE_PROMISE:
  case PRIMITIVE_PROCEDURE_FORCE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_EVAL:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case PORT_INPUT_TEXT_STRING:
  case PORT_OUTPUT_TEXT_STRING:
  case PORT_INPUT_BINARY_BYTEVECTOR:
  case PORT_OUTPUT_BINARY_BYTEVECTOR:
  case EOF_OBJ:
  case READ_ERROR:
  case FILE_ERROR:
  case UNSPECIFIED:
    goto ev_self_eval;
  case IDENTIFIER:
  case IDENTIFIER_VERTICAL:
    goto ev_variable;
  case PAIR: {
    Object t = carref(expr);
    switch (t.type) {
    case IDENTIFIER:
    case IDENTIFIER_VERTICAL: {
      Object t1 = lookup_variable_valueref(t, env);
      switch (t1.type) {
      case NONE: {
        expr = t;
        goto unbound_variable;
      }
      case QUOTE: {
        if (!list_p(expr) || list_length(expr) != 2) {
          goto syntax_error;
        }
        goto ev_quoted;
      }
      case LAMBDA: {
        if (!list_p(expr) || list_length(expr) < 3) {
          goto syntax_error;
        }
        goto ev_lambda;
      }
      case IF: {
        if (!list_p(expr) ||
            (list_length(expr) != 3 && list_length(expr) != 4)) {
          goto syntax_error;
        }
        goto ev_if;
      }
      case SET: {
        if (!list_p(expr) || list_length(expr) != 3) {
          goto syntax_error;
        }
        goto ev_assignment;
      }
      case DEFINE: {
        if (!list_p(expr) || list_length(expr) < 3) {
          goto syntax_error;
        }
        goto ev_definition;
      }
      case BEGIN_TYPE: {
        if (!list_p(expr) || list_length(expr) == 1) {
          goto syntax_error;
        }
        goto ev_begin;
      }
      case AND: {
        if (!list_p(expr)) {
          goto syntax_error;
        }
        goto ev_and;
      }
      case OR: {
        if (!list_p(expr)) {
          goto syntax_error;
        }
        goto ev_or;
      }
      case DELAY: {
        if (!list_p(expr) || list_length(expr) != 2) {
          goto syntax_error;
        }
        goto ev_delay;
      }
      case DELAY_FORCE: {
        if (!list_p(expr) || list_length(expr) != 2) {
          goto syntax_error;
        }
        goto ev_delay_force;
      }
      case PRIMITIVE_PROCEDURE:
      case PROCEDURE:
      case PRIMITIVE_PROCEDURE_MAKE_PROMISE:
      case PRIMITIVE_PROCEDURE_FORCE:
      case PRIMITIVE_PROCEDURE_APPLY:
      case PRIMITIVE_PROCEDURE_EVAL:
      case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
      case CONTINUATION:
      case PRIMITIVE_PROCEDURE_RAISE:
      case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE: {
        if (!list_p(expr)) {
          goto syntax_error;
        }
        goto ev_application;
      }
      default:
        goto unknown_expression_type;
      }
    }
    case PRIMITIVE_PROCEDURE:
    case PROCEDURE:
    case PRIMITIVE_PROCEDURE_MAKE_PROMISE:
    case PRIMITIVE_PROCEDURE_FORCE:
    case PRIMITIVE_PROCEDURE_APPLY:
    case PRIMITIVE_PROCEDURE_EVAL:
    case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
    case CONTINUATION:
    case PRIMITIVE_PROCEDURE_RAISE:
    case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    case PAIR: {
      if (!list_p(expr)) {
        goto syntax_error;
      }
      goto ev_application;
    }
    default:
      goto unknown_expression_type;
    }
  }
  case EMPTY:
    goto unknown_expression_type;
  case NONE:;
    fprintf(stderr, "ksi error eval_dispatch\n");
    exit(1);
  default:
    exit(1);
  }
ev_self_eval:
  object_free(&val);
  val = expr;
  expr.type = NONE;
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
  unev = cdrref(expr);
  expr = carref(expr);
  if (expr.type == IDENTIFIER || expr.type == IDENTIFIER_VERTICAL) {
    cont.cont = &&ev_appl_did_operator_1;
    goto ev_variable;
  }
  save(env);
  save(unev);
  cont.cont = &&ev_appl_did_operator;
  goto eval_dispatch;
ev_appl_did_operator:
  restore(&unev);
  restore(&env);
ev_appl_did_operator_1:
  argl = empty;
  proc = val;
  if (unev.type == EMPTY) {
    goto apply_dispatch;
  }
  save(proc);
ev_appl_operand_loop:
  save(argl);
  expr = car(unev);
  if (cdrref(unev).type == EMPTY) {
    goto ev_appl_last_arg;
  }
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
  case PRIMITIVE_PROCEDURE_MAKE_PROMISE: {
    if (list_length(argl) != 1) {
      object_free(&val);
      val = arguments(argl, "make-promise");
      goto wrong_number_of_arguments;
    }
    goto ev_make_promise;
  }
  case PRIMITIVE_PROCEDURE_FORCE: {
    if (list_length(argl) != 1) {
      object_free(&val);
      val = arguments(argl, "force");
      goto wrong_number_of_arguments;
    }
    goto ev_force;
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
  case PRIMITIVE_PROCEDURE_EVAL: {
    if (list_length(argl) != 2) {
      fprintf(carref(cur_err).port,
              "Error: (eval) wrong numberc of arguments -- ");
      object_write(carref(cur_err).port, argl);
      goto wrong_number_of_arguments;
    }
    goto ev_primitive_procedure_eval;
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
    if (argl.type == EMPTY) {
      val.type = MULTIPLE_ZERO;
    } else if (cdrref(argl).type != EMPTY) {
      val = argl;
      val.type = MULTIPLE;
    } else {
      val = car(argl);
    }
    goto *cont.cont;
  }
  case PRIMITIVE_PROCEDURE_RAISE: {
    if (list_length(argl) != 1) {
      object_free(&val);
      val = arguments(argl, "raise");
      goto wrong_number_of_arguments;
    }
    goto primitive_procedure_raise;
  }
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE: {
    goto primitive_procedure_raise_continuable;
  }
  default:
    goto unknown_procedure_type;
  }
primitive_apply:
  object_free(&val);
  val = proc.proc(argl);
  switch (val.type) {
  case WRONG_NUMBER_OF_ARGUMENTS:
    goto wrong_number_of_arguments;
  case WRONG_TYPE_ARGUMENT:
    goto wrong_type_argument;
  case EXPT_ERROR:
    goto expt_error;
  case EXACT_ERROR:
    goto exact_error;
  case READ_ERROR:
    goto read_error;
  case FILE_ERROR:
    goto file_error;
  case ERROR:
    goto signal_error;
  default:
    break;
  }
  restore(&cont);
  goto *cont.cont;
compound_apply:
  env = carref(proc);
  unev = carref(cdrref(proc));
  env = extend_environment();
  if (env.type == WRONG_NUMBER_OF_ARGUMENTS) {
    fprintf(carref(cur_err).port, "Error: ");
    object_write(carref(cur_err).port, proc);
    fprintf(carref(cur_err).port, " wrong number of arguments -- ");
    object_write(carref(cur_err).port, argl);
    fprintf(carref(cur_err).port, "\n");
    goto wrong_number_of_arguments;
  }
  unev = cdrref(cdrref(proc));
  goto ev_sequence;
ev_force:
  unev = carref(argl);
  if (unev.type == PROMISE) {
    if (carref(unev).type == TRUE_TYPE) {
      object_free(&val);
      val = car(cdrref(unev));
      restore(&cont);
      goto *cont.cont;
    } else {
      expr = car(cdrref(unev));
      save(unev);
      save(env);
      env = cdrref(cdrref(unev));
      cont.cont = &&ev_forced;
      goto eval_dispatch;
    }
  } else {
    object_free(&val);
    val = object_copy(unev);
    restore(&cont);
    goto *cont.cont;
  }
ev_forced:
  restore(&env);
  restore(&unev);
  object_free(&cars[unev.index]);
  cars[unev.index] = true_obj;
  cars[cdrs[unev.index].index] = object_copy(val);
  restore(&cont);
  goto *cont.cont;
ev_make_promise:
  object_free(&val);
  val = cons(car(argl), env);
  val = cons(false_obj, val);
  val.type = PROMISE;
  restore(&cont);
  goto *cont.cont;
ev_primitive_procedure_eval:
  save(env);
  expr = car(argl);
  env = car(cdrref(argl));
  cont.cont = &&ev_primitive_procedure_eval_1;
  goto eval_dispatch;
ev_primitive_procedure_eval_1:
  restore(&env);
  restore(&cont);
  goto *cont.cont;
ev_begin:
  unev = cdrref(expr);
  save(cont);
  goto ev_sequence;
ev_sequence:
  expr = car(unev);
  if (cdrref(unev).type == EMPTY) {
    goto ev_sequence_last_exp;
  }
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
  if (val.type != FALSE_TYPE) {
    goto ev_if_consequence;
  }
  goto ev_if_alternative;
ev_if_alternative:
  tmp = cdrref(cdrref(cdrref(expr)));
  expr = tmp.type == EMPTY ? (Object){.type = UNSPECIFIED} : car(tmp);
  goto eval_dispatch;
ev_if_consequence:
  expr = car(cdrref(cdrref(expr)));
  goto eval_dispatch;
ev_assignment:
  tmp = cdrref(expr);
  unev = carref(tmp);
  save(unev);
  expr = car(cdrref(tmp));
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
    expr = unev;
    goto unbound_variable;
  }
  goto *cont.cont;
ev_definition:
  tmp = cdrref(expr);
  if (carref(tmp).type == PAIR) {
    if (list_length(expr) < 3) {
      goto syntax_error;
    }
    unev = carref(carref(tmp));
    save(unev);
    expr = cons(lambda_sym, cons(cdrref(carref(tmp)), cdrref(tmp)));
  } else {
    if (list_length(expr) != 3) {
      goto syntax_error;
    }
    unev = carref(tmp);
    save(unev);
    expr = car(cdrref(tmp));
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
ev_and:
  unev = cdrref(expr);
  if (unev.type == EMPTY) {
    object_free(&val);
    val = true_obj;
    goto *cont.cont;
  }
  save(cont);
  goto ev_and_loop;
ev_and_loop:
  expr = carref(unev);
  if (cdrref(unev).type == EMPTY) {
    goto ev_and_loop_last_exp;
  }
  save(unev);
  save(env);
  cont.cont = &&ev_and_loop_decided;
  goto eval_dispatch;
ev_and_loop_decided:
  if (val.type == FALSE_TYPE) {
    restore(&env);
    restore(&unev);
    restore(&cont);
    goto *cont.cont;
  }
  goto ev_and_loop_continue;
ev_and_loop_continue:
  restore(&env);
  restore(&unev);
  unev = cdrref(unev);
  goto ev_and_loop;
ev_and_loop_last_exp:
  restore(&cont);
  goto eval_dispatch;
ev_or:
  unev = cdrref(expr);
  if (unev.type == EMPTY) {
    object_free(&val);
    val = false_obj;
    goto *cont.cont;
  }
  save(cont);
  goto ev_or_loop;
ev_or_loop:
  expr = carref(unev);
  if (cdrref(unev).type == EMPTY) {
    goto ev_or_loop_last_exp;
  }
  save(unev);
  save(env);
  cont.cont = &&ev_or_loop_decided;
  goto eval_dispatch;
ev_or_loop_decided:
  if (val.type != FALSE_TYPE) {
    restore(&env);
    restore(&unev);
    restore(&cont);
    goto *cont.cont;
  }
  goto ev_or_loop_continue;
ev_or_loop_continue:
  restore(&env);
  restore(&unev);
  unev = cdrref(unev);
  goto ev_or_loop;
ev_or_loop_last_exp:
  restore(&cont);
  goto eval_dispatch;
ev_delay:
  object_free(&val);
  val = cons(car(cdrref(expr)), env);
  val = cons(false_obj, val);
  val.type = PROMISE;
  goto *cont.cont;
ev_delay_force:
  expr = cons(carref(cdrref(expr)), empty);
  expr = cons((Object){.type = PRIMITIVE_PROCEDURE_FORCE}, expr);
  val = cons(expr, env);
  val = cons(false_obj, val);
  val.type = PROMISE;
  goto *cont.cont;
print_result:
  if (interactive_mode) {
    printf("=> ");
    object_write(carref(cur_out).port, val);
    fprintf(carref(cur_out).port, "\n");
  }
  goto read_eval_print_loop;

unknown_expression_type:
  fprintf(carref(cur_err).port, "Error: unknown expression type -- ");
  object_write(carref(cur_err).port, expr);
  fprintf(carref(cur_err).port, "\n");
  goto signal_error;
unknown_procedure_type:
  restore(&cont);
  fprintf(carref(cur_err).port, "Error: unknown procedure type -- ");
  object_write(carref(cur_err).port, proc);
  fprintf(carref(cur_err).port, "\n");
  goto signal_error;
unbound_variable:
  fprintf(carref(cur_err).port, "Error: unbound variable -- ");
  object_write(carref(cur_err).port, expr);
  fprintf(carref(cur_err).port, "\n");
  goto signal_error;
primitive_procedure_raise:
  fprintf(carref(cur_err).port, "Error: ");
  object_display(carref(cur_err).port, carref(argl));
  fprintf(carref(cur_err).port, "\n");
  goto signal_error;
primitive_procedure_raise_continuable:
  goto signal_error;
wrong_number_of_arguments:
  goto signal_error;
wrong_type_argument:
  goto signal_error;
syntax_error:
  fprintf(carref(cur_err).port, "Error: syntax error - ");
  object_write(carref(cur_err).port, expr);
  fprintf(carref(cur_err).port, "\n");
  goto signal_error;
expt_error:
  goto signal_error;
exact_error:
  goto signal_error;
read_error:
  fprintf(carref(cur_err).port, "Error: %s\n", val.message);
  goto signal_error;
file_error:
  fprintf(carref(cur_err).port, "Error: %s\n", val.message);
  goto signal_error;
signal_error:;
  goto read_eval_print_loop;
}
